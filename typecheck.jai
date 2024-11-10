
/*
    Eventaully, we will want to have typechecking be done in a second pass after the intial construction of the AST.
    
    
    a benefit of having the typechecking in a second step is that we will be able to do type inference on struct literals
    
*/

Typechecking_Flags :: enum_flags {
    NONE :: 0;
    EXPECT_LVALUE :: 1; // this should not get passed down syntax tree except through indexing operations
}

// TODO: after typechecking, we should probably remove references to source text, since the user may want to free the scripts's source after compiling the script
typecheck_node :: (
    using ctxt  : *Script, 
    node_i      :  int, 
    check_flags :  Typechecking_Flags = .NONE, 
    expect_type : *Type_Info          = null
) -> *Type_Info {
    node := *ctxt.ast.nodes[node_i];
    dbg_print("typechecking node %: % %\n", node_i, node.type, node.src_text);
    
    #if DEBUG {
        context.dbg_print_indent += 1;
        defer context.dbg_print_indent -= 1;
    }
    
    if #complete node.type == {
      case .NUMBER;
        if check_flags & .EXPECT_LVALUE  return null;
        return type_info(float32);
        
      case .VARIABLE;
        // variable can be used as lvalue
        var_idx := resolve_variable(ctxt, node.src_text);
        if var_idx == -1 {
            dbg_print("Unable to resolve variable '%'!\n", node.src_text);
            return null;
        }
        node.variable_index = var_idx;
        
        variable := variables[node.variable_index];
        return variable.binding.type;
        
      case .OPERATION;
        if check_flags & .EXPECT_LVALUE  return null;
        
        left_ti  := typecheck_node(ctxt, node.operation.left,  check_flags);
        if !left_ti   return null;
        
        right_ti := typecheck_node(ctxt, node.operation.right, check_flags);
        if !right_ti  return null;
        
        swap_args, is_builtin := false;
        node.operation.operator_index, swap_args = resolve_builtin_operation(node.src_text, left_ti, right_ti);
        if node.operation.operator_index >= 0 {
            is_builtin = true;
            dbg_print("operator is builtin, ");
        } else {
            node.operation.operator_index, swap_args = resolve_procedure_index(ctxt, node.src_text, .[ left_ti, right_ti ], true);
            if node.operation.operator_index == -1  return null;
        }
        dbg_print("resolved to operator index %\n", node.operation.operator_index);
        
        if swap_args   node.operation.flags |= .SWAP_ARGS;
        if is_builtin  node.operation.flags |= .IS_BUILTIN;
        
        dbg_print("opertion flags: %\n", node.operation.flags);
        
        return node_get_value_type(ctxt, node);
        
      case .PROCEDURE_CALL;
        if check_flags & .EXPECT_LVALUE  return null;
        
        dbg_print("typechecking arguments...\n");
        dbg_print("arg_count: %\n", node.procedure_call.arg_count);
        argument_types := NewArray(node.procedure_call.arg_count, *Type_Info,, temp);
        
        arg_node_i := node.procedure_call.first_arg_node;
        arg_idx := 0;
        while arg_node_i != -1 {
            argument_type := typecheck_node(ctxt, arg_node_i);
            if !argument_type  return null;
            argument_types[arg_idx] = argument_type;
            arg_idx += 1;
            arg_node_i = ast.nodes[arg_node_i].next_arg_node;
        }
        
        node.procedure_call.procedure_index = resolve_procedure_index(ctxt, node.src_text, argument_types);
        if node.procedure_call.procedure_index == -1  return null;
        
        return node_get_value_type(ctxt, node);
    }
    
    return null;
}

typecheck_statement :: (using ctxt: *Script, statement: *AST.Statement, check_flags: Typechecking_Flags) -> bool {
    if #complete statement.type == {
      case .ASSIGNMENT;
        // get type of left side
        // we expect left side to be an lvalue
        left_ti := typecheck_node(ctxt, statement.assignment.left.root, check_flags = .EXPECT_LVALUE);
        if !left_ti  return false;
        
        // pass left type as 'expect_type' argument so that we can infer type of struct literals
        right_ti := typecheck_node(ctxt, statement.assignment.right.root, expect_type = left_ti);
        if !right_ti  return false;
        
        if left_ti != right_ti {
            dbg_print("types on left and right side of assignment statement do not match! % != %\n", type_info_to_type(left_ti), type_info_to_type(right_ti));
            return false;
        }
        
        return true;
    }
    return false;
}

typecheck_script :: (script: *Script) -> bool {
    for *script.ast.statements {
        if !typecheck_statement(script, it, .NONE)  return false;
    }
    
    script.flags |= .AST_TYPECHECKED;
    
    return true;
}

