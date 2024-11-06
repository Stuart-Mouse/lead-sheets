
/*
    Eventaully, we will want to have typechecking be done in a second pass after the intial construction of the AST.
    
    
    a benefit of having the typechecking in a second step is that we will be able to do type inference on struct literals
    
*/

Typechecking_Flags :: enum_flags {
    EXPECT_LVALUE; // this should not get passed down syntax tree except through indexing operations
}

typecheck_node :: (using ctxt: *Script_Constructor, node_i: int, check_flags: Typechecking_Flags) -> *Type_Info {
    node := ctxt.nodes[node_i];
    
    if #complete node.type == {
      case .NUMBER;
        if check_flags & .EXPECT_LVALUE  return null, false;
        return type_info(float32), true;
        
      case .VARIABLE;
        // variable can be used as lvalue
        var_idx := resolve_variable(ctxt, node.src_text);
        if var_idx == -1 {
            dbg_print("Unable to resolve variable '%'!\n", ident);
            return -1;
        }
        node.variable_index = var_idx;
        
        variable := variables[node.variable_index];
        return variable.type, true;
        
      case .OPERATION;
        if check_flags & .EXPECT_LVALUE  return null, false;
        
        left_ti  := typecheck_node(ctxt, ast.nodes[left_i ], check_flags);
        right_ti := typecheck_node(ctxt, ast.nodes[right_i], check_flags);
        
        assert(left_ti  != null);
        assert(right_ti != null);
        
        swap_args, is_builtin := false;
        oper_idx:, swap_args = resolve_builtin_operation(node.src_text, left_ti, right_ti);
        if oper_idx >= 0 {
            is_builtin = true;
            dbg_print("operator is builtin, ");
        } else {
            oper_idx, swap_args = resolve_procedure_index(ctxt, node.src_text, .[ left_ti, right_ti ], true);
            if oper_idx == -1  return -1;
        }
        dbg_print("resolved to operator index %\n", oper_idx);
            
        if swap_args   node.operation.flags |= .SWAP_ARGS; 
        if is_builtin  node.operation.flags |= .IS_BUILTIN;  
        
        return node_get_value_type(ctxt, node);
        
      case .PROCEDURE_CALL;
        if check_flags & .EXPECT_LVALUE  return null, false;
        
        argument_types := NewArray(node.procedure_call.argument_nodes.count, *Type_Info,, temp);
        for node.procedure_call.argument_nodes {
            argument_type := typecheck_node(ctxt, ast.nodes[it]);
            assert(argument_type != null);
            argument_types[it_index] = argument_type;
        }
        
        node.procedure_call.procedure_index = resolve_procedure_index(ctxt, ident, argument_types);
        if node.procedure_call.procedure_index == -1  return null;
        
        return node_get_value_type(ctxt, node);
    }
    
    return null;
}

typecheck_statement :: (using ctxt: *Script_Constructor, statement: *AST.Statement, check_flags: Typechecking_Flags) -> *Type_Info {
    if #complete statment.type == {
      case .ASSIGNMENT;
        right_ti := typecheck_node();
        if !right_ti  return null;
        
        // get type of left side
    }
    return null;
}