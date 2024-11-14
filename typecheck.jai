
/*
        
    I thought that we may need to have 2 passes for typechecking
    second pass being necessary to put info on each node about stack position
    but that should actually not be necessary
    and, we actually should be able to calculate maxiumum require stack space from leaves to root, since we can just add up sizes of types and pass that info back up the call stack
    
    so TODO: return running stack size from typechecking routines
    also TODO: these procedures should take a script constructor rather than jsut a script, since we will probably remove the dynamic arrays from script itself
*/

Typechecking_Flags :: enum_flags {
    NONE :: 0;
    EXPECT_LVALUE :: 1; // this should not get passed down syntax tree except through indexing operations
}

// TODO: after typechecking, we should probably remove references to source text, since the user may want to free the scripts's source after compiling the script
typecheck_node :: (
    using ctxt  : *Script_Constructor, 
    node        : *AST.Node, 
    check_flags :  Typechecking_Flags = .NONE, 
    expect_type : *Type_Info          = null
) -> (
    node_type      : *Type_Info,
    stack_required : int
) {
    dbg_print("typechecking node %: % %\n", node, node.type, node.src_text);
    
    #if DEBUG {
        context.dbg_print_indent += 1;
        defer context.dbg_print_indent -= 1;
    }
    
    if #complete node.type == {
      case .NUMBER;
        if check_flags & .EXPECT_LVALUE  return null, 0;
        node.value_type = type_info(float32);
        return type_info(float32), size_of(float32);
        
      case .VARIABLE;
        node.variable_index = resolve_variable(ctxt, node.src_text);
        if node.variable_index == -1 {
            dbg_print("Unable to resolve variable '%'!\n", node.src_text);
            return null, 0;
        }
        
        node.value_type = variables[node.variable_index].binding.type;
        
        stack_required := get_stack_required(node.value_type);
        if check_flags & .EXPECT_LVALUE {
            stack_required = size_of(*void);
            node.flags |= .L_VALUE;
        }
        return node.value_type, stack_required;
        
      case .OPERATION;
        if check_flags & .EXPECT_LVALUE  return null, 0;
        
        left_type, left_stack_required := typecheck_node(ctxt, node.operation.left, check_flags);
        if !left_type   return null, 0;
        
        right_type, right_stack_required := typecheck_node(ctxt, node.operation.right, check_flags);
        if !right_type  return null, 0;
        
        swap_args, is_builtin := false;
        node.operation.operator_index, swap_args = resolve_builtin_operation(node.src_text, left_type, right_type);
        if node.operation.operator_index >= 0 {
            is_builtin = true;
            node.value_type = cast(*Type_Info) BUILTIN_OPERATIONS[node.operation.operator_index].ret;
            dbg_print("operator is builtin, ");
        } else {
            node.operation.operator_index, swap_args = resolve_procedure_index(ctxt, node.src_text, .[ left_type, right_type ], true);
            if node.operation.operator_index == -1  return null, 0;
            procedure := *operators[node.operation.operator_index];
            node.value_type = procedure.type.return_types[0];
        }
        dbg_print("resolved to operator index %\n", node.operation.operator_index);
        
        if swap_args {
            node.operation.flags |= .SWAP_ARGS;
            // just swap left and right sides of binary operator
            // potentially this could create inconsistency in evaluation order if calling procedure with side effects
            // but that's probably not a real concern...
            node.operation.left, node.operation.right = node.operation.right, node.operation.left;
        }
        if is_builtin  node.operation.flags |= .IS_BUILTIN;
        
        dbg_print("operation flags: %\n", node.operation.flags);
        
        if node.value_type.type == .STRUCT {
            node.operation.dst = Dynamic_New(node.value_type,, ctxt.allocator);
            dbg_print("allocating variable of type % at % for node %\n", as_type(node.value_type), node.operation.dst, node);
        }
        
        stack_required := get_stack_required(node.value_type) + left_stack_required + right_stack_required;
        return node.value_type, stack_required;
        
      case .PROCEDURE_CALL;
        if check_flags & .EXPECT_LVALUE  return null, 0;
        
        dbg_print("typechecking arguments...\n");
        dbg_print("arg_count: %\n", node.procedure_call.arg_count);
        
        stack_required := 0;
        argument_types := NewArray(node.procedure_call.arg_count, *Type_Info,, temp); // TODO
        arg_node := node.procedure_call.arg_nodes;
        arg_idx  := 0;
        while arg_node != null {
            argument_type := typecheck_node(ctxt, arg_node);
            if !argument_type  return null, 0;
            argument_types[arg_idx] = argument_type;
            stack_required += argument_type.runtime_size;
            
            arg_idx += 1;
            arg_node = arg_node.next;
        }
        
        node.procedure_call.procedure_index = resolve_procedure_index(ctxt, node.src_text, argument_types);
        if node.procedure_call.procedure_index == -1  return null, 0;
        
        procedure := *procedures[node.procedure_call.procedure_index];
        node.value_type = procedure.type.return_types[0];
        
        if node.value_type.type == .STRUCT {
            node.procedure_call.dst = Dynamic_New(node.value_type,, ctxt.allocator);
            dbg_print("allocating variable of type % at % for node %\n", as_type(node.value_type), node.procedure_call.dst, node);
        }
        
        stack_required += get_stack_required(node.value_type);
        return node.value_type, stack_required;
        
      case .SUBSCRIPT;
        dbg_print("ERROR: unimplemented case for .SUBSCRIPT\n");
        return null, 0;
        
      case .MEMBER;
        dbg_print("ERROR: unimplemented case for .SUBSCRIPT\n");
        return null, 0;
        
        // base_type := typecheck_node(ctxt, node.member.base);
        // if base_type.type != .STRUCT  return null, 0;
        
        // member_ti := get_field(xx base_type, node.member.identifier);
        // if member_ti == null  return null, 0;
        
        // node.value_type    = member_ti.type;
        // node.member.offset = member_ti.offset_in_bytes;
        
        // stack_required := get_stack_required(node.value_type);
        // if check_flags & .EXPECT_LVALUE {
        //     stack_required = size_of(*void);
        //     node.flags |= .L_VALUE;
        // }
        // return node.value_type, stack_required;
    }
    
    return null, 0;
}

typecheck_statement :: (
    using ctxt  : *Script_Constructor, 
    statement   : *AST.Statement, 
    check_flags : Typechecking_Flags
) -> (
    stack_required: int,
    ok: bool 
) {
    if #complete statement.type == {
      case .ASSIGNMENT;
        // get type of left side
        // we expect left side to be an lvalue
        left_type, left_stack_required := typecheck_node(ctxt, statement.assignment.left, check_flags = .EXPECT_LVALUE);
        if !left_type  return 0, false;
        
        // pass left type as 'expect_type' argument so that we can infer type of struct literals
        right_type, right_stack_required := typecheck_node(ctxt, statement.assignment.right, expect_type = left_type);
        if !right_type  return 0, false;
        
        if left_type != right_type {
            dbg_print("types on left and right side of assignment statement do not match! % != %\n", as_type(left_type), as_type(right_type));
            return 0, false;
        }
        
        return left_stack_required + right_stack_required, true;
    }
    return 0, false;
}

typecheck_script :: (script: *Script_Constructor) -> (stack_required: int, ok: bool) {
    max_stack_required: int;
    for *script.ast.statements {
        stack_required, ok := typecheck_statement(script, it, .NONE);
        if !ok  return 0, false;
        max_stack_required = max(max_stack_required, stack_required);
    }
    
    script.flags |= .AST_TYPECHECKED;
    return max_stack_required, true;
}

