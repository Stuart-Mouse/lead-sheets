
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
    node        : *Node, 
    check_flags :  Typechecking_Flags = .NONE, 
    expect_type : *Type_Info          = null
) -> (
    node_type      : *Type_Info
) {
    dbg_print("typechecking node %: % %\n", node, node.type, node.src_text);
    
    #if DEBUG {
        context.dbg_print_indent += 1;
        defer context.dbg_print_indent -= 1;
    }
    
    if node.type == {
      case .NUMBER;
        if check_flags & .EXPECT_LVALUE  return null;
        node.value_type = type_info(float32);
        return type_info(float32);
        
      case .VARIABLE;
        node.variable_index = resolve_variable(ctxt, node.src_text);
        if node.variable_index == -1 {
            dbg_print("Unable to resolve variable '%'!\n", node.src_text);
            return null;
        }
        
        node.value_type = variables[node.variable_index].binding.type;
        
        if check_flags & .EXPECT_LVALUE {
            node.flags |= .L_VALUE;
        }
        return node.value_type;
        
      case .OPERATION;
        if check_flags & .EXPECT_LVALUE  return null;
        
        left_type := typecheck_node(ctxt, node.operation.left, check_flags);
        if !left_type   return null;
        
        right_type := typecheck_node(ctxt, node.operation.right, check_flags);
        if !right_type  return null;
        
        operator_index, swap_args := resolve_builtin_operation(node.src_text, left_type, right_type);
        if operator_index >= 0 {
            node.operation.operator_index = operator_index;
            
            if swap_args {
                node.operation.left, node.operation.right = node.operation.right, node.operation.left;
            }
            
            node.value_type = cast(*Type_Info) BUILTIN_OPERATIONS[operator_index].ret;
            
            if is_aggr(node.value_type) {
                node.operation.dst = Dynamic_New(node.value_type,, ctxt.allocator);
                dbg_print("allocating variable of type % at % for node %\n", as_type(node.value_type), node.operation.dst, node);
            }
            
            return node.value_type;
        } 
        
        // if it was not one of the builtin operators, 
        // then we will try checking for an overloaded procedure that performs the operation
        node.operation.left.next = node.operation.right;
        
        proc_node := Node.{
            type      = .PROCEDURE_CALL,
            src_loc   = node.src_loc,
            src_text  = node.src_text,
            flags     = node.flags | .OVERLOAD_OP,
            procedure_call = .{
                arg_nodes = node.operation.left,
                arg_count = 2, // TODO: what if unary op
            }
        };
        node.* = proc_node;
        
        #through;
        
      case .PROCEDURE_CALL;
        if check_flags & .EXPECT_LVALUE  return null;
        
        dbg_print("typechecking arguments...\n");
        dbg_print("arg_count: %\n", node.procedure_call.arg_count);
        
        argument_types := NewArray(node.procedure_call.arg_count, *Type_Info,, temp); // TODO
        arg_node := node.procedure_call.arg_nodes;
        arg_idx  := 0;
        while arg_node != null {
            argument_type := typecheck_node(ctxt, arg_node);
            if !argument_type  return null;
            argument_types[arg_idx] = argument_type;
            
            arg_idx += 1;
            arg_node = arg_node.next;
        }
        
        node.procedure_call.procedure_index = resolve_procedure_index(ctxt, node.src_text, argument_types, node.flags & .OVERLOAD_OP != 0);
        if node.procedure_call.procedure_index == -1  return null;
        
        procedure := *procedures[node.procedure_call.procedure_index];
        node.value_type = procedure.type.return_types[0];
        
        if is_aggr(node.value_type) {
            node.procedure_call.dst = Dynamic_New(node.value_type,, ctxt.allocator);
            dbg_print("allocating variable of type % at % for node %\n", as_type(node.value_type), node.procedure_call.dst, node);
        }
        
        return node.value_type;
        
      case .MEMBER;
        base_type := typecheck_node(ctxt, node.member.base, check_flags & .EXPECT_LVALUE);
        if base_type.type != .STRUCT  return null;
        
        member_ti, member_offset := get_field(xx base_type, node.member.identifier);
        if member_ti == null  return null;
        
        node.value_type    = member_ti.type;
        node.member.offset = member_offset;
        
        if check_flags & .EXPECT_LVALUE {
            node.flags |= .L_VALUE;
        }
        return node.value_type;
        
      case .SUBSCRIPT;
        base_type :=  cast(*Type_Info_Array) typecheck_node(ctxt, node.subscript.base, check_flags & .EXPECT_LVALUE);
        if base_type.type != .ARRAY {
            log("Typechecking Error: Type '%' cannot be indexed!", as_type(base_type));
            return null;
        }
        
        indexing_type := typecheck_node(ctxt, node.subscript.indexing_expr);
        if indexing_type.type != .INTEGER && indexing_type.type != .FLOAT {
            log("Typechecking Error: Type '%' cannot be used in an indexing expression!", as_type(indexing_type));
            return null;
        }
        
        node.value_type = base_type.element_type;
        if check_flags & .EXPECT_LVALUE {
            node.flags |= .L_VALUE;
        }
        return node.value_type;
        
      case .ASSIGNMENT;
        // get type of left side
        // we expect left side to be an lvalue
        left_type := typecheck_node(ctxt, node.assignment.left, check_flags = .EXPECT_LVALUE);
        if !left_type  return null;
        
        // pass left type as 'expect_type' argument so that we can infer type of struct literals
        right_type := typecheck_node(ctxt, node.assignment.right, expect_type = left_type);
        if !right_type  return null;
        
        if left_type != right_type {
            dbg_print("types on left and right side of assignment statement do not match! % != %\n", as_type(left_type), as_type(right_type));
            return null;
        }
        return type_info(void);
        
      case .BLOCK;
        for node.block.statements {
            if !typecheck_node(ctxt, it)  return null;
        }
        return type_info(void);
        
      case .IF_STATEMENT;
        if !typecheck_node(ctxt, node.if_statement.condition)  return null; // TODO: must be bool or coercible to bool
        if !typecheck_node(ctxt, node.if_statement.statement)  return null;
        return type_info(void);
    }
    
    return null;
}

typecheck_script :: (script: *Script_Constructor) -> bool {
    if !script.ast_root                 return false;
    if script.ast_root.type != .BLOCK  return false;
    
    for *script.ast_root.block.statements {
        // TODO: probably check specifically that type returned here is void, since statements should not have a value_type.
        if !typecheck_node(script, it, .NONE)  return false;
    }
    
    script.flags |= .TYPECHECKED;
    return true;
}


is_aggr :: (t: Type) -> bool { return is_aggr(cast(*Type_Info)t); }

is_aggr :: (ti: *Type_Info) -> bool {
    if ti.type == {
        case .INTEGER; return false;
        case .FLOAT;   return false;
        case .ENUM;    return false;
        case .POINTER; return false;
    }
    return true;
}
