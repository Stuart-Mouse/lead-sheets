
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
    hint_type : *Type_Info          = null
) -> (
    node_type      : *Type_Info
) {
    dbg_print("typechecking node %: %\n", node, node.type, node.src_text);
    dbg_print("expecting type: %\n", as_type(hint_type));
    
    #if DEBUG {
        context.dbg_print_indent += 1;
        defer context.dbg_print_indent -= 1;
    }
    
    if node.type == {
      case .NUMBER;
        if check_flags & .EXPECT_LVALUE {
            dbg_print("Error: number literal cannot be used as an lvalue.\n");
            return null;
        }
        
        if hint_type && (hint_type.type == .INTEGER || hint_type.type == .FLOAT) {
            dbg_print("returning expected type: %\n", as_type(hint_type));
            node.value_type = hint_type;
            return hint_type;
        }
        
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
            node.flags |= .IS_LVALUE;
        }
        return node.value_type;
        
      case .PROCEDURE;
        // in order to typecheck a procedure node, we would need to know what type ot expect, so that we can resolve overloads
        // so for now, this node can only be used in the context of a PROCEDURE_CALL, where it is typechecked inline
        dbg_print("Error: typechecking a procedure node in isolation is not currently supported.\n");
        return null;
        
      case .OPERATION;
        if check_flags & .EXPECT_LVALUE  return null;
        
        left_type := typecheck_node(ctxt, node.operation.left, check_flags, hint_type);
        if !left_type   return null;
        
        right_type := typecheck_node(ctxt, node.operation.right, check_flags, hint_type);
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
        
        // need to create a new node for the procedure itself, kinda dumb
        proc_node := alloc_node(ctxt);
        proc_node.type            = .PROCEDURE;
        proc_node.src_text        = node.src_text;
        proc_node.src_loc         = node.src_loc;
        proc_node.enclosing_scope = node.enclosing_scope;
        
        // then replace the current operation node with a procedure_call node
        proc_call_node := Node.{
            type            = .PROCEDURE_CALL,
            src_loc         = node.src_loc,
            src_text        = node.src_text,
            flags           = node.flags | .OVERLOAD_OP,
            enclosing_scope = node.enclosing_scope,
            procedure_call  = .{
                procedure_node = proc_node,
                arg_nodes = node.operation.left,
                arg_count = 2, // TODO: what if unary op
            }
        };
        node.* = proc_call_node;
        
        #through;
        
      case .PROCEDURE_CALL;
        if check_flags & .EXPECT_LVALUE  return null;
        
        dbg_print("typechecking arguments...\n");
        dbg_print("arg_count: %\n", node.procedure_call.arg_count);
        
        argument_types := NewArray(node.procedure_call.arg_count, *Type_Info,, temp); // TODO
        for node.procedure_call.arg_nodes {
            argument_type := typecheck_node(ctxt, it);
            if !argument_type  return null;
            argument_types[it_index] = argument_type;
        }
        
        proc_node := node.procedure_call.procedure_node;
        if proc_node == null {
            dbg_print("proc_node was null!\n");
            return null;
        }
        
        if node.procedure_call.procedure_node.type == {
          case .PROCEDURE;
            // we don't call typecheck_node on proc_node, because we need the context of the arguments here to resolve overloaded procedures 
            // so we basically just typecheck this node inline here and set its value_type manually below
            
            proc_node.procedure_index = resolve_procedure_index(ctxt, proc_node.src_text, argument_types, node.flags & .OVERLOAD_OP != 0);
            if proc_node.procedure_index == -1  return null;
            
            procedure := *procedures[proc_node.procedure_index];
            proc_node.value_type = procedure.type;
            node.value_type = ifx procedure.type.return_types then procedure.type.return_types[0] else type_info(void);
            
            
          case;
            ti_proc := cast(*Type_Info_Procedure) typecheck_node(ctxt, proc_node);
            if ti_proc == null {
                dbg_print("Error: failed to typecheck procedure_node.\n");
                return null;
            } 
            if ti_proc.type != .PROCEDURE {
                dbg_print("Error: attempt to call a non-procedure type as if it were a procedure.\n");
                return null;
            }
            if ti_proc.return_types.count > 1 {
                dbg_print("Error: attempt to call a procedure which returns more than one value. This is not currently supported.\n");
                return null;
            }
            
            // typecheck arguments. we know what they must be, so we just straightforwardly match. can also easily add logic to coerce/cast
            // TODO: coerce/up-cast arguments
            if ti_proc.argument_types.count != argument_types.count {
                dbg_print("Error: incorrect number of arguments provided for procedure call. Expected %, got %.\n", ti_proc.argument_types.count, argument_types.count);
                return null;
            }
            for 0..argument_types.count-1 {
                if ti_proc.argument_types[it] != argument_types[it] {
                    dbg_print("Error: mismatch on argument % for procedure call. Expected %, got %.\n", it, as_type(ti_proc.argument_types[it]), as_type(argument_types[it]));
                    return null;
                }
            }
            
            node.value_type = ifx ti_proc.return_types then ti_proc.return_types[0] else type_info(void);
        }
        
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
        
        dbg_print("member value type: %\n", as_type(member_ti.type));
        dbg_print("member offset: %\n", member_offset);
        
        if check_flags & .EXPECT_LVALUE {
            node.flags |= .IS_LVALUE;
        }
        return node.value_type;
        
        
      case .SUBSCRIPT;
        base_type :=  cast(*Type_Info_Array) typecheck_node(ctxt, node.subscript.base, check_flags & .EXPECT_LVALUE);
        if base_type.type != .ARRAY {
            log("Typechecking Error: Type '%' cannot be indexed!", as_type(base_type));
            return null;
        }
        
        indexing_type := typecheck_node(ctxt, node.subscript.indexing_expr, hint_type = type_info(int));
        if indexing_type.type != .INTEGER && indexing_type.type != .FLOAT {
            log("Typechecking Error: Type '%' cannot be used in an indexing expression!", as_type(indexing_type));
            return null;
        }
        
        node.value_type = base_type.element_type;
        if check_flags & .EXPECT_LVALUE {
            node.flags |= .IS_LVALUE;
        }
        return node.value_type;
        
        
      case .ASSIGNMENT;
        if check_flags & .EXPECT_LVALUE  return null;
        
        // get type of left side
        // we expect left side to be an lvalue
        left_type := typecheck_node(ctxt, node.assignment.left, check_flags = .EXPECT_LVALUE);
        if !left_type  return null;
        
        // pass left type as 'hint_type' argument so that we can infer type of struct literals
        right_type := typecheck_node(ctxt, node.assignment.right, hint_type = left_type);
        if !right_type  return null;
        
        if left_type != right_type {
            dbg_print("types on left and right side of assignment statement do not match! % != %\n", as_type(left_type), as_type(right_type));
            return null;
        }
        
        node.value_type = type_info(void);
        return node.value_type;
        
        
      case .BLOCK;
        if check_flags & .EXPECT_LVALUE  return null;
        
        for node.block.statements {
            if !typecheck_node(ctxt, it)  return null;
        }
        
        node.value_type = type_info(void);
        return node.value_type;
        
        
      case .IF_STATEMENT;
        if check_flags & .EXPECT_LVALUE  return null;
        
        if !typecheck_node(ctxt, node.if_statement.condition)  return null; // TODO: must be bool or coercible to bool
        if !typecheck_node(ctxt, node.if_statement.statement)  return null;
        
        node.value_type = type_info(void);
        return node.value_type;
        
        
      case .WHILE_LOOP;
        if check_flags & .EXPECT_LVALUE  return null;
        
        if !typecheck_node(ctxt, node.while_loop.condition)  return null; // TODO: must be bool or coercible to bool
        if !typecheck_node(ctxt, node.while_loop.statement)  return null;
        
        node.value_type = type_info(void);
        return node.value_type;
        
        
      case .FOR_LOOP;
        if check_flags & .EXPECT_LVALUE  return null;
        
        control_type: *Type_Info;
        if node.for_loop.control.type == {
          case .RANGE;
            // TODO: this won't work for negative numbers, since the expect type doesn't get passed up through operation node...
            control_type = typecheck_node(ctxt, node.for_loop.control, hint_type = type_info(int)); 
          case;
            control_type = typecheck_node(ctxt, node.for_loop.control, check_flags = .EXPECT_LVALUE);
        }
        
        // TODO: may not allow float here?
        if !control_type || (control_type.type != .ARRAY && control_type.type != .INTEGER && control_type.type != .FLOAT)  return null; 
        
        if !typecheck_node(ctxt, node.for_loop.statement)  return null;
        
        node.value_type = type_info(void);
        return node.value_type;
        
        
      case .RANGE;
        if check_flags & .EXPECT_LVALUE  return null;
        
        dbg_print("typechecking range lower\n");
        assert(node.range.lower != null);
        lower_type := typecheck_node(ctxt, node.range.lower, hint_type = type_info(int));
        if lower_type == null  return null;
        
        dbg_print("typechecking range upper\n");
        assert(node.range.upper != null);
        upper_type := typecheck_node(ctxt, node.range.upper, hint_type = lower_type);
        if lower_type != upper_type { 
            dbg_print("Error: type mismatch on lower and upper bounds of range.\n");
            return null;
        }
        
        node.value_type = lower_type;
        return node.value_type;
        
      case .ITERATOR;
        node.iterator.parent_for = get_enclosing_for_loop(node.enclosing_scope);
        if !node.iterator.parent_for {
            dbg_print("Error: Attempt to use 'it' ouside of the context of a for loop!\n");
            return null;
        }
        
        control_type := node.iterator.parent_for.for_loop.control.value_type;
        if control_type.type == {
          case .ARRAY;
            node.value_type = (cast(*Type_Info_Array)control_type).element_type;
            // in future, may need checks on whether array itself is an lvalue
            // this would be important if we wanted to have array literals in scripts
            if check_flags & .EXPECT_LVALUE {
                node.flags |= .IS_LVALUE;
            }
            return node.value_type;
            
          case .FLOAT; #through; // ehh...
          case .INTEGER;
            node.value_type = control_type;
            if check_flags & .EXPECT_LVALUE {
                dbg_print("Error: attempt to use an integer-typed iterator as an lvalue.\n");
                return null;
            }
            return node.value_type;
        }
        
        dbg_print("Error: Iterator used in a for loop with an invalid control type? (%)\n", as_type(control_type));
        return null;
        
        
      case .ITERATOR_INDEX;
        if check_flags & .EXPECT_LVALUE  return null;
        
        node.iterator.parent_for = get_enclosing_for_loop(node.enclosing_scope);
        if !node.iterator.parent_for  return null;
        
        if node.iterator.parent_for.for_loop.control.value_type.type != .ARRAY {
            dbg_print("Error: attempt to use it_index in a range-based for loop.\n");
            return null;
        }
        
        node.value_type = type_info(int);
        return node.value_type;
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
        case .INTEGER;   return false;
        case .FLOAT;     return false;
        case .ENUM;      return false;
        case .BOOL;      return false;
        case .POINTER;   return false;
        case .PROCEDURE; return false;
        case .TYPE;      return false;
    }
    return true;
}
