

/*
    evaluate.jai
    
    As an alternative to execute_node, which pushes results on the script stack, evalaute_node will allocate the intermediate values in temproary storage.
    In the future, it may be ideal to re-implement execute_node in terms of evaluate_node, using hint_storage to stick the results on the script stack instead of doing so by default
    Ultimately, this should simplify execute_node while also offering multiple ways of interfacing with lead sheets expressions.
    
*/

evaluate_expression :: (script: *Script, expr: string) -> (Any, bool) {
    root := parse_expression(script, expr);
    if !typecheck_node(script, root, .IS_STATEMENT_ROOT)  return Any.{}, false;
    
    value, ok := evaluate_node(script, root);
    if !ok  return Any.{}, false;
    
    return value, true;
}

evaluate_node :: (
    script:         *Script, 
    node:           *Node,
    hint_storage:   *void    = null
) -> Any, bool {
    dprint("evaluating node %: %\n", node, node.node_type);
    dprint_push_indent();
    
    // only allocates space for the return value if hint_storage was null (TODO?: or hint_type did not match node.value_type)
    make_return_value :: () -> Any #expand {
        ret := Any.{ node.value_type, hint_storage };
        if ret.value_pointer == null {
            ret.value_pointer = Dynamic_New(node.value_type,, temp);
        }
        return ret;
    }
    
    if node.node_type == {
      case Node_Directive;
        return null, true; // Directives are run as soon as they are parsed, so just skip this node.
        
        
      case Node_Literal;
        literal := node.(*Node_Literal);
        if literal.flags & .IS_LVALUE then assert(literal.flags & .IS_MALLEABLE == .IS_MALLEABLE || literal.literal_type == .ANY);
        
        if literal.literal_type == {
          case .ANY;
            dprint("returning any literal with value %\n", literal.any);
            return literal.any, true;
            
          case .STRING;
            dprint("returning string literal with value %\n", literal.text);
            return literal.any, true;
            
          case .NUMBER;
            return to_any(literal.number), true;
            
          case .STRUCT;
            ret := make_return_value();
            member_ptr := ret.value_pointer;
            for literal.aggr.expressions {
                member := literal.value_type.(*Type_Info_Struct).members[it_index];
                member_any, ok := evaluate_node(script, it, hint_storage = member_ptr);
                if member_any.value_pointer != member_ptr {
                    memcpy(member_ptr, member_any.value_pointer, member.type.runtime_size);
                }
                member_ptr += member.type.runtime_size;
            }
            return ret, true;
            
          case .ARRAY;
            log("Error: array literals not yet implemented.");
        }
        return Any.{}, false;
        
        
      case Node_Identifier;
        identifier := node.(*Node_Identifier);
        if identifier.identifier_type == {
          case .DECLARATION;
            declaration := identifier.declaration;
            variable_any := Any.{ identifier.value_type, declaration.value_pointer };
            dprint("returning internal variable with value % at %\n", variable_any, variable_any.value_pointer);
            return variable_any, true;
            
          case .EXTERNAL_VARIABLE;
            variable := *script.variables[identifier.variable_index];
            dprint("returning external variable with value % at %\n", variable.binding, variable.binding.value_pointer);
            return variable.binding, true;
            
          case .EXTERNAL_PROCEDURE;
            procedure := *script.procedures[identifier.procedure_index];
            dprint("returning external procedure with type % at %\n", as_type(procedure.type), procedure.ptr);
            assert(identifier.flags & .IS_LVALUE == 0);
            return Any.{ procedure.type, *procedure.ptr }, true;
        }
        return null, false;

        
      case Node_Operation;
        operation := node.(*Node_Operation);
        
        if operation.flags & .OVERLOAD {
            any, ok := evaluate_node(script, operation.overload_procedure);
            return any, ok;
        }
        
        left_any, ok := evaluate_node(script, operation.left);
        if !ok  return null, false;
        
        right_any: Any;
        if operation.operator_type != .UNARY {
            right_any, ok = evaluate_node(script, operation.right);
            if !ok  return null, false;
        }
        
        // special case: operator = is always a simple memcpy
        if operation.name == "=" {
            assert(operation.left.flags & .IS_LVALUE != 0);
            memcpy(left_any.value_pointer, right_any.value_pointer, right_any.type.runtime_size);
            return null, true;
        }
        
        ret := make_return_value();
        
        ok = execute_builtin_operation(operation.operator_index, left_any.value_pointer, right_any.value_pointer, ret.value_pointer);
        if !ok  return null, false;
        
        dprint("returning result of binary operation: % %\n", as_type(operation.value_type), ret);
        return ret, true;
        
        
      case Node_Procedure_Call;
        procedure_call := node.(*Node_Procedure_Call);
        
        ret := make_return_value();
        
        proc_any, ok := evaluate_node(script, procedure_call.procedure_expression);
        if !ok  return null, false;
        assert(proc_any.type.type == .PROCEDURE);
        dprint("proc_type: %\n", as_type(proc_any.type));
        
        proc_ptr := proc_any.value_pointer.(**void).*;
        ti_proc  := proc_any.type.(*Type_Info_Procedure);
        
        {
            dcReset(script.dyncall_vm);
            jai_call :: true;
            if jai_call || !(ti_proc.procedure_flags & .HAS_NO_CONTEXT) {
                dcArgPointer(script.dyncall_vm, *context);
            }
            
            return_type := ifx ti_proc.return_types 
                then ti_proc.return_types[0]
                else type_info(void);
            
            for procedure_call.arguments {
                arg_any, ok := evaluate_node(script, it);
                if !ok  return null, false;
                if !push_argument(script.dyncall_vm, arg_any.type, arg_any.value_pointer, true)  return null, false;
            }
            
            dcArgPointer(script.dyncall_vm, ret.value_pointer);
            dcCallVoid(script.dyncall_vm, xx proc_ptr);
        }
        
        dprint("returning result of procedure call: %\n", Any.{ procedure_call.value_type, ret.value_pointer });
        return ret, true;
        
        
      case Node_Cast;
        node_cast := node.(*Node_Cast);
        
        ret := make_return_value();
        
        left_any, ok := evaluate_node(script, node_cast.left);
        if !remap_data(ret, left_any)  return null, false;
        
        return ret, true;
        
        
      case Node_Dot;
        dot := node.(*Node_Dot);
        
        struct_any, ok := evaluate_node(script, dot.left);
        if !ok  return null, false;
        member_any := Any.{ dot.value_type, struct_any.value_pointer + dot.member_offset };
        
        dprint("struct value ptr: %\n", struct_any.value_pointer);
        dprint("member value ptr: %\n", member_any.value_pointer);
        
        return member_any, true;
        
        
      case Node_Subscript;
        subscript := node.(*Node_Subscript);
        
        base_any,   ok := evaluate_node(script, subscript.base_expression);
        index_any:, ok  = evaluate_node(script, subscript.indexing_expression);
        
        ti_array := subscript.base_expression.value_type.(*Type_Info_Array);
        indexing_type := subscript.indexing_expression.value_type;
        
        // get index as s64
        // TODO: maybe improve this
        //       for bytecode, could do an instruction for inplace type cast
        index_as_s64: s64;
        assert(remap_int(index_as_s64, index_any));
        
        array_count, array_data := get_array_count_and_data(base_any.value_pointer, xx base_any.type);
        
        if index_as_s64 < 0 || index_as_s64 >= array_count {
            log("Runtime Error: array index '%' was out of bounds! Max index is %.", index_as_s64, array_count-1);
            return null, false;
        }
        
        element_any := Any.{ node.value_type, array_data + index_as_s64 * node.value_type.runtime_size };
        dprint("returning result of indexing operation: %\n", element_any);
        return element_any, true;
        
    
      case Node_Declaration;
        declaration := node.(*Node_Declaration);
        // if declaration.declaration_type != .INTERNAL_VARIABLE {
        //     log("Error: cannot execute a declaration node with declaration type of %", declaration.declaration_type);
        //     return false;
        // }
        
        // TODO: we need to check that the identifier does not refer to a previously declared variable
        if declaration.init_expression {
            r_value, ok := evaluate_node(script, declaration.init_expression);
            if !ok {
                dprint("failed to evaluate init expression of declaration statement!\n");
                return null, false;
            }
            
            l_value := Any.{ declaration.value_type, declaration.value_pointer };
            
            dprint("l_value.value_pointer: %\n", l_value.value_pointer);
            dprint("r_value.value_pointer: %\n", r_value.value_pointer);
            
            dprint("l_value: % % at %\n", as_type(l_value.type), l_value, l_value.value_pointer);
            dprint("r_value: % % at %\n", as_type(r_value.type), r_value, r_value.value_pointer);
            
            memcpy(l_value.value_pointer, r_value.value_pointer, r_value.type.runtime_size);
        } else {
            l_value := Any.{ declaration.value_type, declaration.value_pointer };
            memset(l_value.value_pointer, 0, l_value.type.runtime_size);
        }
        return null, true;
        
        
      case Node_Block;
        block := node.(*Node_Block);
        for block.statements {
            dprint("executing statement #%\n", it_index);
            _, ok := evaluate_node(script, it);
            if !ok  return null, false;
        }
        return null, true;
        
        
      case Node_If_Statement;
        if_statement := node.(*Node_If_Statement);
        condition, ok := evaluate_node(script, if_statement.condition);
        if !ok {
            dprint("failed to evaluate left side of assignment statement!\n");
            return null, false;
        }
        dprint("condition: % %\n", as_type(condition.type), condition);
        
        condition_as_bool: bool;
        any_to_bool(condition_as_bool, condition);
        if condition_as_bool {
            _, ok := evaluate_node(script, if_statement.statement);
            if !ok {
                dprint("failed to evaluate left side of assignment statement!\n");
                return null, false;
            }
        }
        return null, true;
        
        
      case Node_While_Loop;
        while_loop := node.(*Node_While_Loop);
        while loop := true {
            condition, ok := evaluate_node(script, while_loop.condition);
            if !ok {
                dprint("failed to evaluate left side of assignment statement!\n");
                return null, false;
            }
            dprint("condition: % %\n", as_type(condition.type), condition);
            
            condition_as_bool: bool;
            any_to_bool(condition_as_bool, condition);
            if !condition_as_bool  break loop;
            
            _, ok = evaluate_node(script, while_loop.statement);
            if !ok {
                dprint("failed to evaluate left side of assignment statement!\n");
                return null, false;
            }
        }
        return null, true;
        
        
      case Node_For_Loop;
        for_loop := node.(*Node_For_Loop);
        if for_loop.control_type == {
          case .ARRAY;
            array, ok := evaluate_node(script, for_loop.array_expression);
            if !ok {
                dprint("failed to evaluate for loop control node!\n");
                return null, false;
            }
            
            ti_array := for_loop.array_expression.value_type.(*Type_Info_Array);
            
            array_count, array_data := get_array_count_and_data(array.value_pointer, xx array.type);
            
            // these will be accessed by iterator and iterator_index nodes during iteration
            it_index: int;
            for_loop.it_decl.value_pointer       = array_data;
            for_loop.it_index_decl.value_pointer = *it_index;
            
            while it_index < array_count {
                defer {
                    for_loop.it_decl.value_pointer += ti_array.element_type.runtime_size;
                    it_index += 1;
                }
                
                _, ok := evaluate_node(script, for_loop.statement);
                if !ok {
                    dprint("failed to evaluate left side of assignment statement!\n");
                    return null, false;
                }
            }
            return null, true;
            
          case .RANGE;
            lower, ok := evaluate_node(script, for_loop.range.lower);
            if !ok {
                dprint("failed to evaluate for_loop.range.lower!\n");
                return null, false;
            }
            upper:, ok = evaluate_node(script, for_loop.range.upper);
            if !ok {
                dprint("failed to evaluate for_loop.range.upper!\n");
                return null, false;
            }
            
            control_type := for_loop.range.lower.value_type;
            upper_as_s64: int;
            lower_as_s64: int;
            remap_int(upper_as_s64, upper);
            remap_int(lower_as_s64, lower);
            
            for lower_as_s64..upper_as_s64 {
                _it: u64 = 0; // just need 8 bytes of zeros. we will just use this space as dst for dynamically typed int value for iterator
                remap_data(Any.{ control_type, *_it }, it);
                
                for_loop.it_decl.value_pointer = *_it;
                // TODO: for now we just don't set it_index. 
                // we should probably prevent using it_index for range-based loops in typecheck 
                
                _, ok := evaluate_node(script, for_loop.statement);
                if !ok {
                    dprint("failed to evaluate left side of assignment statement!\n");
                    return null, false;
                }
            }
            return null, true;
        }
        
        return null, false;
    }
    
    assert(false);
    return null, false;
}





evaluate_directive :: (script: *Script, directive: *Node_Directive) -> bool {
    procedure := script.directives[directive.directive_index];
    ti_proc   := procedure.type;
    
    // + 2 below for implicitly passing script pointer and data pointer
    if ti_proc.argument_types.count != directive.arguments_count + 2 {
        log("Error: incorrect number of arguments provided for procedure call. Expected %, got %.\n", ti_proc.argument_types.count, directive.arguments_count);
        return false;
    }
    
    for directive.arguments {
        argument_type := typecheck_node(script, it);
        expected_type := ti_proc.argument_types[it_index + 2]; // + 1 for implicitly passing script pointer and data pointer
        if !argument_type || argument_type != expected_type {
            log("Error: mismatch on argument % for procedure call. Expected %, got %.\n", it, as_type(expected_type), as_type(argument_type));
            return false;
        }
    }
    
    assert(ti_proc.return_types.count == 1);
    assert(ti_proc.return_types[0] == type_info(bool));
    
    assert(ti_proc.argument_types.count >= 2);
    assert(ti_proc.argument_types[0] == type_info(*Script));
    assert(ti_proc.argument_types[1] == type_info(*void));
    assert(ti_proc.procedure_flags & .IS_C_CALL == 0);
    
    return_type := ti_proc.return_types[0];
    return_ptr  := script.stack_ptr;
    
    if is_aggr(return_type) {
        stack_push_any(script, Any.{ return_type, return_ptr });         
    } else {
        stack_push_zeroes(script, 8);
    }
    arg_ptr_start := script.stack_ptr;
    
    for directive.arguments {
        _, ok := evaluate_node(script, it);
        if !ok  return false;
    }
    
    dcReset(script.dyncall_vm);
    jai_call :: true;
    if !jai_call || !(ti_proc.procedure_flags & .HAS_NO_CONTEXT) {
        dcArgPointer(script.dyncall_vm, *context);
    }
    
    if !push_argument(script.dyncall_vm, type_info(*Script), *script)  return false;
    if !push_argument(script.dyncall_vm, type_info(*void), *procedure.data)  return false;
    
    arg_ptr := arg_ptr_start;
    for arg_type: array_view(ti_proc.argument_types, 2) {
        value_pointer := arg_ptr;
        if is_aggr(arg_type)  value_pointer = value_pointer.(**void).*;
        if !push_argument(script.dyncall_vm, arg_type, value_pointer, true)  return false;
        arg_ptr += size_of(*void);
    }
    
    dcArgPointer(script.dyncall_vm, return_ptr);
    dcCallVoid(script.dyncall_vm, xx procedure.ptr);
    
    script.stack_ptr = arg_ptr_start;
    
    assert(return_type == type_info(bool)); // can't be too safe!
    return stack_pop_any(script, return_type).value_pointer.(*bool).*;
}