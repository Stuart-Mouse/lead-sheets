

/*
    evaluate.jai
    
    As an alternative to execute_node, which pushes results on the script stack, evalaute_node will allocate the intermediate values in temproary storage.
    In the future, it may be ideal to re-implement execute_node in terms of evaluate_node, using hint_storage to stick the results on the script stack instead of doing so by default
    Ultimately, this should simplify execute_node while also offering multiple ways of interfacing with lead sheets expressions.
*/

evaluate_expression :: (script: *Script, expr: string) -> (Any, bool) {
    root := parse_expression_string(script, expr);
    if root == null  return Null_Any, false;
    
    if !typecheck_node(script, root, .IS_STATEMENT_ROOT)  return Null_Any, false;
    
    value := evaluate_node(script, root);
    if !!script.error  return Null_Any, false;
    
    return value, true;
}

evaluate_statement :: (script: *Script, expr: string, store_global_declarations := false) -> (Any, bool) {
    root := parse_statement(script, expr);
    if root == null  return Null_Any, false;
    
    if !typecheck_node(script, root, .IS_STATEMENT_ROOT)  return Null_Any, false;
    
    value := evaluate_node(script, root);
    if !!script.error  return Null_Any, false;
    
    if store_global_declarations {
        if root.node_type == Node_Declaration {
            array_add(*script.global_declarations, xx root);
        }
    }
    
    return value, true;
}

evaluate_node :: (
    script:         *Script, 
    node:           *Node,
    hint_storage:   *void    = null,
    explicit_call := false
) -> Any {
    dprint("evaluating node %: %\n", node, node.node_type);
    dprint_push_indent();
    
    if script.evaluate_callback {
        result := script.evaluate_callback(script, node, hint_storage, explicit_call);
        if !!script.error  return Null_Any;
        if result.type != null  return result;
    }
    
    // If no hint_storage is provided, then we allocate in temp storage.
    // The caller should always be aware of this and call the function accordingly!
    result := Any.{ node.value_type, hint_storage };
    if result.value_pointer == null {
        result.value_pointer = Dynamic_New(node.value_type,, temp);
    }
    
    if node.node_type == {
      case Node_Directive;
        directive := node.(*Node_Directive);
        if directive.runtime_node {
            result = evaluate_node(script, directive.runtime_node, hint_storage);
        }
        
        
      case Node_Literal;
        literal := node.(*Node_Literal);
        if literal.flags & .IS_LVALUE then assert(literal.flags & .IS_MALLEABLE == .IS_MALLEABLE || literal.literal_type == .ANY);
        
        if #complete literal.literal_type == {
          case .ANY;     result = literal.any;
          case .STRING;  result = literal.text;
          case .NUMBER;  result = to_any(*literal.number);
          case .STRUCT;
            ti_struct := literal.value_type.(*Type_Info_Struct);
            assert(ti_struct.type == .STRUCT);
            for literal.aggr.expressions {
                member     := ti_struct.members[it_index];
                member_ptr := result.value_pointer + member.offset_in_bytes;
                member_any := evaluate_node(script, it, member_ptr);
            }
        }
        
        
      case Node_Identifier;
        identifier := node.(*Node_Identifier);
        if #complete identifier.identifier_type == {
          case .DECLARATION;
            declaration := identifier.declaration;
            result = Any.{ identifier.value_type, declaration.value_pointer };
            dprint("returning internal variable with value % at %\n", result, result.value_pointer);
            
          case .EXTERNAL_VARIABLE;
            variable := *script.variables[identifier.index];
            result = variable.binding;
            dprint("returning external variable with value % at %\n", result, result.value_pointer);
            
          case .EXTERNAL_PROCEDURE;
            procedure := *script.procedures[identifier.index];
            result = Any.{ procedure.type, *procedure.pointer };
            // dprint("returning external procedure with type % at %\n", as_type(result.type), procedure.pointer);
            assert(identifier.flags & .IS_LVALUE == 0);
            
          case .LITERAL;
            result = evaluate_node(script, identifier.literal, hint_storage);
            
          case .TYPE;
            result = script.type_table[identifier.index].type;
            
          case; #through; 
          case .STRUCT_MEMBER; #through; // we don't evaluate this case since the parent Node_Dot will just grab the struct member offset manually.
          case .UNRESOLVED;
            assert(false, "invalid case %", identifier.identifier_type);
        }
        
        
      case Node_Operation;
        operation := node.(*Node_Operation);
        
        if operation.flags & .OVERLOAD {
            result = evaluate_node(script, operation.overload_procedure, hint_storage);
        } else {
            left_any := evaluate_node(script, operation.left);
            if !!script.error  return Null_Any;
            
            right_any: Any;
            if operation.operator_type != .UNARY {
                right_any = evaluate_node(script, operation.right);
                if !!script.error  return Null_Any;
            }
            
            if operation.name == "=" {
                // special case: operator = is always a simple memcpy
                assert(operation.left.flags & .IS_LVALUE != 0);
                memcpy(left_any.value_pointer, right_any.value_pointer, right_any.type.runtime_size);
                result = left_any;
            } else {
                assert(operation.value_type == result.type);
                if !execute_builtin_operation(operation.operator_index, left_any.value_pointer, right_any.value_pointer, result.value_pointer) {
                    set_general_error(script, "Failed while trying to execute builtin operation, operator index: %", operation.operator_index);
                    return Null_Any;
                }
            }
            
            dprint("returning result of binary operation % % % -> % %\n", left_any, operation.name, right_any, as_type(result.type), result);
        }
        
        
      case Node_Procedure_Call;
        procedure_call := node.(*Node_Procedure_Call);
        
        proc_any := evaluate_node(script, procedure_call.procedure_expression);
        if !!script.error  return Null_Any;
        
        assert(proc_any.type.type == .PROCEDURE);
        dprint("proc_type: %\n", as_type(proc_any.type));
        
        arguments_anys := NewArray(procedure_call.arguments.count, Any,, temp);
        for procedure_call.arguments {
            arguments_anys[it_index] = evaluate_node(script, it);
            if !!script.error  return Null_Any;
        }
        
        proc_ptr := proc_any.value_pointer.(**void).*;
        ti_proc  := proc_any.type.(*Type_Info_Procedure);
        
        // TODO: we really should support #c_call procedures in Lead Sheets, kind surprised I never got around to that tbh...
        {
            dcReset(script.dyncall_vm);
            if !(ti_proc.procedure_flags & .HAS_NO_CONTEXT) {
                dcArgPointer(script.dyncall_vm, *context);
            }
            
            return_type := ifx ti_proc.return_types 
                then ti_proc.return_types[0]
                else type_info(void);
            
            for arguments_anys 
                if !push_argument(script.dyncall_vm, it.type, it.value_pointer, true)  
                    return Null_Any;
            
            if return_type != type_info(void) {
                dcArgPointer(script.dyncall_vm, result.value_pointer);
            }
            dcCallVoid(script.dyncall_vm, xx proc_ptr);
        }
        dprint("returning result of procedure call: %\n", Any.{ procedure_call.value_type, result.value_pointer });
        
        
      case Node_Cast;
        node_cast := node.(*Node_Cast);
        
        left_any := evaluate_node(script, node_cast.value);
        if !remap_data(result, left_any) {
            set_general_error(script, "Failed while trying to remap_data in Node_Cast: % -> %", as_type(left_any.type), as_type(result.type));
            return Null_Any;
        }
        
        
      case Node_Dot;
        dot := node.(*Node_Dot);
        
        if dot.left == null {
            assert(node.value_type.type == .ENUM || node.value_type.type == .STRUCT);
            result = evaluate_node(script, dot.right);
        } else {
            if dot.left.value_type.type == {
              case .STRUCT;
                struct_any := evaluate_node(script, dot.left);
                if !!script.error  return Null_Any;
                
                result = Any.{ dot.value_type, struct_any.value_pointer + dot.right.(*Node_Identifier).member.offset_in_bytes };
                dprint("struct value ptr: %\n", struct_any.value_pointer);
                dprint("member value ptr: %\n", result.value_pointer);
                
              case .TYPE;
                result = evaluate_node(script, dot.right, hint_storage);
                if !!script.error  return Null_Any;
            }
        }
        
        
      case Node_Subscript;
        subscript := node.(*Node_Subscript);
        
        base_any  := evaluate_node(script, subscript.base_expression);
        if !!script.error  return Null_Any;
        index_any := evaluate_node(script, subscript.indexing_expression);
        if !!script.error  return Null_Any;
        
        ti_array := subscript.base_expression.value_type.(*Type_Info_Array);
        
        // get index as s64
        // TODO: maybe improve this. for bytecode, could do an instruction for inplace type cast
        index_as_s64, ok := remap_to(s64, index_any);  assert(ok);
        
        array_count, array_data := get_array_count_and_data(base_any.value_pointer, xx base_any.type);
        
        if index_as_s64 < 0 || index_as_s64 >= array_count {
            set_general_error(script, "Array index '%' was out of bounds! Max index is %.", index_as_s64, array_count-1);
            return Null_Any;
        }
        
        result = Any.{ node.value_type, array_data + index_as_s64 * node.value_type.runtime_size };
        dprint("returning result of indexing operation: %\n", result);
        
    
      case Node_Declaration;
        assert(hint_storage == null);
        declaration := node.(*Node_Declaration);
        
        l_value := Any.{ declaration.value_type, declaration.value_pointer };
        if declaration.init_expression {
            evaluate_node(script, declaration.init_expression, l_value.value_pointer);
            if !!script.error  return Null_Any;
        } else {
            memset(l_value.value_pointer, 0, l_value.type.runtime_size);
        }
        
        
      case Node_Block;
        assert(hint_storage == null);
        block := node.(*Node_Block);
        if block.name && !explicit_call  return Null_Any;
        
        for block.statements {
            dprint("executing statement #%\n", it_index);
            evaluate_node(script, it);
            if !!script.error  return Null_Any;
        }
        
        
      case Node_If_Statement;
        assert(hint_storage == null);
        if_statement := node.(*Node_If_Statement);
        condition := evaluate_node(script, if_statement.condition);
        if !!script.error  return Null_Any;
        
        dprint("condition: % %\n", as_type(condition.type), condition);
        
        condition_as_bool: bool;
        any_to_bool(condition_as_bool, condition);
        if condition_as_bool {
            evaluate_node(script, if_statement.statement);
            if !!script.error  return Null_Any;
        }
        
        
      case Node_While_Loop;
        assert(hint_storage == null);
        while_loop := node.(*Node_While_Loop);
        while loop := true {
            condition := evaluate_node(script, while_loop.condition);
            if !!script.error  return Null_Any;
            
            dprint("condition: % %\n", as_type(condition.type), condition);
            
            condition_as_bool: bool;
            any_to_bool(condition_as_bool, condition);
            if !condition_as_bool  break loop;
            
            evaluate_node(script, while_loop.statement);
            if !!script.error  return Null_Any;
        }
        
        
      case Node_For_Loop;
        assert(hint_storage == null);
        for_loop := node.(*Node_For_Loop);
        if for_loop.control_type == {
          case .ARRAY;
            array := evaluate_node(script, for_loop.array_expression);
            if !!script.error  return Null_Any;
            
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
                
                evaluate_node(script, for_loop.statement);
                if !!script.error  return Null_Any;
            }
            
          case .RANGE;
            lower := evaluate_node(script, for_loop.range.lower);
            if !!script.error  return Null_Any;
            upper := evaluate_node(script, for_loop.range.upper);
            if !!script.error  return Null_Any;
            
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
                
                evaluate_node(script, for_loop.statement);
                if !!script.error  return Null_Any;
            }
            
          case .LIST;
            _it_index: int;
            for_loop.it_index_decl.value_pointer = *_it_index;
            for for_loop.list {
                _it_index = it_index;
                it_any := evaluate_node(script, it);
                if !!script.error  return Null_Any;

                for_loop.it_decl.value_pointer = it_any.value_pointer;
                evaluate_node(script, for_loop.statement);
                if !!script.error  return Null_Any;
            }
        }
        
        
      case;
        set_general_error(script, "Unhandled node type '%' in evaluate_node.", node.node_type);
    }
    
    // copy result to hint_storage if need be
    // this may occur for certain small values where we just set result from another Any
    // doing it this way just prevents us having to do the memcpy in each individual case
    if hint_storage != null && result.value_pointer != hint_storage {
        memcpy(hint_storage, result.value_pointer, result.type.runtime_size);
    }
    return result;
}


