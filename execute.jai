

/*
    TODO: Cleaning up execute.jai, removing evaluate.jai
    
    need to implement callback like we have for evaluation
        user callbacks will now need to push to script stack rather than return an Any
        this means the user needs to be a bit more aware of lvalue/rvalue semantics, since they will need to know when to push indirectly
        maybe we can aid in this by passing up .EXPECT_LVALUE in the execution callback, so at least we make the user aware of what is expected
    
    TODO: We should probably just let the user entirely override the evaluate_node with my_evaluate_node
          That way, they can just call the standard evaluate_node as their default case.
*/

execute_script :: (script: *Script) -> bool {
    if !(script.flags & .PARSED) {
        set_execution_error(script, "Cannot execute script before AST has been constructed.\n", node = null);
        return false;
    }
    if !(script.flags & .TYPECHECKED) {
        set_execution_error(script, "Cannot execute script before AST has been typechecked.\n", node = null);
        return false;
    }
    
    reset_stack(*script.stack);
    script.my_evaluate_node(script, script.ast_root);
    return !has_error(script);
}

evaluate_statement :: (script: *Script, expr: string, store_global_declarations := false) -> (Any, bool) {
    root := parse_statement_string(script, expr);
    if root == null {
        log("root was null");
        return NULL_ANY, false;
    }
    
    if !typecheck_node(script, root, .IS_STATEMENT_ROOT)  return NULL_ANY, false;
    
    value := New_Any(root.value_type, initialized = false,, get_pool_allocator(*script.pool));
    script.my_evaluate_node(script, root, provided_storage = value.value_pointer);
    if has_error(script)  return NULL_ANY, false;
    
    if store_global_declarations {
        if root.node_type == Node_Declaration {
            array_add(*script.global_declarations, xx root);
        }
    }
    
    return value, true;
}

Execution_Flags :: enum_flags {
    NONE :: 0;
    EXPLICIT_CALL :: 1;  // used to indicate that we ought to run a named block
}

evaluate_node_or_return :: (script: *Script, node: *Node, provided_storage: *void = null, flags := Execution_Flags.NONE) -> Any #expand {
    value := script.my_evaluate_node(script, node, provided_storage, flags);
    if has_error(script)  `return NULL_ANY;
    return value;
}

/*
    When executing a node, we need to handle two different ways in which the result may be returned.
    If there is provided_storage for the result, then we write the result directly to that memory location.
    Otherwise, we push the result value (or a pointer to the result value, depending on should_push_by_pointer) to the stack.
    
    The contract for executing nodes is this:
        If script.my_evaluate_node returns false
            The script error will be set with some valid error.
            This error should be propogated back up to the user.
        If script.my_evaluate_node returns true
            If provided_storage is not null
                The result value must be written to the provided_storage.
                script.stack.top must have the same value as script.my_evaluate_node was called.
            If provided_storage is null
                If should_push_by_pointer(node)
                    A pointer to the result value must be pushed to top of the stack
                If !should_push_by_pointer(node)
                    The result value must be pushed to top of the stacks
*/
evaluate_node :: (script: *Script, node: *Node, provided_storage: *void = null, flags := Execution_Flags.NONE) -> Any {
    dprint("executing node %: %", node, node.node_type);
    dprint_push_indent();
    
    dprint("stack_pointer offset: %  (%)", script.stack.top - script.stack.memory, script.stack.top);
    if provided_storage  dprint("provided storage is %", provided_storage);
    if should_push_by_pointer(node)  dprint("expecting lvalue");
    
    if !node.value_type { dprint("Warning: node has no set value type!"); }
    
    if !node.flags & .TYPECHECKED {
        set_execution_error(script, "Cannot execute a node that has not been typechecked.", node = node);
        return NULL_ANY;
    }
    
    return_result :: (node: *Node, any: Any) -> bool #expand {
        if provided_storage {
            memcpy(provided_storage, any.value_pointer, any.type.runtime_size);
            `return Any.{ node.value_type, provided_storage };
        }
        
        ok, result := push_any(*script.stack, any, should_push_by_pointer(node));
        if !ok  set_execution_error(script, "Ran out of space on stack!", node = null);
        
        dprint("push_any(%, %)", as_type(any.type), should_push_by_pointer(node));
        if should_push_by_pointer(node)  dprint("pushed ptr value: %", any.value_pointer);
        if is_aggregate(result.type) {
            dprint("result: @ %", result.value_pointer);
        } else {
            dprint("result: %", result);
        }
        
        `return result;
    }
    
    get_result_storage :: (node: *Node) -> *void #expand {
        if provided_storage  return provided_storage;
        
        result := push(*script.stack, null, node.value_type.runtime_size);
        if !result {
            set_execution_error(script, "Ran out of space on stack!", node = null);
            `return NULL_ANY;
        }
        return result;
    }
    
    if node.node_type == {
      case Node_Directive;
        directive := node.(*Node_Directive);
        if directive.runtime_node {
            return script.my_evaluate_node(script, directive.runtime_node);
        }
        return VOID_ANY;
        
        
      case Node_Literal;
        literal := node.(*Node_Literal);
        if should_push_by_pointer(literal) {
            assert(literal.flags & .IS_MALLEABLE == .IS_MALLEABLE || literal.literal_type == .ANY);
        }
        
        if #complete literal.literal_type == {
          case .ANY;
            return_result(literal, literal.any);
            
          case .STRING;
            return_result(literal, literal.text);
            
          case .BOOLEAN; #through;
          case .NUMBER;
            return_result(literal, to_any(*literal.number));
            
          case .STRUCT;
            value_pointer: *void;
            if literal.aggr.value_pointer
                then value_pointer = literal.aggr.value_pointer;
                else value_pointer = get_result_storage(literal);
            
            write_pointer := value_pointer;
            for literal.aggr.expressions {
                evaluate_node_or_return(script, it, provided_storage = write_pointer);
                write_pointer += it.value_type.runtime_size;
            }
            
            if literal.aggr.value_pointer {
                return_result(literal, Any.{ literal.value_type, literal.aggr.value_pointer });
            }
            return Any.{ literal.value_type, value_pointer };
        }
        assert(false, "Invalid literal type in script.my_evaluate_node.");
        
        
      case Node_Identifier;
        identifier := node.(*Node_Identifier);
        dprint("name: %", get_identifier_name(script, identifier));
        if #complete identifier.identifier_type == {
          case .DECLARATION;
            declaration := identifier.declaration;
            if declaration.flags & .MACRO {
                // return script.my_evaluate_node(script, declaration.init_expression);
            }
            
            variable_any := Any.{ declaration.value_type, get_value_pointer(script, declaration) };
            return_result(identifier, variable_any);
            
          case .EXTERNAL_VARIABLE;
            variable := *script.variables[identifier.index];
            value    := get_value(variable);
            
            if variable.value_pointer == null {
                set_execution_error(script, "Unable to get value of external variable '%'.", variable.name, node = node);
                return NULL_ANY;
            }
            
            if variable.type.runtime_size > 8
                then dprint("returning external variable at %", variable.value_pointer);
                else dprint("returning external variable with value % at %", value, variable.value_pointer);
            
            return_result(identifier, value);
            
          case .EXTERNAL_PROCEDURE;
            assert(!should_push_by_pointer(identifier));
            return_result(identifier, script.procedures[identifier.index].pointer);
            
          case .LITERAL;
            return script.my_evaluate_node(script, identifier.literal, provided_storage = provided_storage);
            
          case .TYPE;
            assert(!should_push_by_pointer(identifier));
            return_result(identifier, script.type_table[identifier.index].type);
            
            // identifier types that should never be executed:
          case .UNRESOLVED;
          case .STRUCT_MEMBER;
        }
        assert(false, "Invalid identifier type in script.my_evaluate_node: %", identifier.identifier_type);
        
        
      case Node_Operation;
        operation := node.(*Node_Operation);
        
        if operation.flags & .OVERLOAD {
            return script.my_evaluate_node(script, operation.overload_procedure, provided_storage = provided_storage);
        }
        
        // TODO: special case for assignment to an Any. We should check that left and right types match.
        // special case: operator = is always a simple memcpy
        if operation.name == "=" {
            assert(should_push_by_pointer(operation.left));
            evaluate_node_or_return(script, operation.left);
            value_pointer := pop_as(*script.stack, *void);
            return script.my_evaluate_node(script, operation.right, provided_storage = value_pointer);
        }
        
        _operator := get_operator(script, operation);
        
        result_value_pointer: *void;
        if _operator.kind != .ASSIGNMENT {
            result_value_pointer = get_result_storage(operation);
        }
        defer_restore(*script.stack.top);
        
        right: Any;
        left := evaluate_node_or_return(script, operation.left);
        dprint("left: %, %", as_type(left.type), left.value_pointer);
        
        if !is_unary(_operator.kind) {
            right = evaluate_node_or_return(script, operation.right);
        }
        dprint("left 2: %, %", as_type(left.type), left.value_pointer);
        
        dprint("operation name: %, index: %, kind: %", operation.name, operation.operator_index, _operator.kind);
        
        if node.flags & .SWAP_OPERANDS {
            dprint("swapping operands");
            left, right = right, left;
        }
        
        dprint("left: %, right: %, result pointer: %", left, right, result_value_pointer);
        if !execute_builtin_operation(operation.builtin_operation_index, left.value_pointer, right.value_pointer, result_value_pointer)  return NULL_ANY;
        
        if _operator.kind == .ASSIGNMENT {
            assert(operation.value_type == xx void);
            return VOID_ANY;
        }
        
        result_any := Any.{ operation.value_type, result_value_pointer };
        return result_any;
        
        
      case Node_Procedure_Call;
        procedure_call := node.(*Node_Procedure_Call);
        
        return_value_pointer := get_result_storage(procedure_call);
        {
            defer_restore(*script.stack.top);
            
            dprint("return_value_pointer: %", return_value_pointer);
            
            procedure_info := procedure_call.procedure_expression.value_type.(*Type_Info_Procedure);
            assert(procedure_info.type == .PROCEDURE);
            dprint("procedure_info: %", as_type(procedure_info));
            
            procedure_pointer := any_isa(evaluate_node_or_return(script, procedure_call.procedure_expression), *void).*;
            
            {
                // TODO: in the future, we should probably just use *void instead of Any for arguments array, only using Any in debug mode
                ARGUMENTS_ARRAY_ELEMENT_TYPE :: Any;
                
                push_ok, arguments := push_empty_array(*script.stack, ARGUMENTS_ARRAY_ELEMENT_TYPE, procedure_info.argument_types.count);
                if !push_ok {
                    set_execution_error(script, type = .STACK_OVERFLOW, node = node);
                    return NULL_ANY;
                }
                
                assert(procedure_call.arguments.count == procedure_info.argument_types.count);
                for procedure_info.argument_types {
                    argument_node := procedure_call.arguments[it_index];
                    argument_any  := evaluate_node_or_return(script, argument_node);
                    
                    assert(argument_node.value_type == it);
                    arguments[it_index] = argument_any;
                    
                    if argument_any.type.runtime_size > 8
                        then dprint("argument %, % @ %", it_index, as_type(argument_any.type), argument_any.value_pointer);
                        else dprint("argument %, % @ %: %", it_index, as_type(argument_any.type), argument_any.value_pointer, argument_any);
                }
                
                result := Any.{ procedure_call.value_type, return_value_pointer };
                result_array: [] Any; 
                if result.type != xx void then result_array = .[ result ];
                
                #if USING_DYNCALL {
                    if !do_dyncall(script.dyncall_vm, Any.{ procedure_info, *procedure_pointer }, arguments, result_array) {
                        set_general_error(script, "Failed while trying to make dyncall.");
                        return NULL_ANY;
                    }
                } else {
                    if !try_calling_procedure_with_wrapper(procedure_info, procedure_pointer, arguments, result_array) {
                        set_general_error(script, "Unable to find wrapper for procedure of type: %", as_type(procedure_info));
                        return NULL_ANY;
                    }
                }
            }
        }
        
        // TODO: handle in a more systematic way, for other types of expressions that may be used as a statement.
        if procedure_call.flags & .DISCARD_VALUE {
            dprint("Discarding unused result of procedure call expression.");
            assert(provided_storage == null);
            pop_any(*script.stack, procedure_call.value_type);
            return VOID_ANY;
        }
        
        result_any := Any.{ procedure_call.value_type, return_value_pointer };
        return result_any;
        
        
      case Node_Cast;
        node_cast := node.(*Node_Cast);
        
        // TODO: special case to handle conversion from Any.
        
        src_type := node_cast.value.value_type;
        dst_type := node_cast.value_type;
        
        // special case for implicitly taking a reference
        if dst_type.type == .POINTER 
        && dst_type.(*Type_Info_Pointer).pointer_to == src_type {
            assert(should_push_by_pointer(node_cast.value));
            assert(!should_push_by_pointer(node_cast));
            
            dprint("implicit dereference from % to %", as_type(node_cast.value.value_type), as_type(node_cast.value_type));
            
            // NOTE: We use peek_any to reinterpret the indirectly pushed value as a pointer to that value. 
            //       The only reason we have to do that here is so that the returned Any matches the type the caller is expecting.
            evaluate_node_or_return(script, node_cast.value);
            return peek_any(*script.stack, node.value_type);
        }
        
        // NOTE: no conversions to or from aggregates (other than implicitly taking references) are currently supported
        assert(!is_aggregate(node_cast.value_type) && !is_aggregate(node_cast.value.value_type));
        
        dst := Any.{ node.value_type, get_result_storage(node_cast) };
        
        evaluate_node_or_return(script, node_cast.value);
        src := pop_any(*script.stack, node_cast.value.value_type);
        
        if !Convert.any_to_any(dst, src) {
            set_general_error(script, "Failed to execute cast from % to %. Source value was %", as_type(src.type), as_type(dst.type), src);
            return NULL_ANY;
        }
        return dst;
        
        
      case Node_Dot;
        dot := node.(*Node_Dot);
        
        if dot.flags & .IS_ARROW {
            assert(false, "Arrow is currently unimplemented. This should not have passed typechecking.");
            // return script.my_evaluate_node(script, dot.right);
        }
        
        if dot.left == null {
            assert(node.value_type.type == .ENUM || node.value_type.type == .STRUCT);
            return script.my_evaluate_node(script, dot.right, provided_storage = provided_storage);
        }
        
        left_type := dot.left.value_type;
        if left_type.type == {
          case .POINTER;
            pointer_info := left_type.(*Type_Info_Pointer);
            assert(pointer_info.pointer_to.type == .STRUCT);
            // override left type for struct case so that we we pop the correct type
            dprint("override left_type: % -> %", as_type(left_type), as_type(pointer_info.pointer_to));
            left_type = pointer_info.pointer_to;
            
            evaluate_node_or_return(script, dot.left);
            
            struct_any, did_dereference := dereference_any_pointer(pop_any(*script.stack, pointer_info, false));
            assert(did_dereference);
            member_any := Any.{ dot.value_type, struct_any.value_pointer + dot.right.(*Node_Identifier).member.offset_in_bytes };
            
            dprint("struct value ptr: %", struct_any.value_pointer);
            dprint("member value ptr: %", member_any.value_pointer);
            
            return_result(dot, member_any);
            
          case .STRUCT;
            evaluate_node_or_return(script, dot.left);
            struct_any := pop_any(*script.stack, left_type, true); // NOTE: always pop inirectly here
            member_any := Any.{ dot.value_type, struct_any.value_pointer + dot.right.(*Node_Identifier).member.offset_in_bytes };
            
            dprint("struct value ptr: %", struct_any.value_pointer);
            dprint("member value ptr: %", member_any.value_pointer);
            
            return_result(dot, member_any);
            
          case .TYPE;
            // NOTE: the type expression on LHS was evaluated during typechecking
            return script.my_evaluate_node(script, dot.right, provided_storage = provided_storage);
        }
        
        assert(false, "Invalid Node_Dot: %", dot.*);
        
        
      case Node_Subscript;
        subscript := node.(*Node_Subscript);
        
        evaluate_node_or_return(script, subscript.base_expression);
        evaluate_node_or_return(script, subscript.indexing_expression);
        
        index_any := pop_any(*script.stack, subscript.indexing_expression.value_type);
        base_any  := pop_any(*script.stack, subscript.base_expression.value_type, true);
        
        ti_array := subscript.base_expression.value_type.(*Type_Info_Array);
        indexing_type := subscript.indexing_expression.value_type;
        
        index_as_s64: s64;
        assert(Convert.any_to_int(index_as_s64, index_any));
        
        array_count, array_data := get_array_count_and_data(base_any.value_pointer, xx base_any.type);
        
        if index_as_s64 < 0 || index_as_s64 >= array_count {
            set_execution_error(script, "Array index % was out of bounds! Max index is %.", index_as_s64, array_count-1, node = node);
            return NULL_ANY;
        }
        
        element := Any.{ node.value_type, array_data + index_as_s64 * node.value_type.runtime_size };
        
        if element.type.runtime_size > 8
            then dprint("returning result of indexing operation at %", element.value_pointer);
            else dprint("returning result of indexing operation with value % at %", element, element.value_pointer);
        
        return_result(node, element);
        
        
      case Node_Declaration;
        declaration := node.(*Node_Declaration);
        
        // early return on macro for now so that we don't overwrite value of malleable literal
        // the initial value is evaluated in typechecking phase, which is fine since for now we only allow simple literals in a macro decl
        if declaration.flags & .MACRO  return VOID_ANY;
        
        dprint("declaration name: %", get_declaration_name(script, declaration));
        value_pointer := get_value_pointer(script, declaration);
        
        if declaration.init_expression {
            evaluate_node_or_return(script, declaration.init_expression, provided_storage = value_pointer);
        } else {
            // TODO: use initializer_of for aggregate types?
            memset(value_pointer, 0, declaration.value_type.runtime_size);
        }
        return VOID_ANY;
        
        
      case Node_Block;
        block := node.(*Node_Block);
        
        // NOTE: we don't execute named blocks unless explicitly called
        if block.name && (flags & .EXPLICIT_CALL)  return VOID_ANY;
        
        // for debugging purposes
        stack_top_before := script.stack.top;
        defer assert(script.stack.top == stack_top_before);
        
        is_stack_frame := is_stack_frame_block(script, block);
        if is_stack_frame {
            if !push_frame(*script.stack, block.stack_frame_size) {
                set_execution_error(script, type = .STACK_OVERFLOW, node = node);
                return NULL_ANY;
            }
        }
        for block.statements {
            stack_top_before := script.stack.top;
            defer assert(script.stack.top == stack_top_before, sprint_node(script, it));
            
            evaluate_node_or_return(script, it);
        }
        if is_stack_frame {
            pop_frame(*script.stack);
        }
        return VOID_ANY;
        
        
      case Node_If_Statement;
        if_statement := node.(*Node_If_Statement);
        evaluate_node_or_return(script, if_statement.condition);
        
        condition := pop_any(*script.stack, if_statement.condition.value_type);
        dprint("condition: % %", as_type(condition.type), condition);
        
        condition_as_bool: bool;
        Convert.any_to_bool(condition_as_bool, condition);
        if condition_as_bool {
            evaluate_node_or_return(script, if_statement.statement);
        }
        return true;
        
        
      case Node_While_Loop;
        while_loop := node.(*Node_While_Loop);
        while loop := true {
            evaluate_node_or_return(script, while_loop.condition);
            
            condition := pop_any(*script.stack, while_loop.condition.value_type);
            dprint("condition: % %", as_type(condition.type), condition);
            
            condition_as_bool: bool;
            Convert.any_to_bool(condition_as_bool, condition);
            if !condition_as_bool  break loop;
            
            evaluate_node_or_return(script, while_loop.statement);
        }
        return VOID_ANY;
        
        
      case Node_For_Loop;
        for_loop := node.(*Node_For_Loop);
        if for_loop.control_type == {
          case .ARRAY;
            array_info := for_loop.array_expression.value_type.(*Type_Info_Array);
            evaluate_node_or_return(script, for_loop.array_expression);
            array := pop_any(*script.stack, array_info, true); // TODO: maybe not use lvalue later?
            
            array_count, array_data := get_array_count_and_data(array.value_pointer, xx array.type);
            
            // these will be accessed by iterator and iterator_index nodes during iteration
            it_index: int;
            for_loop.it_decl.value_pointer       = array_data;
            for_loop.it_index_decl.value_pointer = *it_index;
            
            while it_index < array_count {
                evaluate_node_or_return(script, for_loop.statement);
                for_loop.it_decl.value_pointer += array_info.element_type.runtime_size;
                it_index += 1;
            }
            return true;
            
          case .RANGE;
            evaluate_node_or_return(script, for_loop.range.lower);
            evaluate_node_or_return(script, for_loop.range.upper);
            
            control_type := for_loop.range.lower.value_type;
            upper := pop_any(*script.stack, control_type);
            lower := pop_any(*script.stack, control_type);
            
            upper_as_s64: int;
            if !Convert.any_to_int(upper_as_s64, upper) {
                set_execution_error(script, "Failed to convert upper bound of for loop iteration range to s64. Source value was % of type %", upper, as_type(upper.type), node = for_loop.range.upper);
                return NULL_ANY;
            }
            
            lower_as_s64: int;
            if !Convert.any_to_int(lower_as_s64, lower) {
                set_execution_error(script, "Failed to convert lower bound of for loop iteration range to s64. Source value was % of type %", lower, as_type(lower.type), node = for_loop.range.lower);
                return NULL_ANY;
            }
            
            for lower_as_s64..upper_as_s64 {
                _it: Number_Union;
                Convert.any_to_any(Any.{ control_type, *_it }, it);
                
                for_loop.it_decl.value_pointer = *_it;
                // TODO: for now we just don't set it_index. 
                // we should probably prevent using it_index for range-based loops in typecheck 
                
                evaluate_node_or_return(script, for_loop.statement);
            }
            return true;
            
          case .LIST;
            _it_index: int;
            for_loop.it_index_decl.value_pointer = *_it_index;
            for for_loop.list {
                _it_index = it_index;
                evaluate_node_or_return(script, it);
                for_loop.it_decl.value_pointer = pop_any(*script.stack, for_loop.it_decl.value_type, true).value_pointer;
                evaluate_node_or_return(script, for_loop.statement);
            }
            return VOID_ANY;
        }
        
        assert(false, "Invalid control expression in for loop.");
    }
    
    assert(false, "Unhandled node type in script.my_evaluate_node: %", node.node_type);
    return NULL_ANY;
}

