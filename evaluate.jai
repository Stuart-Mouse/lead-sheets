
evaluate_script :: (script: *Script) -> bool {
    if has_error(script) {
        log("Attempted to execute script with error. Error must be cleared before calling evaluate_script.");
        return false;
    }
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
    DISCARD_VALUE;
}

evaluate_node_or_return :: (script: *Script, node: *Node, provided_storage: *void = null, flags := Execution_Flags.NONE) -> Any #expand {
    // log("% -> %", node, as_type(node.value_type));
    
    value := script.my_evaluate_node(script, node, provided_storage, flags);
    if has_error(script)  `return NULL_ANY;
    
    if !(node.flags & .VALUE_TYPE_IS_NOMINAL) {
        assert(value.type == node.value_type, "%: % (%) claims to have value_type %, but it returned type %", get_location(node), node.node_type, node, as_type(node.value_type), as_type(value.type));
        
        if flags & .DISCARD_VALUE {
            dprint("Discarding unused result.");
            pop_any(*script.stack, node.value_type);
            return VOID_ANY;
        }
    }
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
    dprint("Evaluating % @ %", node.node_type, node);
    dprint_push_indent();
    
    if !node.value_type  dprint("Warning: node's value_type is null!");
    
    if !node.flags & .TYPECHECKED {
        set_execution_error(script, "Cannot execute a node that has not been typechecked.", node = node);
        return NULL_ANY;
    }
    
    return_result :: (node: *Node, any: Any) -> bool #expand {
        if provided_storage {
            dprint("Returning result using provided storage @ %: ", provided_storage, newline = false);
            if !is_aggregate(any.type)  dprint("%", any);
            dprint(" (%)", as_type(any.type));
            
            memcpy(provided_storage, any.value_pointer, any.type.runtime_size);
            `return Any.{ node.value_type, provided_storage };
        }
        ok, result := push_any(*script.stack, any, should_push_by_pointer(node));
        if !ok  set_execution_error(script, "Ran out of space on stack!", node = null);
        
        dprint("Returning result using stack @ %: ", result.value_pointer, newline = false);
        if !is_aggregate(any.type)  dprint("%", any);
        dprint(" (%)", as_type(any.type));
        `return result;
    }
    
    get_result_storage :: (node: *Node) -> *void #expand {
        if provided_storage {
            dprint("Got provided storage @ % for result of type: %", provided_storage, as_type(node.value_type));
            return provided_storage;
        }
        
        result := push(*script.stack, null, node.value_type.runtime_size);
        if !result {
            set_execution_error(script, "Ran out of space on stack!", node = null);
            `return NULL_ANY;
        }
        dprint("Got space on stack @ % for result of type: %", result, as_type(node.value_type));
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
        dprint("Identifier name: %", get_identifier_name(script, identifier));
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
            return_result(identifier, value);
            
          case .EXTERNAL_PROCEDURE;
            assert(!should_push_by_pointer(identifier));
            return_result(identifier, to_any(*script.procedures[identifier.index]));
            
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
            dprint("Evaluating overloaded operator...");
            return script.my_evaluate_node(script, operation.overload_procedure, provided_storage = provided_storage);
        }
        
        // TODO: special case for assignment to an Any. We should check that left and right types match.
        // special case: operator = is always a simple memcpy
        if operation.name == "=" {
            dprint("Operation is simple assignment.");
            assert(should_push_by_pointer(operation.left));
            evaluate_node_or_return(script, operation.left);
            value_pointer := pop_as(*script.stack, *void);
            script.my_evaluate_node(script, operation.right, provided_storage = value_pointer);
            return VOID_ANY; // NOTE: all assignments return void
        }
        
        _operator := get_operator(script, operation);
        dprint("Operator name: %, index: %, kind: %", operation.name, operation.operator_index, _operator.kind);
        
        result_value_pointer: *void;
        if _operator.kind != .ASSIGNMENT {
            result_value_pointer = get_result_storage(operation);
        }
        defer_restore(*script.stack.top);
        
        right: Any;
        left := evaluate_node_or_return(script, operation.left);
        if !is_unary(_operator.kind) {
            right = evaluate_node_or_return(script, operation.right);
        }
        
        if node.flags & .SWAP_OPERANDS {
            dprint("Swapping operands...");
            left, right = right, left;
        }
        
        dprint("left: %, right: %, result pointer: %", left, right, result_value_pointer);
        if !execute_builtin_operation(operation.builtin_operation_index, left.value_pointer, right.value_pointer, result_value_pointer)  return NULL_ANY;
        
        return Any.{ operation.value_type, result_value_pointer };
        
        
      case Node_Procedure_Call;
        procedure_call := node.(*Node_Procedure_Call);
        
        return_value_pointer := get_result_storage(procedure_call);
        defer_restore(*script.stack.top);
        
        dprint("return_value_pointer: %", return_value_pointer);
        
        procedure_info := procedure_call.procedure_expression.value_type.(*Type_Info_Procedure);
        assert(procedure_info.type == .PROCEDURE);
        dprint("procedure_info: %", as_type(procedure_info));
        
        procedure := evaluate_node_or_return(script, procedure_call.procedure_expression);
        
        // TODO: in the future, we should probably just use *void instead of Any for arguments array, only using Any in debug mode
        ARGUMENTS_ARRAY_ELEMENT_TYPE :: Any;
        
        push_ok, arguments := push_empty_array(*script.stack, ARGUMENTS_ARRAY_ELEMENT_TYPE, procedure_info.argument_types.count);
        if !push_ok {
            set_execution_error(script, type = .STACK_OVERFLOW, node = node);
            return NULL_ANY;
        }
        
        assert(procedure_call.arguments.count == procedure_info.argument_types.count);
        for procedure_call.arguments {
            arguments[it_index] = evaluate_node_or_return(script, it);
            
            if arguments[it_index].type.runtime_size > 8
                then dprint("argument #%, % @ %",    it_index, as_type(arguments[it_index].type), arguments[it_index].value_pointer);
                else dprint("argument #%, % @ %: %", it_index, as_type(arguments[it_index].type), arguments[it_index].value_pointer, arguments[it_index]);
        }
        
        result := Any.{ procedure_call.value_type, return_value_pointer };
        result_array: [] Any; 
        if result.type != xx void then result_array = .[ result ];
        
        #if USING_DYNCALL {
            if !do_dyncall(script.dyncall_vm, procedure, arguments, result_array) {
                set_general_error(script, "Failed while trying to make dyncall.");
                return NULL_ANY;
            }
        } else {
            procedure_pointer := procedure.value_pointer.(**void).*;
            if !try_calling_procedure_with_wrapper(procedure_info, procedure_pointer, arguments, result_array) {
                set_general_error(script, "Unable to find wrapper for procedure of type: %", as_type(procedure_info));
                return NULL_ANY;
            }
        }
        
        result_any := Any.{ procedure_call.value_type, return_value_pointer };
        return result_any;
        
        
      case Node_Cast;
        node_cast := node.(*Node_Cast);
        
        // TODO: special case to handle conversion from Any.
        
        src_type := node_cast.value.value_type;
        dst_type := node_cast.value_type;

        dst := Any.{ dst_type, get_result_storage(node_cast) };
        
        // special case for implicitly taking a reference
        if dst_type.type == .POINTER 
        && dst_type.(*Type_Info_Pointer).pointer_to == src_type {
            assert(should_push_by_pointer(node_cast.value));
            assert(!should_push_by_pointer(node_cast));
            
            dprint("Implicitly referencing: % -> %", as_type(node_cast.value.value_type), as_type(node_cast.value_type));
            
            src := evaluate_node_or_return(script, node_cast.value);
            dst.value_pointer.(**void).* = src.value_pointer;
            return dst;
        }
        
        // NOTE: no conversions to or from aggregates (other than implicitly taking references) are currently supported
        assert(!is_aggregate(node_cast.value_type) && !is_aggregate(node_cast.value.value_type));
        
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
        doing_implict_dereference := false;
        
        if left_type.type == {
          case .POINTER;
            assert(!should_push_by_pointer(dot.left));
            
            pointer_info := left_type.(*Type_Info_Pointer);
            assert(pointer_info.pointer_to.type == .STRUCT);
            
            dprint("Implicitly dereferencing: % -> %", as_type(left_type), as_type(pointer_info.pointer_to));
            left_type = pointer_info.pointer_to;
            doing_implict_dereference = true;
            #through;
            
          case .STRUCT;
            if should_push_by_pointer(dot.left) || doing_implict_dereference {
                evaluate_node_or_return(script, dot.left);
                struct_pointer := pop_as(*script.stack, *void);
                member_any := Any.{ dot.value_type, struct_pointer + dot.right.(*Node_Identifier).member.offset_in_bytes };
                return_result(dot, member_any);
            }
            
            dprint("%: LHS is pushed by value.", get_location(dot));
            
            // NOTE: We may later find that this case is problematic and results in stack corruption issues!
            //       So if something like that happens I hope I can identify it and remember to look here!
            //       If the LHS struct value could not be pushed by pointer, then that means it's on the stack.
            //       And if it's on the stack, we need to be careful about not overwriting it.
            //       In the scope of this one node, we can do that pretty easily by pushing the result value (RHS) before pushing the LHS,
            //       but it's possible there's convoluted cases I have not yet recognized in which other nodes further up the tree
            //       may accidentally push some large value that overwrites the LHS we pushed here.
            result := Any.{ node.value_type, get_result_storage(node) };
            
            evaluate_node_or_return(script, dot.left);
            struct_pointer := pop(*script.stack, left_type.runtime_size);
            
            member_any := Any.{ dot.value_type, struct_pointer + dot.right.(*Node_Identifier).member.offset_in_bytes };
            
            if should_push_by_pointer(dot) {
                memcpy(result.value_pointer, *member_any.value_pointer, size_of(*void));
            } else {
                memcpy(result.value_pointer, member_any.value_pointer, member_any.type.runtime_size);
            }
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
            // NOTE: initialize_any may be a somewhat nontrivial operation if we have some complex value type
            value := Any.{ declaration.value_type, value_pointer };
            initialize_any(value);
        }
        return VOID_ANY;
        
        
      case Node_Block;
        block := node.(*Node_Block);
        
        // NOTE: we don't execute named blocks unless explicitly called
        if block.name && !(flags & .EXPLICIT_CALL)  return VOID_ANY;
        
        is_stack_frame := is_stack_frame_block(script, block);
        if is_stack_frame {
            if !push_frame(*script.stack, block.stack_frame_size) {
                set_execution_error(script, type = .STACK_OVERFLOW, node = node);
                return NULL_ANY;
            }
        }
        for block.statements {
            stack_top_before := script.stack.top;
            defer assert(script.stack.top == stack_top_before, "Invalid stack use on statement at %.", get_location(it));
            
            evaluate_node_or_return(script, it, null, .DISCARD_VALUE);
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
        return VOID_ANY;
        
        
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
            return VOID_ANY;
            
          case .RANGE;
            evaluate_node_or_return(script, for_loop.range.lower);
            evaluate_node_or_return(script, for_loop.range.upper);
            
            control_type := for_loop.range.lower.value_type;
            upper := pop_any(*script.stack, control_type);
            lower := pop_any(*script.stack, control_type);
            
            // NOTE: for now, we are just requiring that the control type is actually int
            //       the bounds will still be implicitly cast to int if needed.
            assert(control_type == xx int);
            upper_as_s64 := any_isa(upper, int).*;
            lower_as_s64 := any_isa(lower, int).*;
            
            for lower_as_s64..upper_as_s64 {
                for_loop.it_decl.value_pointer = *it;
                evaluate_node_or_return(script, for_loop.statement);
            }
            return VOID_ANY;
            
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

