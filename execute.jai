

/*
    TODO: Cleaning up execute.jai, removing evaluate.jai
    
    need to implement callback like we have for evaluation
        user callbacks will now need to push to script stack rather than return an Any
        this means the user needs to be a bit more aware of lvalue/rvalue semantics, since they will need to know when to push indirectly
        maybe we can aid in this by passing up .EXPECT_LVALUE in the execution callback, so at least we make the user aware of what is expected
        
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
    return execute_node(script, script.ast_root);
}

Execution_Flags :: enum_flags {
    NONE :: 0;
    EXPLICIT_CALL :: 1;  // used to indicate that we ought to run a named block
}

/*
    When executing a node, we need to handle two different ways in which the result may be returned.
    If there is provided_storage for the result, then we write the result directly to that memory location.
    Otherwise, we push the result value (or a pointer to the result value, depending on should_push_by_pointer) to the stack.
    
    The contract for executing nodes is this:
        If execute_node returns false
            The script error will be set with some valid error.
            This error should be propogated back up to the user.
        If execute_node returns true
            If provided_storage is not null
                The result value must be written to the provided_storage.
                script.stack.top must have the same value as execute_node was called.
            If provided_storage is null
                If should_push_by_pointer(node)
                    A pointer to the result value must be pushed to top of the stack
                If !should_push_by_pointer(node)
                    The result value must be pushed to top of the stacks
*/
execute_node :: (script: *Script, node: *Node, provided_storage: *void = null, flags := Execution_Flags.NONE) -> bool {
    dprint("executing node %: %", node, node.node_type);
    dprint_push_indent();
    
    dprint("stack_pointer offset: %  (%)", script.stack.top - script.stack.memory, script.stack.top);
    if should_push_by_pointer(node)  dprint("expecting lvalue");
    
    if !node.value_type { dprint("Warning: node has no set value type!"); }
    
    if !node.flags & .TYPECHECKED {
        set_execution_error(script, "Cannot execute a node that has not been typechecked.", node = node);
        return false;
    }
    
    push_result :: (node: *Node, any: Any) -> bool #expand {
        if provided_storage {
            memcpy(provided_storage, any.value_pointer, any.type.runtime_size);
            return true;
        }
        
        dprint("push_any(%, %)", as_type(any.type), should_push_by_pointer(node));
        if should_push_by_pointer(node)  dprint("pushed ptr value: %", any.value_pointer);
        
        result := push(*script.stack, any.value_pointer, any.type.runtime_size, should_push_by_pointer(node));
        if !result  set_execution_error(script, "Ran out of space on stack!", node = null);
        return result != null;
    }
    
    get_result_storage :: (node: *Node) -> *void #expand {
        if provided_storage  return provided_storage;
        
        result := push(*script.stack, null, node.value_type.runtime_size);
        if !result {
            set_execution_error(script, "Ran out of space on stack!", node = null);
            `return false;
        }
        return result;
    }
    
    if node.node_type == {
      case Node_Directive;
        directive := node.(*Node_Directive);
        if directive.runtime_node {
            return execute_node(script, directive.runtime_node);
        }
        return true;
        
        
      case Node_Literal;
        literal := node.(*Node_Literal);
        if should_push_by_pointer(literal) {
            assert(literal.flags & .IS_MALLEABLE == .IS_MALLEABLE || literal.literal_type == .ANY);
        }
        
        if #complete literal.literal_type == {
          case .ANY;
            return push_result(literal, literal.any);
            
          case .STRING;
            return push_result(literal, literal.text);
            
          case .BOOLEAN; #through;
          case .NUMBER;
            return push_result(literal, to_any(*literal.number));
            
          case .STRUCT;
            write_ptr: *void;
            if provided_storage 
                then write_ptr = get_result_storage(literal);
                else write_ptr = literal.aggr.value_pointer;
            
            for literal.aggr.expressions {
                if !execute_node(script, it, provided_storage = write_ptr)  return false;
                write_ptr += it.value_type.runtime_size;
            }
            
            if !provided_storage {
                if !push_result(literal, Any.{ literal.value_type, literal.aggr.value_pointer })  return false;
            }
            return true;
        }
        assert(false, "Invalid literal type in execute_node.");
        
        
      case Node_Identifier;
        identifier := node.(*Node_Identifier);
        dprint("name: %", get_identifier_name(script, identifier));
        if #complete identifier.identifier_type == {
          case .DECLARATION;
            declaration := identifier.declaration;
            if declaration.flags & .MACRO {
                // return execute_node(script, declaration.init_expression);
            }
            
            variable_any := Any.{ declaration.value_type, get_value_pointer(script, declaration) };
            dprint("returning internal variable with value % at %", variable_any, variable_any.value_pointer);
            return push_result(identifier, variable_any);
            
          case .EXTERNAL_VARIABLE;
            variable := *script.variables[identifier.index];
            value    := get_value(variable);
            
            if variable.value_pointer == null {
                set_execution_error(script, "Unable to get value of external variable '%'.", variable.name, node = node);
                return false;
            }
            
            if variable.type.runtime_size > 8
                then dprint("returning external variable at %", variable.value_pointer);
                else dprint("returning external variable with value % at %", value, variable.value_pointer);
            
            return push_result(identifier, value);
            
          case .EXTERNAL_PROCEDURE;
            assert(!should_push_by_pointer(identifier));
            return push_result(identifier, script.procedures[identifier.index].pointer);
            
          case .LITERAL;
            return execute_node(script, identifier.literal, provided_storage = provided_storage);
            
          case .TYPE;
            assert(!should_push_by_pointer(identifier));
            return push_result(identifier, script.type_table[identifier.index].type);
            
            // identifier types that should never be executed:
          case .UNRESOLVED;
          case .STRUCT_MEMBER;
        }
        assert(false, "Invalid identifier type in execute_node: %", identifier.identifier_type);
        
        
      case Node_Operation;
        operation := node.(*Node_Operation);
        
        if operation.flags & .OVERLOAD {
            return execute_node(script, operation.overload_procedure, provided_storage = provided_storage);
        }
        
        
        // special case: operator = is always a simple memcpy
        if operation.name == "=" {
            assert(should_push_by_pointer(operation.left));
            if !execute_node(script, operation.left)   return false;
            value_pointer := pop_as(*script.stack, *void);
            return execute_node(script, operation.right, provided_storage = value_pointer);
        }
        
        
        result_value_pointer: *void;
        
        _operator := get_operator(script, operation);
        if _operator.kind != .ASSIGNMENT {
            result_value_pointer = get_result_storage(operation);
        }
        
        if !execute_node(script, operation.left)  return false;
        if !is_unary(_operator.kind) {
            if !execute_node(script, operation.right)  return false;
        }
        
        dprint("operation name: %, index: %, kind: %", operation.name, operation.operator_index, _operator.kind);
        
        right: Any;
        if !is_unary(_operator.kind) {
            right = pop_any(*script.stack, operation.right.value_type, should_push_by_pointer(operation.right));
        }
        left := pop_any(*script.stack, operation.left.value_type, should_push_by_pointer(operation.left));
        
        if node.flags & .SWAP_OPERANDS {
            left, right = right, left;
        }
        
        dprint("left: %, right: %, result pointer: %", left, right, result_value_pointer);
        if !execute_builtin_operation(operation.builtin_operation_index, left.value_pointer, right.value_pointer, result_value_pointer)  return false;
        
        result_any := Any.{ operation.value_type, result_value_pointer };
        dprint("returning result of operation: % %", as_type(operation.value_type), result_any);
        return true;
        
        
      case Node_Procedure_Call;
        procedure_call := node.(*Node_Procedure_Call);
        
        return_value_pointer := get_result_storage(procedure_call);
        dprint("return_value_pointer: %", return_value_pointer);
        
        if !execute_node(script, procedure_call.procedure_expression)  return false;
        procedure_info := procedure_call.procedure_expression.value_type.(*Type_Info_Procedure);
        assert(procedure_info.type == .PROCEDURE);
        dprint("procedure_info: %", as_type(procedure_info));
        
        // we have to grab the actual procedure pointer value now before overwriting with another stack push below
        procedure_pointer := pop_as(*script.stack, *void);
        
        {
            // TODO: in the future, we should probably just use *void instead of Any for arguments array, only using Any in debug mode
            ARGUMENTS_ARRAY_ELEMENT_TYPE :: Any;
            
            push_ok, arguments := push_empty_array(*script.stack, ARGUMENTS_ARRAY_ELEMENT_TYPE, procedure_info.argument_types.count);
            if !push_ok {
                set_execution_error(script, type = .STACK_OVERFLOW, node = node);
                return false;
            }
            defer pop_array(*script.stack, ARGUMENTS_ARRAY_ELEMENT_TYPE);
            
            
            for procedure_call.arguments  if !execute_node(script, it)  return false;
            
            for < procedure_info.argument_types {
                argument_node := procedure_call.arguments[it_index];
                
                // NOTE: We provide an Any with the type from argument_node, which may not be the same as the formal argument type if we are doing an implicit reference.
                //       The actual implicit reference is currently handled in the call_procedure wrapper, which is not really ideal, but it's what I've got to do for the time being.
                argument_any := pop_any(*script.stack, it, should_push_by_pointer(argument_node));
                arguments[it_index] = argument_any;
                
                if argument_any.type.runtime_size > 8
                    then dprint("argument % @ %", it_index, argument_any.value_pointer);
                    else dprint("argument % @ %: %", it_index, argument_any.value_pointer, argument_any);
            }
            
            
            result := Any.{ procedure_call.value_type, return_value_pointer };
            result_array: [] Any; 
            if result.type != xx void then result_array = .[ result ];
            
            #if USING_DYNCALL {
                if !do_dyncall(script.dyncall_vm, Any.{ procedure_info, *procedure_pointer }, arguments, result_array) {
                    set_general_error(script, "Failed while trying to make dyncall.");
                    return false;
                }
            } else {
                if !try_calling_procedure_with_wrapper(procedure_info, procedure_pointer, arguments, result_array) {
                    set_general_error(script, "Unable to find wrapper for procedure of type: %", as_type(procedure_info));
                    return false;
                }
            }
        }
        
        dprint("result of procedure call: %", Any.{ procedure_call.value_type, return_value_pointer });
        
        // TODO: handle in a more systematic way, for other types of expressions that may be used as a statement.
        // NOTE: we are assuming here the DISCARD_VALUE being set and provided_storage being non-null are absolutely mutually exclusive 
        if procedure_call.flags & .DISCARD_VALUE {
            dprint("Discarding unused result of procedure call expression.");
            pop_any(*script.stack, procedure_call.value_type);
        }
        return true;
        
        
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
            
            // Actually do nothing, but caller will pop the pointer as rvalue instead of as lvalue now
            return execute_node(script, node_cast.value);
        }
        
        // NOTE: no conversions to or from aggregates (other than implicitly taking references) are currently supported
        assert(!is_aggregate(node_cast.value_type) && !is_aggregate(node_cast.value.value_type));
        
        dst := Any.{ node.value_type, get_result_storage(node_cast) };
        
        if !execute_node(script, node_cast.value)  return false;
        src := pop_any(*script.stack, node_cast.value.value_type);
        
        if !Convert.any_to_any(dst, src) {
            set_general_error(script, "Failed to execute cast from % to %. Source value was %", as_type(src.type), as_type(dst.type), src);
            return false;
        }
        
        return true;
        
        
      case Node_Dot;
        dot := node.(*Node_Dot);
        
        if dot.flags & .IS_ARROW {
            assert(false, "Arrow is currently unimplemented. This should not have passed typechecking.");
            // return execute_node(script, dot.right);
        }
        
        if dot.left == null {
            assert(node.value_type.type == .ENUM || node.value_type.type == .STRUCT);
            return execute_node(script, dot.right, provided_storage = provided_storage);
        }
        
        left_type := dot.left.value_type;
        if left_type.type == {
          case .POINTER;
            pointer_info := left_type.(*Type_Info_Pointer);
            assert(pointer_info.pointer_to.type == .STRUCT);
            // override left type for struct case so that we we pop the correct type
            dprint("override left_type: % -> %", as_type(left_type), as_type(pointer_info.pointer_to));
            left_type = pointer_info.pointer_to;
            
            if !execute_node(script, dot.left)  return false;
            
            struct_any, did_dereference := dereference_any_pointer(pop_any(*script.stack, pointer_info, false));
            assert(did_dereference);
            member_any := Any.{ dot.value_type, struct_any.value_pointer + dot.right.(*Node_Identifier).member.offset_in_bytes };
            
            dprint("struct value ptr: %", struct_any.value_pointer);
            dprint("member value ptr: %", member_any.value_pointer);
            
            return push_result(dot, member_any);
            
          case .STRUCT;
            if !execute_node(script, dot.left)  return false;
            struct_any := pop_any(*script.stack, left_type, true); // NOTE: always pop inirectly here
            member_any := Any.{ dot.value_type, struct_any.value_pointer + dot.right.(*Node_Identifier).member.offset_in_bytes };
            
            dprint("struct value ptr: %", struct_any.value_pointer);
            dprint("member value ptr: %", member_any.value_pointer);
            
            return push_result(dot, member_any);
            
          case .TYPE;
            // NOTE: the type expression on LHS was evaluated during typechecking
            return execute_node(script, dot.right, provided_storage = provided_storage);
        }
        
        assert(false, "Invalid Node_Dot: %", dot.*);
        
      case Node_Subscript;
        subscript := node.(*Node_Subscript);
        
        if !execute_node(script, subscript.base_expression)  return false;      
        if !execute_node(script, subscript.indexing_expression)  return false;      
        
        index_any := pop_any(*script.stack, subscript.indexing_expression.value_type);
        base_any  := pop_any(*script.stack, subscript.base_expression.value_type, true);
        
        ti_array := subscript.base_expression.value_type.(*Type_Info_Array);
        indexing_type := subscript.indexing_expression.value_type;
        
        index_as_s64: s64;
        assert(Convert.any_to_int(index_as_s64, index_any));
        
        array_count, array_data := get_array_count_and_data(base_any.value_pointer, xx base_any.type);
        
        if index_as_s64 < 0 || index_as_s64 >= array_count {
            set_execution_error(script, "Array index % was out of bounds! Max index is %.", index_as_s64, array_count-1, node = node);
            return false;
        }
        
        element := Any.{ node.value_type, array_data + index_as_s64 * node.value_type.runtime_size };
        
        if element.type.runtime_size > 8
            then dprint("returning result of indexing operation at %", element.value_pointer);
            else dprint("returning result of indexing operation with value % at %", element, element.value_pointer);
        
        return push_result(node, element);
        
        
      case Node_Declaration;
        declaration := node.(*Node_Declaration);
        
        // early return on macro for now so that we don't overwrite value of malleable literal
        // the initial value is evaluated in typechecking phase, which is fine since for now we only allow simple literals in a macro decl
        if declaration.flags & .MACRO  return true;
        
        dprint("declaration name: %", get_declaration_name(script, declaration));
        value_pointer := get_value_pointer(script, declaration);
        
        if declaration.init_expression {
            if !execute_node(script, declaration.init_expression, provided_storage = value_pointer)  return false;
        } else {
            // TODO: use initializer_of for aggregate types?
            memset(value_pointer, 0, declaration.value_type.runtime_size);
        }
        return true;
        
        
      case Node_Block;
        block := node.(*Node_Block);
        
        // NOTE: we don't execute named blocks unless explicitly called
        if block.name && (flags & .EXPLICIT_CALL)  return true;
        
        // for debugging purposes
        stack_top_before := script.stack.top;
        defer assert(script.stack.top == stack_top_before);
        
        is_stack_frame := is_stack_frame_block(script, block);
        if is_stack_frame {
            if !push_frame(*script.stack, block.stack_frame_size) {
                set_execution_error(script, type = .STACK_OVERFLOW, node = node);
                return false;
            }
        }
        for block.statements {
            if !execute_node(script, it)  return false;
        }
        if is_stack_frame {
            pop_frame(*script.stack);
        }
        return true;
        
        
      case Node_If_Statement;
        if_statement := node.(*Node_If_Statement);
        if !execute_node(script, if_statement.condition)  return false;
        
        condition := pop_any(*script.stack, if_statement.condition.value_type);
        dprint("condition: % %", as_type(condition.type), condition);
        
        condition_as_bool: bool;
        Convert.any_to_bool(condition_as_bool, condition);
        if condition_as_bool {
            if !execute_node(script, if_statement.statement)  return false;
        }
        return true;
        
        
      case Node_While_Loop;
        while_loop := node.(*Node_While_Loop);
        while loop := true {
            if !execute_node(script, while_loop.condition)  return false;
            
            condition := pop_any(*script.stack, while_loop.condition.value_type);
            dprint("condition: % %", as_type(condition.type), condition);
            
            condition_as_bool: bool;
            Convert.any_to_bool(condition_as_bool, condition);
            if !condition_as_bool  break loop;
            
            if !execute_node(script, while_loop.statement)  return false;
        }
        return true;
        
        
      case Node_For_Loop;
        for_loop := node.(*Node_For_Loop);
        if for_loop.control_type == {
          case .ARRAY;
            if !execute_node(script, for_loop.array_expression)  return false;
            
            ti_array := for_loop.array_expression.value_type.(*Type_Info_Array);
            array := pop_any(*script.stack, ti_array, true); // TODO: maybe not use lvalue later?
            
            array_count, array_data := get_array_count_and_data(array.value_pointer, xx array.type);
            
            // these will be accessed by iterator and iterator_index nodes during iteration
            it_index: int;
            for_loop.it_decl.value_pointer       = array_data;
            for_loop.it_index_decl.value_pointer = *it_index;
            
            while it_index < array_count {
                if !execute_node(script, for_loop.statement)  return false;
                for_loop.it_decl.value_pointer += ti_array.element_type.runtime_size;
                it_index += 1;
            }
            return true;
            
          case .RANGE;
            if !execute_node(script, for_loop.range.lower)  return false;
            if !execute_node(script, for_loop.range.upper)  return false;
            
            control_type := for_loop.range.lower.value_type;
            upper := pop_any(*script.stack, control_type);
            lower := pop_any(*script.stack, control_type);
            
            upper_as_s64: int;
            if !Convert.any_to_int(upper_as_s64, upper) {
                set_execution_error(script, "Failed to convert upper bound of for loop iteration range to s64. Source value was % of type %", upper, as_type(upper.type), node = for_loop.range.upper);
                return false;
            }
            
            lower_as_s64: int;
            if !Convert.any_to_int(lower_as_s64, lower) {
                set_execution_error(script, "Failed to convert lower bound of for loop iteration range to s64. Source value was % of type %", lower, as_type(lower.type), node = for_loop.range.lower);
                return false;
            }
            
            for lower_as_s64..upper_as_s64 {
                _it: Number_Union;
                Convert.any_to_any(Any.{ control_type, *_it }, it);
                
                for_loop.it_decl.value_pointer = *_it;
                // TODO: for now we just don't set it_index. 
                // we should probably prevent using it_index for range-based loops in typecheck 
                
                if !execute_node(script, for_loop.statement)  return false;
            }
            return true;
            
          case .LIST;
            _it_index: int;
            for_loop.it_index_decl.value_pointer = *_it_index;
            for for_loop.list {
                _it_index = it_index;
                if !execute_node(script, it)  return false;
                for_loop.it_decl.value_pointer = pop_any(*script.stack, for_loop.it_decl.value_type, true).value_pointer;
                if !execute_node(script, for_loop.statement)  return false;
            }
            return true;
        }
        
        assert(false, "Invalid control expression in for loop.");
    }
    
    assert(false);
    return false;
}

