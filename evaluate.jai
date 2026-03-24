
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

Evaluation_Flags :: enum_flags {
    NONE :: 0;
    EXPLICIT_CALL :: 1;  // used to indicate that we ought to run a named block
    DISCARD_VALUE;
}

evaluate_node_or_return :: (script: *Script, node: *Node, provided_storage: *void = null, flags := Evaluation_Flags.NONE) -> Any #expand {
    // log("% -> %", node, as_type(node.value_type));
    
    value := script.my_evaluate_node(script, node, provided_storage, flags);
    if has_error(script)  `return NULL_ANY;
    
    // #if DEBUG
    if !(node.flags & .VALUE_TYPE_IS_NOMINAL) && node.value_type.type != .ANY {
        assert(value.type == node.value_type, "%: % (%) claims to have value_type %, but it returned type %", get_location(node), node.node_type, node, as_type(node.value_type), as_type(value.type));
        
        if flags & .DISCARD_VALUE {
            dprint("Discarding unused result of % @ % of type %", node.node_type, script.stack.top, as_type(node.value_type));
            return pop_any(*script.stack, node.value_type);
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
                script.stack.top must have the same value as it did before evaluate_node was called.
            If provided_storage is null
                If should_push_by_pointer(node)
                    A pointer to the result value must be pushed to top of the stack
                else
                    The result value itself must be pushed to top of the stack
                    
    TODO: consider returning bool as first parameter to signify whether some error has ocurred.
    
    TODO: we are having the same problem here with having a massive stack frame that we had before with typechecking
          until that issue is (hopefully, eventually) fixed, I will have to refactor this funciton to check each execute each individual node type in its own procedure.
*/
evaluate_node :: (script: *Script, node: *Node, provided_storage: *void = null, flags := Evaluation_Flags.NONE) -> Any {
    dprint("Evaluating % (%) @ %", node.node_type, node, get_location(node));
    dprint_push_indent();
    
    assert(node.value_type != null);
    
    if node.value_type == null {
        set_execution_error(script, "Cannot execute a node that has not been typechecked.", node = node);
        return NULL_ANY;
    }
    
    result: Any = VOID_ANY;
    if node.node_type == {
      case Node_Directive;
        directive := node.(*Node_Directive);
        if directive.runtime_node {
            return script.my_evaluate_node(script, directive.runtime_node, provided_storage = provided_storage);
        }
        return VOID_ANY;
        
      case Node_Literal;
        result = evaluate_literal(script, xx node, provided_storage, flags);
        
      case Node_Identifier;
        result = evaluate_identifier(script, xx node, provided_storage, flags);
        
      case Node_Unary_Operator;
        result = evaluate_unary_operator(script, xx node, provided_storage, flags);
        
      case Node_Binary_Operator;
        result = evaluate_binary_operator(script, xx node, provided_storage, flags);
        
      case Node_Procedure_Call;
        result = evaluate_procedure_call(script, xx node, provided_storage, flags);
        
      case Node_Cast;
        result = evaluate_cast(script, xx node, provided_storage, flags);
        
      case Node_Dot;
        result = evaluate_dot(script, xx node, provided_storage, flags);
        
      case Node_Subscript;
        result = evaluate_subscript(script, xx node, provided_storage, flags);
        
      case Node_Declaration;
        result = evaluate_declaration(script, xx node, provided_storage, flags);
        
      case Node_Block;
        result = evaluate_block(script, xx node, provided_storage, flags);
        
      case Node_If_Statement;
        result = evaluate_if_statement(script, xx node, provided_storage, flags);
        
      case Node_If_Expression;
        result = evaluate_if_expression(script, xx node, provided_storage, flags);
        
      case Node_While_Loop;
        result = evaluate_while_loop(script, xx node, provided_storage, flags);
        
      case Node_For_Loop;
        result = evaluate_for_loop(script, xx node, provided_storage, flags);
        
      case Node_Break;
        result = evaluate_break(script, xx node, provided_storage, flags);
        
      case;
        assert(false, "Unhandled node type in script.my_evaluate_node: %", node.node_type);
    }
    return result;
}

evaluate_literal :: (script: *Script, literal: *Node_Literal, provided_storage: *void = null, flags := Evaluation_Flags.NONE) -> Any {
    if should_push_by_pointer(literal) {
        assert(literal.flags & .IS_MALLEABLE == .IS_MALLEABLE || literal.literal_type == .ANY);
    }
    
    result: Any;
    if #complete literal.literal_type == {
      case .ANY;
        result = prepare_result(script, literal, literal.any, provided_storage);
        
      case .STRING;
        result = prepare_result(script, literal, literal.text, provided_storage);
        
      case .BOOLEAN; #through;
      case .NUMBER;
        result = prepare_result(script, literal, to_any(*literal.number), provided_storage);
        
      case .STRUCT;
        // TODO: if the value of the struct literal is constant, then we should not have to recompute the inner expressions every time.
        
        value_pointer: *void;
        if literal.aggregate.value_pointer
            then value_pointer = literal.aggregate.value_pointer;
            else value_pointer = get_result_storage(script, literal, provided_storage);
        
        if !value_pointer  return NULL_ANY;
        
        write_pointer := value_pointer;
        for literal.aggregate.expressions {
            evaluate_node_or_return(script, it, provided_storage = write_pointer);
            write_pointer += it.value_type.runtime_size;
        }
        
        if literal.aggregate.value_pointer {
            result = prepare_result(script, literal, Any.{ literal.value_type, literal.aggregate.value_pointer }, provided_storage);
        } else {
            result = Any.{ literal.value_type, value_pointer };
        }
        
      case;
        assert(false, "Invalid literal type in evaluate_literal.");
    }
    return result;
}

evaluate_identifier :: (script: *Script, identifier: *Node_Identifier, provided_storage: *void = null, flags := Evaluation_Flags.NONE) -> Any {
    result: Any;
    if #complete identifier.identifier_type == {
      case .DECLARATION;
        declaration := identifier.declaration;
        if declaration.flags & .MACRO {
            // return script.my_evaluate_node(script, declaration.init_expression);
        }
        
        variable_any := Any.{ declaration.value_type, get_value_pointer(script, declaration) };
        result = prepare_result(script, identifier, variable_any, provided_storage);
        
      case .LITERAL;
        result = script.my_evaluate_node(script, identifier.literal, provided_storage = provided_storage);
        
      case .EXTERNAL_TYPE;
        assert(!should_push_by_pointer(identifier));
        result = prepare_result(script, identifier, script.type_table[identifier.index].type, provided_storage);
        
      case .EXTERNAL_VARIABLE;
        variable := *script.variables[identifier.index];
        value    := get_value(variable);
        if variable.value_pointer == null {
            set_execution_error(script, "Unable to get value of external variable '%'.", variable.name, node = identifier);
            result = NULL_ANY;
        } else {
            result = prepare_result(script, identifier, value, provided_storage);
        }
        
      case .EXTERNAL_PROCEDURE;
        assert(!should_push_by_pointer(identifier));
        result = prepare_result(script, identifier, to_any(*script.procedures[identifier.index]), provided_storage);
        
        // identifier types that should never be executed:
      case .UNRESOLVED; #through;
      case .STRUCT_MEMBER; #through;
      case;
        assert(false, "Invalid identifier type in script.my_evaluate_node: %", identifier.identifier_type);
    }
    return result;
}

evaluate_unary_operator :: (script: *Script, unary_operator: *Node_Unary_Operator, provided_storage: *void = null, flags := Evaluation_Flags.NONE) -> Any {
    if unary_operator.flags & .OVERLOAD {
        dprint("Evaluating overloaded operator...");
        return script.my_evaluate_node(script, unary_operator.overload_procedure, provided_storage = provided_storage);
    }
    
    operator_info := get_operator_info(script, unary_operator);
    dprint("Unary operator name: %, index: %, kind: %", unary_operator.token, unary_operator.operator_id, operator_info.lexical_type);
    
    result_value_pointer: *void;
    if operator_info.lexical_type != .ASSIGNMENT {
        result_value_pointer = get_result_storage(script, unary_operator, provided_storage);
        if !result_value_pointer  return NULL_ANY;
    }
    defer_restore(*script.stack.top);
    
    expression_value := evaluate_node_or_return(script, unary_operator.expression);
    
    dprint("expression: %, result pointer: %", expression_value, result_value_pointer);
    if !execute_builtin_operation(unary_operator.builtin_operation_id, expression_value.value_pointer, null, result_value_pointer)  return NULL_ANY;
    
    return Any.{ unary_operator.value_type, result_value_pointer };
}

evaluate_binary_operator :: (script: *Script, binary_operator: *Node_Binary_Operator, provided_storage: *void = null, flags := Evaluation_Flags.NONE) -> Any {
    if binary_operator.flags & .OVERLOAD {
        dprint("Evaluating overloaded operator...");
        return script.my_evaluate_node(script, binary_operator.overload_procedure, provided_storage = provided_storage);
    }
    
    // special case: operator = is always a simple memcpy
    if binary_operator.token == "=" {
        dprint("Binary operator is simple assignment.");
        assert(should_push_by_pointer(binary_operator.left));
        evaluate_node_or_return(script, binary_operator.left);
        value_pointer := pop_as(*script.stack, *void);
        evaluate_node_or_return(script, binary_operator.right, provided_storage = value_pointer);
        return VOID_ANY; // NOTE: all assignments return void
    }
    
    operator_info := get_operator_info(script, binary_operator);
    dprint("Binary operator name: %, index: %, kind: %", binary_operator.token, binary_operator.operator_id, operator_info.lexical_type);
    
    result_value_pointer: *void;
    if operator_info.lexical_type != .ASSIGNMENT {
        result_value_pointer = get_result_storage(script, binary_operator, provided_storage);
        if !result_value_pointer  return NULL_ANY;
    }
    defer_restore(*script.stack.top);
    
    left  := evaluate_node_or_return(script, binary_operator.left);
    right := evaluate_node_or_return(script, binary_operator.right);
    
    if binary_operator.flags & .SWAP_OPERANDS {
        dprint("Swapping operands");
        left, right = right, left;
    }
    
    dprint("left: %, right: %, result pointer: %", left, right, result_value_pointer);
    if !execute_builtin_operation(binary_operator.builtin_operation_id, left.value_pointer, right.value_pointer, result_value_pointer)  return NULL_ANY;
    
    return Any.{ binary_operator.value_type, result_value_pointer };
}

evaluate_procedure_call :: (script: *Script, procedure_call: *Node_Procedure_Call, provided_storage: *void = null, flags := Evaluation_Flags.NONE) -> Any {
    return_value_pointer := get_result_storage(script, procedure_call, provided_storage);
    if !return_value_pointer  return NULL_ANY;
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
        set_execution_error(script, type = .STACK_OVERFLOW, node = procedure_call);
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
            set_general_error(script, "Failed while trying to do dyncall.");
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
}

evaluate_cast :: (script: *Script, _cast: *Node_Cast, provided_storage: *void = null, flags := Evaluation_Flags.NONE) -> Any {
    src_type := _cast.value.value_type;
    dst_type := _cast.value_type;
    
    // special case to handle conversion from Any
    if src_type.type == .ANY {
        src := evaluate_node_or_return(script, _cast.value, provided_storage);
        // pop_any(*script.stack, src.type, false);
        if src.type != dst_type {
            set_execution_error(script, "Failed type assertion on Any. Expected type was %, but got %", as_type(dst_type), as_type(src.type), node = _cast);
            return NULL_ANY;
        }
        return src;
        // memcpy(dst.value_pointer, src.value_pointer, dst.type.runtime_size);
        // return dst;
    }
    
    dst_value_pointer := get_result_storage(script, _cast, provided_storage);
    if !dst_value_pointer  return NULL_ANY;
    dst := Any.{ dst_type, dst_value_pointer };
    
    // special case for implicitly taking a reference
    if dst_type.type == .POINTER 
    && dst_type.(*Type_Info_Pointer).pointer_to == src_type {
        assert(should_push_by_pointer(_cast.value));
        assert(!should_push_by_pointer(_cast));
        
        dprint("Implicitly referencing: % -> %", as_type(_cast.value.value_type), as_type(_cast.value_type));
        
        src := evaluate_node_or_return(script, _cast.value);
        dst.value_pointer.(**void).* = src.value_pointer;
        return dst;
    }
    
    // NOTE: no conversions to or from aggregates (other than implicitly taking references) are currently supported
    assert(!is_aggregate(_cast.value_type) && !is_aggregate(_cast.value.value_type));
    
    evaluate_node_or_return(script, _cast.value);
    src := pop_any(*script.stack, _cast.value.value_type);
    
    if !Convert.any_to_any(dst, src) {
        set_execution_error(script, "Failed to execute cast from % to %. Source value was %", as_type(src.type), as_type(dst.type), src, node = _cast);
        return NULL_ANY;
    }
    
    dprint("Returning result of cast: %", dst);
    return dst;
}

evaluate_dot :: (script: *Script, dot: *Node_Dot, provided_storage: *void = null, flags := Evaluation_Flags.NONE) -> Any {
    if dot.flags & .IS_ARROW {
        if script.get_virtual_member == null {
            set_execution_error(script, "Script's get_virtual_member procedure was not provided by user!", node = dot);
            return false;
        }
        
        evaluate_node_or_return(script, dot.left);
        assert(should_push_by_pointer(dot.left));
        owner := pop_any(*script.stack, dot.left.value_type, true);
        
        member_identifier := dot.right.(*Node_Identifier);
        assert(member_identifier.identifier_type == .UNRESOLVED);
        
        ok, member_value := script.get_virtual_member(script, owner, member_identifier.name);
        if !ok {
            set_execution_error(script, "get_virtual_member failed with owning type: %", as_type(owner.type), node = dot);
            return false;
        }
        return prepare_result(script, dot, member_value, provided_storage);
    }
    
    if dot.left == null {
        assert(dot.value_type.type == .ENUM || dot.value_type.type == .STRUCT);
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
            member_any := Any.{ dot.value_type, struct_pointer + dot.member_offset };
            return prepare_result(script, dot, member_any, provided_storage);
        }
        
        dprint("%: LHS is pushed by value.", get_location(dot));
        
        // NOTE: We may later find that this case is problematic and results in stack corruption issues!
        //       So if something like that happens I hope I can identify it and remember to look here!
        //       If the LHS struct value could not be pushed by pointer, then that means it's on the stack.
        //       And if it's on the stack, we need to be careful about not overwriting it.
        //       In the scope of this one node, we can do that pretty easily by pushing the result value (RHS) before pushing the LHS,
        //       but it's possible there's convoluted cases I have not yet recognized in which other nodes further up the tree
        //       may accidentally push some large value that overwrites the LHS we pushed here.
        result_value_pointer := get_result_storage(script, dot, provided_storage);
        if !result_value_pointer  return NULL_ANY;
        result := Any.{ dot.value_type, result_value_pointer };
        
        evaluate_node_or_return(script, dot.left);
        struct_pointer := pop(*script.stack, left_type.runtime_size);
        
        member_any := Any.{ dot.value_type, struct_pointer + dot.member_offset };
        
        if should_push_by_pointer(dot) {
            memcpy(result.value_pointer, *member_any.value_pointer, size_of(*void));
        } else {
            memcpy(result.value_pointer, member_any.value_pointer, member_any.type.runtime_size);
        }
        return prepare_result(script, dot, member_any, provided_storage);
        
        
      case .TYPE;
        // NOTE: the type expression on LHS was evaluated during typechecking
        return script.my_evaluate_node(script, dot.right, provided_storage = provided_storage);
        
      case;
        assert(false, "Invalid Node_Dot: %", dot.*);
    }
    
    return NULL_ANY; // unreachable
}

evaluate_subscript :: (script: *Script, subscript: *Node_Subscript, provided_storage: *void = null, flags := Evaluation_Flags.NONE) -> Any {
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
        set_execution_error(script, "Array index % was out of bounds! Max index is %.", index_as_s64, array_count-1, node = subscript);
        return NULL_ANY;
    }
    
    element := Any.{ subscript.value_type, array_data + index_as_s64 * subscript.value_type.runtime_size };
    
    return prepare_result(script, subscript, element, provided_storage);
}

evaluate_declaration :: (script: *Script, declaration: *Node_Declaration, provided_storage: *void = null, flags := Evaluation_Flags.NONE) -> Any {
    // virtual member delaration
    if declaration.left.node_type == Node_Dot && (declaration.left.flags & .IS_ARROW) {
        if script.declare_virtual_member == null {
            set_execution_error(script, "Script's declare_virtual_member procedure was not provided by user!", node = declaration);
            return false;
        }
        dot := declaration.left.(*Node_Dot);
        
        evaluate_node_or_return(script, dot.left);
        assert(should_push_by_pointer(dot.left));
        owner := pop_any(*script.stack, dot.left.value_type, true);
        
        member_identifier := dot.right.(*Node_Identifier);
        assert(member_identifier.identifier_type == .UNRESOLVED);
        
        ok, member_value, redeclared := script.declare_virtual_member(script, owner, member_identifier.name, declaration.value_type);
        if !ok {
            set_execution_error(script, "declare_virtual_member failed with owning type: %", as_type(owner.type), node = declaration);
            return false;
        }
        
        if !(declaration.flags & .MACRO) || !redeclared {
            if declaration.init_expression {
                evaluate_node_or_return(script, declaration.init_expression, provided_storage = member_value.value_pointer);
            } else {
                initialize_any(member_value);
            }
        }
        
        return prepare_result(script, declaration, member_value, provided_storage);
    }
    
    dprint("declaration name: %", get_name(script, declaration));
    
    value_pointer := get_value_pointer(script, declaration);
    dprint("declaration value_pointer: %", value_pointer);
    
    result := Any.{ declaration.value_type, value_pointer };
    
    if !(declaration.flags & .MACRO) {
        if declaration.init_expression {
            evaluate_node_or_return(script, declaration.init_expression, provided_storage = value_pointer);
        } else {
            initialize_any(result);
        }
    }
    
    return prepare_result(script, declaration, result, provided_storage);
}

evaluate_block :: (script: *Script, block: *Node_Block, provided_storage: *void = null, flags := Evaluation_Flags.NONE) -> Any {
    result_storage := get_result_storage(script, block, provided_storage);
    defer_restore(*script.break_result_storage);
    script.break_result_storage = result_storage;
    
    // NOTE: we don't execute named blocks unless explicitly called
    if block.name && !(flags & .EXPLICIT_CALL)  return Any.{ block.value_type, result_storage };
    
    block.stack_pointer = push(*script.stack, null, block.stack_frame_size);
    dprint("pushed % bytes to stack @ % for declarations in block", block.stack_frame_size, block.stack_pointer);
    
    if !block.stack_pointer {
        set_execution_error(script, type = .STACK_OVERFLOW, node = block);
        return NULL_ANY;
    }
    defer pop(*script.stack, block.stack_frame_size);
    
    for block.statements {
        stack_top_before := script.stack.top;
        
        evaluate_node_or_return(script, it, null, .DISCARD_VALUE);
        
        if script.break_target {
            if script.break_target == block {
                script.break_target = null;
                
                // This is a bit of a hack, but because we interrupt the usual control flow with a break 
                // and store the result directly to the target block, we need to manually set the stack pointer
                // back to where it is supposed to be so that our stack usage checks remain happy.
                script.stack.top = stack_top_before; 
            }
            break;
        }
        
        assert(script.stack.top == stack_top_before, "Invalid stack use on statement at %. stack top: %, stack top before: %", get_location(it), script.stack.top, stack_top_before);
    }
    
    return Any.{ block.value_type, result_storage };
}

// TODO: how can we get the block's provided storage to use here?
//       for now we will have to always make an extra copy...
evaluate_break :: (script: *Script, _break: *Node_Break, provided_storage: *void = null, flags := Evaluation_Flags.NONE) -> Any {
    result := evaluate_node_or_return(script, _break.value_expression, script.break_result_storage);
    script.break_target = _break.resolved_target;
    return result;
}

evaluate_if_statement :: (script: *Script, if_statement: *Node_If_Statement, provided_storage: *void = null, flags := Evaluation_Flags.NONE) -> Any {
    evaluate_node_or_return(script, if_statement.condition);
    
    condition := pop_any(*script.stack, if_statement.condition.value_type);
    dprint("condition: % %", as_type(condition.type), condition);
    
    subcondition: Any;
    if if_statement.subcondition {
        evaluate_node_or_return(script, if_statement.subcondition);
        subcondition = pop_any(*script.stack, if_statement.subcondition.value_type);
        dprint("subcondition: % %", as_type(condition.type), condition);
    }
    
    _, condition_as_bool := Convert.any_to(bool, ifx if_statement.subcondition then subcondition else condition);
    
    if condition_as_bool {
        if if_statement.then_block {
            evaluate_node_or_return(script, if_statement.then_block, null, .DISCARD_VALUE);
        }
    } else if if_statement.else_block {
        evaluate_node_or_return(script, if_statement.else_block, null, .DISCARD_VALUE);
    }
    
    return VOID_ANY;
}

evaluate_if_expression :: (script: *Script, if_expression: *Node_If_Expression, provided_storage: *void = null, flags := Evaluation_Flags.NONE) -> Any {
    evaluate_node_or_return(script, if_expression.condition);
    
    condition := pop_any(*script.stack, if_expression.condition.value_type);
    dprint("condition: % %", as_type(condition.type), condition);
    
    _, condition_as_bool := Convert.any_to(bool, condition);
    
    if condition_as_bool {
        if if_expression.then_expression {
            return evaluate_node_or_return(script, if_expression.then_expression, null, .NONE);
        } else {
            return prepare_result(script, if_expression, condition, provided_storage);
        }
    } else if if_expression.else_expression {
        return evaluate_node_or_return(script, if_expression.else_expression, null, .NONE);
    }
    
    return prepare_result(script, if_expression, Any.{ if_expression.value_type, null }, provided_storage);
}

evaluate_while_loop :: (script: *Script, while_loop: *Node_While_Loop, provided_storage: *void = null, flags := Evaluation_Flags.NONE) -> Any {
    while loop := true {
        evaluate_node_or_return(script, while_loop.condition);
        
        condition := pop_any(*script.stack, while_loop.condition.value_type);
        dprint("condition: % %", as_type(condition.type), condition);
        
        _, condition_as_bool := Convert.any_to(bool, condition);
        if !condition_as_bool  break loop;
        
        evaluate_node_or_return(script, while_loop.block, null, .DISCARD_VALUE);
    }
    return VOID_ANY;
}

evaluate_for_loop :: (script: *Script, for_loop: *Node_For_Loop, provided_storage: *void = null, flags := Evaluation_Flags.NONE) -> Any {
    if for_loop.control_type == {
      case .ARRAY;
        array_info := for_loop.array_expression.value_type.(*Type_Info_Array);
        evaluate_node_or_return(script, for_loop.array_expression);
        array := pop_any(*script.stack, array_info, should_push_by_pointer(for_loop.array_expression));
        
        array_count, array_data := get_array_count_and_data(array.value_pointer, xx array.type);
        
        // these will be accessed by iterator and iterator_index nodes during iteration
        it_index: int;
        for_loop.it_decl.value_pointer       = array_data;
        for_loop.it_index_decl.value_pointer = *it_index;
        
        while it_index < array_count {
            evaluate_node_or_return(script, for_loop.block, null, .DISCARD_VALUE);
            for_loop.it_decl.value_pointer += array_info.element_type.runtime_size;
            it_index += 1;
        }
        
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
            evaluate_node_or_return(script, for_loop.block, null, .DISCARD_VALUE);
        }
        
      case .LIST;
        _it_index: int;
        for_loop.it_index_decl.value_pointer = *_it_index;
        for for_loop.list {
            _it_index = it_index;
            evaluate_node_or_return(script, it);
            for_loop.it_decl.value_pointer = pop_any(*script.stack, for_loop.it_decl.value_type, true).value_pointer;
            evaluate_node_or_return(script, for_loop.block, null, .DISCARD_VALUE);
        }
        
      case;
        assert(false, "Invalid control expression in for loop.");
    }
    return VOID_ANY;
}

#scope_file

prepare_result :: (script: *Script, node: *Node, any: Any, provided_storage: *void) -> Any {
    if provided_storage {
        // assert(node.value_type.type != .ANY);
        
        #if DEBUG {
            dprint("Returning result using provided storage @ %: ", provided_storage, newline = false);
            if !is_aggregate(any.type)  dprint("%", any, newline = false);
            dprint(" (%)", as_type(any.type));
        }
        
        if any.value_pointer 
            then memcpy(provided_storage, any.value_pointer, any.type.runtime_size);
            else memset(provided_storage, 0, any.type.runtime_size);
        
        return Any.{ any.type, provided_storage };
    }
    
    ok, result := push_any(*script.stack, any, should_push_by_pointer(node));
    if !ok  set_execution_error(script, "Ran out of space on stack!", node = node);
    
    #if DEBUG {
        dprint("Returning result @ % using stack: ", result.value_pointer, newline = false);
        if !is_aggregate(any.type)  dprint("%", any, newline = false);
        dprint(" (%)", as_type(any.type));
    }
    
    return result;
}


get_result_storage :: (script: *Script, node: *Node, provided_storage: *void) -> *void {
    if provided_storage {
        assert(node.value_type != xx Any);
        dprint("Got provided storage @ % for result of type: %", provided_storage, as_type(node.value_type));
        return provided_storage;
    }
    
    result := push(*script.stack, null, node.value_type.runtime_size);
    if result {
        dprint("Got space on stack @ % for result of type: %", result, as_type(node.value_type));
    } else {
        set_execution_error(script, "Ran out of space on stack!", node = node);
    }
    return result;
}

