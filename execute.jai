

/*
    TODO: we should differentiate execute and evaluate procs
    the procs here should be labelled execute, since they work primarily through side effects
        i.e., they push/pop things on the preallocated stack and rely on typecheck phase to have been done correctly

    by contrast, I will probably reintroduce some evaluate procs that basically do things the old way of dynamically typechecking and returning Any's
    these may be useful in the case of scripts that only need to be run once, e.g. evaluating expresions in gon files or seomthing
    
    as it stands, I think the execute procs will probably have decent enough performance characteristics that going to a bytecode will probabyl not be worth it for a good while
    it will be much easier to debug things using the old ast walking method, and because we are typechecked, it's really more like a fat ass bytecode with horrible cache misses
    but we would still have bad cache misses in bytecode with procedure calls anyhow because of the type info navigation that's required
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

/*
    Even though the script has already been typechecked, I use push_any(scriptack_pop_any 
        so that it's easier to get the type info when needed for debugging.
    Using these procedures also just makes the code here a bit easier to read and captures the 
        type-specific rules about additional indirections automatically.
    When moving to bytecode though, we should be just use stack_push and stack_pop, 
        which are probably slightly faster and don't require passing type info.
        
    The explicit_call param is now required for signalling that we are indeed wanting to run a named block.
    Maybe this is sort of a back hack though...
*/
execute_node :: (script: *Script, node: *Node, explicit_call := false) -> bool {
    dprint("executing node %: %", node, node.node_type);
    dprint_push_indent();
    
    dprint("stack_pointer offset: %  (%)", script.stack.top - script.stack.memory, script.stack.top);
    if node.flags & .PUSH_BY_POINTER  dprint("expecting lvalue");
    
    if !node.value_type {
        dprint("Warning: node has no set value type!");
    }
    
    if !node.flags & .TYPECHECKED {
        set_execution_error(script, "Cannot execute a node that has not been typechecked.", node = node);
        return false;
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
        if literal.flags & .PUSH_BY_POINTER {
            assert(literal.flags & .IS_MALLEABLE == .IS_MALLEABLE || literal.literal_type == .ANY);
        }
        
        if #complete literal.literal_type == {
          case .ANY;
            if !push_any(script, literal.any, literal.flags & .PUSH_BY_POINTER != 0)  return false;
            dprint("returning literal with value %", literal.any);
            return true;
            
          case .STRING;
            if !push_any(script, literal.text, literal.flags & .PUSH_BY_POINTER != 0)  return false;
            dprint("returning literal with value %", literal.text);
            return true;
            
          case .BOOLEAN; #through;
          case .NUMBER;
            if !push_any(script, to_any(*literal.number), literal.flags & .PUSH_BY_POINTER != 0)  return false;
            dprint("returning literal with value %", to_any(*literal.number));
            return true;
            
          case .STRUCT;
            if !push_any(script, Any.{ literal.value_type, literal.aggr.value_pointer }, literal.flags & .PUSH_BY_POINTER != 0)  return false;
            write_ptr := literal.aggr.value_pointer;
            // TODO: if we enhance typechecking to pass value pointer for these values, 
            //       then we don't need to do any pushing or popping from stack here.
            //       will require changes to almost all execute cases though...
            //       if node.dst_provided  memcpy directly  else push stack
            for literal.aggr.expressions {
                if !execute_node(script, it)  return false;
                memcpy(write_ptr, pop_any(script, it.value_type).value_pointer, it.value_type.runtime_size);
                write_ptr += it.value_type.runtime_size;
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
            if !push_any(script, variable_any, identifier.flags & .PUSH_BY_POINTER != 0)  return false;
            return true;
            
          case .EXTERNAL_VARIABLE;
            variable := *script.variables[identifier.index];
            if variable.value_pointer == null {
                set_execution_error(script, "Unable to get value of external variable '%'.", variable.name, node = node);
                return false;
            }
            
            value := get_value(variable);
            if variable.type.runtime_size > 8
                then dprint("returning external variable at %", variable.value_pointer);
                else dprint("returning external variable with value % at %", value, variable.value_pointer);
            if !push_any(script, value, identifier.flags & .PUSH_BY_POINTER != 0)  return false;
            return true;
            
          case .EXTERNAL_PROCEDURE;
            procedure := *script.procedures[identifier.index];
            dprint("returning external procedure with type % at %", as_type(procedure.proc_info), procedure.pointer);
            assert(identifier.flags & .PUSH_BY_POINTER == 0);
            if !push_any(script, procedure.pointer)  return false;
            return true;
            
          case .LITERAL;
            return execute_node(script, identifier.literal);
            
            
            // identifier types that should never be executed:
          case .UNRESOLVED;
          case .TYPE;
          case .STRUCT_MEMBER;
        }
        assert(false, "Invalid identifier type in execute_node: %", identifier.identifier_type);
        
        
      case Node_Operation;
        operation := node.(*Node_Operation);
        
        if operation.flags & .OVERLOAD {
            return execute_node(script, operation.overload_procedure);
        }
        
        result_value_pointer := script.stack.top;
        
        _operator := get_operator(script, operation);
        if _operator.kind != .ASSIGNMENT {
            // TODO: don't push space for the return value if we have some hint storage
            //       need to first implement some means to check if we will have hint storage during typechecking
            if !push_zeroes(script, operation.value_type.runtime_size)  return false;
        }
        
        if !execute_node(script, operation.left)  return false;
        
        if !is_unary(_operator.kind) {
            if !execute_node(script, operation.right)  return false;
        }
        
        // special case: operator = is always a simple memcpy
        if operation.name == "=" {
            assert(operation.left.flags & .PUSH_BY_POINTER != 0);
            r_value := pop_any(script, operation.right.value_type);
            l_value := pop_any(script, operation.left.value_type, true);
            memcpy(l_value.value_pointer, r_value.value_pointer, r_value.type.runtime_size);
            return true;
        }
        
        dprint("operation name: %, index: %, kind: %", operation.name, operation.operator_index, _operator.kind);
        
        right: Any;
        if !is_unary(_operator.kind) {
            right = pop_any(script, operation.right.value_type, operation.right.flags & .PUSH_BY_POINTER != 0);
        }
        left := pop_any(script, operation.left.value_type, operation.left.flags & .PUSH_BY_POINTER != 0);
        
        // TODO: swizzle arguments if needed
        
        dprint("left: %, right: %, result pointer: %", left, right, result_value_pointer);
        if !execute_builtin_operation(operation.builtin_operation_index, left.value_pointer, right.value_pointer, result_value_pointer)  return false;
        
        result_any := Any.{ operation.value_type, script.stack.top };
        dprint("returning result of operation: % %", as_type(operation.value_type), result_any);
        return true;
        
        
      case Node_Procedure_Call;
        procedure_call := node.(*Node_Procedure_Call);
        // TODO: this will have to be refactored later for bytecode (to push result ptr after popping args)
        
        if !execute_node(script, procedure_call.procedure_expression)  return false;
        proc_info := procedure_call.procedure_expression.value_type.(*Type_Info_Procedure);
        assert(proc_info.type == .PROCEDURE);
        
        dprint("proc_info: %", as_type(proc_info));
        
        procedure  := pop_any(script, proc_info); // TODO: really no need to get an any here. but we need to fix the older stack_pop proc
        
        // we have to grab the actual procedure pointer value now before overwriting with another stack push below
        proc_ptr   := procedure.value_pointer.(**void).*;
        
        return_ptr := script.stack.top;
        if !push_zeroes(script, procedure_call.value_type.runtime_size)  return false;
        
        // TODO: in the future, we should probably just use *void instead of Any for arguments array, only using Any in debug mode
        arguments: [] Any;
        arguments.data  = script.stack.top;
        arguments.count = proc_info.argument_types.count;
        arguments_push_size := arguments.count * size_of(Any);
        if !push_zeroes(script, arguments_push_size)  return false;
        
        spba := script.stack.top;
        
        for procedure_call.arguments  if !execute_node(script, it)  return false;
        
        // arguments := NewArray(proc_info.argument_types.count, Any,, temp);
        for < proc_info.argument_types {
            argument_node := procedure_call.arguments[it_index];
            
            // NOTE: We provide an Any with the type from argument_node, which may not be the same as the formal argument type if we are doing an implicit reference.
            //       The actual implicit reference is currently handled in the call_procedure wrapper, which is not really ideal, but it's what I've got to do for the time being.
            argument_any := pop_any(script, it, argument_node.flags & .PUSH_BY_POINTER != 0);
            arguments[it_index] = argument_any;
            
            if argument_any.type.runtime_size > 8
                then dprint("argument % @ %", it_index, argument_any.value_pointer);
                else dprint("argument % @ %: %", it_index, argument_any.value_pointer, argument_any);
        }
        
        assert(spba == script.stack.top);
        
        result := Any.{ procedure_call.value_type, return_ptr };
        result_array: [] Any; 
        
        dprint("return_ptr: %", return_ptr);
        
        if result.type != xx void then result_array = .[ result ];
        #if USING_DYNCALL {
            if !do_dyncall(script.dyncall_vm, Any.{ proc_info, *proc_ptr }, arguments, result_array) {
                set_general_error(script, "Failed while trying to make dyncall.");
                return false;
            }
        } else {
            if !try_calling_procedure_with_wrapper(proc_info, proc_ptr, arguments, result_array) {
                set_general_error(script, "Unable to find wrapper for procedure of type: %", as_type(proc_info));
                return false;
            }
        }
        
        dprint("returning result of procedure call: %", Any.{ procedure_call.value_type, return_ptr });
        
        pop(*script.stack, arguments_push_size);
        
        if procedure_call.flags & .DISCARD_VALUE {
            pop_any(script, procedure_call.value_type);
        }
        return true;
        
        
      case Node_Cast;
        node_cast := node.(*Node_Cast);
        
        src_type := node_cast.value.value_type;
        dst_type := node_cast.value_type;
        
        // special case for implicitly taking a reference
        if dst_type.type == .POINTER 
        && dst_type.(*Type_Info_Pointer).pointer_to == src_type {
            // Actually do nothing, but caller will pop the pointer as rvalue instead of as lvalue now
            assert(node_cast.value.flags & .PUSH_BY_POINTER != 0);
            assert(node_cast.flags & .PUSH_BY_POINTER == 0);
            
            if !execute_node(script, node_cast.value)  return false;
            return true;
        }
        
        // TODO: special case to handle conversion from Any. 
        
        // TODO: right now this assumes we will only ever do casts between trivial types (we should actually check this!)
        assert(!is_aggr(node_cast.value_type) && !is_aggr(node_cast.value.value_type));
        
        dst := Any.{ node.value_type, script.stack.top };
        if !push_zeroes(script, 8)  return false;
        
        if !execute_node(script, node_cast.value)  return false;
        src := pop_any(script, node_cast.value.value_type);
        
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
            return execute_node(script, dot.right);
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
            
            struct_any, did_dereference := dereference_any_pointer(pop_any(script, pointer_info, false));
            assert(did_dereference);
            member_any := Any.{ dot.value_type, struct_any.value_pointer + dot.right.(*Node_Identifier).member.offset_in_bytes };
            
            dprint("struct value ptr: %", struct_any.value_pointer);
            dprint("member value ptr: %", member_any.value_pointer);
            
            if !push_any(script, member_any, (dot.flags & .PUSH_BY_POINTER) == .PUSH_BY_POINTER)  return false;
            return true;
            
          case .STRUCT;
            if !execute_node(script, dot.left)  return false;
            struct_any := pop_any(script, left_type, true); // NOTE: always pop inirectly here
            member_any := Any.{ dot.value_type, struct_any.value_pointer + dot.right.(*Node_Identifier).member.offset_in_bytes };
            
            dprint("struct value ptr: %", struct_any.value_pointer);
            dprint("member value ptr: %", member_any.value_pointer);
            
            if !push_any(script, member_any, (dot.flags & .PUSH_BY_POINTER) == .PUSH_BY_POINTER)  return false;
            return true;
            
          case .TYPE;
            // type expression on left side is evaluated during typechecking
            return execute_node(script, dot.right);
        }
        
        assert(false);
        
      case Node_Subscript;
        subscript := node.(*Node_Subscript);
        
        if !execute_node(script, subscript.base_expression)  return false;      
        if !execute_node(script, subscript.indexing_expression)  return false;      
        
        index_any := pop_any(script, subscript.indexing_expression.value_type);
        base_any  := pop_any(script, subscript.base_expression.value_type, true);
        
        ti_array := subscript.base_expression.value_type.(*Type_Info_Array);
        indexing_type := subscript.indexing_expression.value_type;
        
        // get index as s64
        // TODO: maybe improve this
        //       for bytecode, could do an instruction for inplace type cast
        index_as_s64: s64;
        assert(Convert.any_to_int(index_as_s64, index_any));
        
        array_count, array_data := get_array_count_and_data(base_any.value_pointer, xx base_any.type);
        
        if index_as_s64 < 0 || index_as_s64 >= array_count {
            set_execution_error(script, "Array index % was out of bounds! Max index is %.", index_as_s64, array_count-1, node = node);
            return false;
        }
        
        element := Any.{ node.value_type, array_data + index_as_s64 * node.value_type.runtime_size };
        if !push_any(script, element, node.flags & .PUSH_BY_POINTER == .PUSH_BY_POINTER)  return false;
        // dprint("returning result of indexing operation: %", element);
        
        if element.type.runtime_size > 8
            then dprint("returning result of indexing operation at %", element.value_pointer);
            else dprint("returning result of indexing operation with value % at %", element, element.value_pointer);
        
        return true;
        
        
      case Node_Declaration;
        declaration := node.(*Node_Declaration);
        
        // early return on macro for now so that we don't overwrite value of malleable literal
        // the initial value is evaluated in typechecking phase, which is fine since for now we only allow simple literals in a macro decl
        if declaration.flags & .MACRO  return true;
        
        l_value := Any.{ declaration.value_type, get_value_pointer(script, declaration) };
        
        if declaration.init_expression {
            if !execute_node(script, declaration.init_expression)  return false;
            r_value := pop_any(script, declaration.init_expression.value_type);
            
            dprint("l_value.value_pointer: %", l_value.value_pointer);
            dprint("r_value.value_pointer: %", r_value.value_pointer);
            
            dprint("l_value.type: %", as_type(l_value.type));
            dprint("r_value.type: %", as_type(r_value.type));
            
            dprint("r_value: %", r_value);
            
            memcpy(l_value.value_pointer, r_value.value_pointer, r_value.type.runtime_size);
        } else {
            memset(l_value.value_pointer, 0, l_value.type.runtime_size);
        }
        return true;
        
        
      case Node_Block;
        block := node.(*Node_Block);
        if block.name && !explicit_call  return true; // we don't execute named blocks unless explicitly called
        
        previous_frame := script.stack.frame;
        previous_top   := script.stack.top;
        
        // TODO: is_stack_frame_block
        if block.name || block == script.ast_root {
            script.stack.frame = script.stack.top;
            script.stack.top += block.stack_frame_size;
        }
        
        for block.statements {
            if !execute_node(script, it)  return false;
        }
        
        // Check that we've popped ourselves back to where we started
        if block.name || block == script.ast_root {
            script.stack.top -= block.stack_frame_size;
            assert(script.stack.top == previous_top, "stack.top does not match what it was when we entered this block! % != %", script.stack.top, previous_top);
            script.stack.frame = previous_frame;
        }
        
        return true;
        
        
      case Node_If_Statement;
        if_statement := node.(*Node_If_Statement);
        if !execute_node(script, if_statement.condition)  return false;
        
        condition := pop_any(script, if_statement.condition.value_type);
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
            
            condition := pop_any(script, while_loop.condition.value_type);
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
            array := pop_any(script, ti_array, true); // TODO: maybe not use lvalue later?
            
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
            upper := pop_any(script, control_type);
            lower := pop_any(script, control_type);
            
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
                for_loop.it_decl.value_pointer = pop_any(script, for_loop.it_decl.value_type, true).value_pointer;
                if !execute_node(script, for_loop.statement)  return false;
            }
            return true;
        }
        
        assert(false, "Invalid control expression in for loop.");
    }
    
    assert(false);
    return false;
}

