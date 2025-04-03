

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
        log("Error: cannot execute script before AST has been constructed!\n");
        return false;
    }
    if !(script.flags & .TYPECHECKED) {
        log("Error: cannot execute script before AST has been typechecked!\n");
        return false;
    }
    
    script.stack_ptr = script.stack_base;
    
    for script.ast_root.statements {
        dprint("executing statement #%\n", it_index);
        if !execute_node(script, it)  return false;
    }
    
    sentinel := cast(*type_of(STACK_SENTINEL_VALUE)) (script.stack_base + script.stack_size);
    if sentinel.* != STACK_SENTINEL_VALUE {
        log("Error: sentinel value was overwritten! was %\n", sentinel.*);
        return false;
    }
    
    return true;
}

/*
    Even though the script has already been typechecked, I use stack_push_any and stack_pop_any 
        so that it's easier to get the type info when needed for debugging.
    Using these procedures also just makes the code here a bit easier to read and captures the 
        type-specific rules about additional indirections automatically.
    When moving to bytecode though, we should be just use stack_push and stack_pop, 
        which are probably slightly faster and don't require passing type info.
        
    The explicit_call param is now required for signalling that we are indeed wanting to run a named block.
    Maybe this is sort of a back hack though...
*/
execute_node :: (script: *Script, node: *Node, explicit_call := false) -> bool {
    dprint("executing node %: %\n", node, node.node_type);
    dprint_push_indent();
    
    dprint("stack_pointer offset: %  (%)\n", script.stack_ptr - script.stack_base, script.stack_ptr);
    if node.flags & .IS_LVALUE  dprint("expecting lvalue\n");
    
    if !node.value_type {
        dprint("Warning: node has no set value type!\n");
    }
    
    if !node.flags & .TYPECHECKED {
        // TODO: file/line or content of node please?
        log("Error: cannot execute a node that has not been typechecked!");
        log("\tNode type was %", node.node_type);
        log("\tNode expression was %", sprint_node(script, node));
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
        if literal.flags & .IS_LVALUE then assert(literal.flags & .IS_MALLEABLE == .IS_MALLEABLE || literal.literal_type == .ANY);
        
        if #complete literal.literal_type == {
          case .ANY;
            if !stack_push_any(script, literal.any, literal.flags & .IS_LVALUE != 0)  return false;
            dprint("returning literal with value %\n", literal.any);
            return true;
            
          case .STRING;
            if !stack_push_any(script, literal.text, literal.flags & .IS_LVALUE != 0)  return false;
            dprint("returning literal with value %\n", literal.text);
            return true;
            
          case .NUMBER;
            if !stack_push_any(script, to_any(*literal.number), literal.flags & .IS_LVALUE != 0)  return false;
            dprint("returning literal with value %\n", to_any(*literal.number));
            return true;
            
          case .STRUCT;
            if !stack_push_any(script, Any.{ literal.value_type, literal.aggr.value_pointer }, literal.flags & .IS_LVALUE != 0)  return false;
            write_ptr := literal.aggr.value_pointer;
            // TODO: if we enhance typechecking to pass value pointer for these values, 
            //       then we don't need to do any pushing or popping from stack here.
            //       will require changes to almost all execute cases though...
            //       if node.dst_provided  memcpy directly  else push stack
            for literal.aggr.expressions {
                if !execute_node(script, it)  return false;
                memcpy(write_ptr, stack_pop_any(script, it.value_type).value_pointer, it.value_type.runtime_size);
                write_ptr += it.value_type.runtime_size;
            }
            return true;
        }
        return false;
        
        
      case Node_Identifier;
        identifier := node.(*Node_Identifier);
        if identifier.identifier_type == {
          case .DECLARATION;
            declaration := identifier.declaration;
            if declaration.flags & .MACRO {
                // return execute_node(script, declaration.init_expression);
            }
            
            variable_any := Any.{ identifier.value_type, declaration.value_pointer };
            dprint("returning internal variable with value % at %\n", variable_any, variable_any.value_pointer);
            if !stack_push_any(script, variable_any, identifier.flags & .IS_LVALUE != 0)  return false;
            return true;
            
          case .EXTERNAL_VARIABLE;
            variable := *script.variables[identifier.index];
            dprint("returning external variable with value % at %\n", variable.binding, variable.binding.value_pointer);
            if !stack_push_any(script, variable.binding, identifier.flags & .IS_LVALUE != 0)  return false;
            return true;
            
          case .EXTERNAL_PROCEDURE;
            procedure := *script.procedures[identifier.index];
            dprint("returning external procedure with type % at %\n", as_type(procedure.type), procedure.ptr);
            assert(identifier.flags & .IS_LVALUE == 0);
            if !stack_push_any(script, procedure.ptr)  return false;
            return true;
            
          case .LITERAL;
            return execute_node(script, identifier.literal);
            
          case;
            log("Error: unhandled case in execute_node for identifier type: %", identifier.identifier_type);
            return false;
        }
        return false;

        
      case Node_Operation;
        operation := node.(*Node_Operation);
        
        if operation.flags & .OVERLOAD {
            return execute_node(script, operation.overload_procedure);
        }
        
        if operation.operator_type != .ASSIGNMENT {
            if is_aggr(operation.value_type) {
                if !stack_push_any(script, Any.{ operation.value_type, operation.return_ptr })  return false;         
            } else {
                if !stack_push_zeroes(script, 8)  return false;
            }
        }
        
        if !execute_node(script, operation.left)  return false;
        
        if operation.operator_type != .UNARY {
            if !execute_node(script, operation.right)  return false;
        }
        
        // special case: operator = is always a simple memcpy
        if operation.name == "=" {
            assert(operation.left.flags & .IS_LVALUE != 0);
            r_value := stack_pop_any(script, operation.right.value_type);
            l_value := stack_pop_any(script, operation.left.value_type, true);
            memcpy(l_value.value_pointer, r_value.value_pointer, r_value.type.runtime_size);
            return true;
        }
        
        // TODO: maybe use the simpler version of execute_builtin_operation, but with this more dynamic stack popping
        //       this is probably a little bit slower, but it may be worth it to reduce code bloat
        //       the other benefit(?) of going this way is that we could do the swap args swizzle here, rather than swap them at the ast level
        // op    := BUILTIN_OPERATIONS[operation.operator_index];
        // right := null;
        // if !is_unary  right = stack_pop (script, is_aggr(op.right));
        // left  := stack_pop (script, is_aggr(op.left ));
        // ret   := stack_peek(script, is_aggr(op.ret  ));
        // if !execute_builtin_operation(operation.operator_index, left, right, ret)  return false;
        
        // dprint("operation: %\n", op);
        
        execute_builtin_operation(script, operation.operator_index);
        
        result_any := Any.{ 
            operation.value_type, 
            ifx is_aggr(operation.value_type) then operation.return_ptr else script.stack_ptr
        };
        dprint("returning result of binary operation: % %\n", as_type(operation.value_type), result_any);
        return true;
        
        
      case Node_Procedure_Call;
        procedure_call := node.(*Node_Procedure_Call);
        // TODO: this will have to be refactored later for bytecode (to push result ptr after popping args)
        
        if !execute_node(script, procedure_call.procedure_expression)  return false;
        proc_type  := procedure_call.procedure_expression.value_type.(*Type_Info_Procedure);
        assert(proc_type.type == .PROCEDURE);
        dprint("proc_type: %\n", as_type(proc_type));
        
        procedure  := stack_pop_any(script, proc_type); // TODO: really no need to get an any here. but we need to fix the older stack_pop proc
        // we have to grab the actual procedure pointer value now before overwriting with another stack push below
        proc_ptr   := procedure.value_pointer.(**void).*;
        return_ptr := script.stack_ptr;
        
        if is_aggr(procedure_call.value_type) {
            if !stack_push_any(script, Any.{ procedure_call.value_type, procedure_call.return_ptr })  return false;
            return_ptr = procedure_call.return_ptr;
        } else {
            // stack_push_zeroes(script, procedure_call.value_type.runtime_size);
            stack_push_zeroes(script, 8);
        }
        arg_ptr_start := script.stack_ptr;
        
        for procedure_call.arguments  if !execute_node(script, it)  return false;
        
        // script.stack_ptr -= procedure_call.arg_count * PTR_SIZE;
        {
            dcReset(script.dyncall_vm);
            jai_call :: true;
            if jai_call || !(proc_type.procedure_flags & .HAS_NO_CONTEXT) {
                dcArgPointer(script.dyncall_vm, *context);
            }
            
            return_type := ifx proc_type.return_types 
                then proc_type.return_types[0]
                else type_info(void);
            
            arg_ptr := arg_ptr_start;
            for arg_type: proc_type.argument_types {
                value_pointer := arg_ptr;
                if is_aggr(arg_type)  value_pointer = value_pointer.(**void).*;
                if !push_argument(script.dyncall_vm, arg_type, value_pointer, true)  return false;
                arg_ptr += size_of(*void);
            }
            
            if return_type != type_info(void) {
                dcArgPointer(script.dyncall_vm, return_ptr);
            }
            dcCallVoid(script.dyncall_vm, xx proc_ptr);
        }
        
        script.stack_ptr = arg_ptr_start;
        
        dprint("returning result of procedure call: %\n", Any.{ procedure_call.value_type, return_ptr });
        
        if procedure_call.flags & .DISCARD_VALUE {
            stack_pop_any(script, procedure_call.value_type);
        }
        return true;
        
        
      case Node_Cast;
        node_cast := node.(*Node_Cast);
        
        // TODO: right now this assumes we will only ever do casts between trivial types (we should actually check this!)
        dst := Any.{ node.value_type, script.stack_ptr };
        if !stack_push_zeroes(script, 8)  return false;
        
        if !execute_node(script, node_cast.value)  return false;
        src := stack_pop_any(script, node_cast.value.value_type);
        
        if !remap_data(dst, src)  return false;
        
        return true;
        
        
      case Node_Dot;
        dot := node.(*Node_Dot);
        
        if dot.left == null {
            assert(node.value_type.type == .ENUM || node.value_type.type == .STRUCT);
            return execute_node(script, dot.right);
        }
        
        if dot.left.value_type.type == {
          case .STRUCT;
            if !execute_node(script, dot.left)  return false;  
            struct_any := stack_pop_any(script, dot.left.value_type, true);
            member_any := Any.{ dot.value_type, struct_any.value_pointer + dot.right.(*Node_Identifier).member.offset_in_bytes };
            
            dprint("struct value ptr: %\n", struct_any.value_pointer);
            dprint("member value ptr: %\n", member_any.value_pointer);
            
            if !stack_push_any(script, member_any, (dot.flags & .IS_LVALUE) == .IS_LVALUE)  return false;
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
        
        index_any := stack_pop_any(script, subscript.indexing_expression.value_type);
        base_any  := stack_pop_any(script, subscript.base_expression.value_type, true);
        
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
            return false;
        }
        
        element := Any.{ node.value_type, array_data + index_as_s64 * node.value_type.runtime_size };
        if !stack_push_any(script, element, node.flags & .IS_LVALUE == .IS_LVALUE)  return false;
        dprint("returning result of indexing operation: %\n", element);
        return true;
        
    
      case Node_Declaration;
        declaration := node.(*Node_Declaration);
        // if declaration.declaration_type != .INTERNAL_VARIABLE {
        //     log("Error: cannot execute a declaration node with declaration type of %", declaration.declaration_type);
        //     return false;
        // }
        
        // early return on macro for now so that we don't overwrite value of malleable literal
        // the initial value is evaluated in typechecking phase, which is fine since for now we only allow simple literals in a macro decl
        if declaration.flags & (.MACRO)  return true;
        
        if declaration.init_expression {
            if !execute_node(script, declaration.init_expression) {
                dprint("failed to evaluate init expression of declaration statement!\n");
                return false;
            }
            
            r_value := stack_pop_any(script, declaration.init_expression.value_type);
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
        return true;
        
        
      case Node_Block;
        block := node.(*Node_Block);
        if block.name && !explicit_call  return true; // we don't execute named blocks unless explicitly called
        
        for block.statements {
            dprint("executing statement #%\n", it_index);
            if !execute_node(script, it)  return false;
        }
        return true;
        
        
      case Node_If_Statement;
        if_statement := node.(*Node_If_Statement);
        if !execute_node(script, if_statement.condition) {
            dprint("failed to evaluate left side of assignment statement!\n");
            return false;
        }
        
        condition := stack_pop_any(script, if_statement.condition.value_type);
        dprint("condition: % %\n", as_type(condition.type), condition);
        
        condition_as_bool: bool;
        any_to_bool(condition_as_bool, condition);
        if condition_as_bool {
            if !execute_node(script, if_statement.statement) {
                dprint("failed to evaluate left side of assignment statement!\n");
                return false;
            }
        }
        return true;
        
        
      case Node_While_Loop;
        while_loop := node.(*Node_While_Loop);
        while loop := true {
            if !execute_node(script, while_loop.condition) {
                dprint("failed to evaluate left side of assignment statement!\n");
                return false;
            }
            
            condition := stack_pop_any(script, while_loop.condition.value_type);
            dprint("condition: % %\n", as_type(condition.type), condition);
            
            condition_as_bool: bool;
            any_to_bool(condition_as_bool, condition);
            if !condition_as_bool  break loop;
            
            if !execute_node(script, while_loop.statement) {
                dprint("failed to evaluate left side of assignment statement!\n");
                return false;
            }
        }
        return true;
        
        
      case Node_For_Loop;
        for_loop := node.(*Node_For_Loop);
        if for_loop.control_type == {
          case .ARRAY;
            if !execute_node(script, for_loop.array_expression) {
                dprint("failed to evaluate for loop control node!\n");
                return false;
            }
            
            ti_array := for_loop.array_expression.value_type.(*Type_Info_Array);
            array := stack_pop_any(script, ti_array, true); // TODO: maybe not use lvalue later?
            
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
                
                if !execute_node(script, for_loop.statement) {
                    dprint("failed to evaluate for loop statement!\n");
                    return false;
                }
            }
            return true;
            
          case .RANGE;
            if !execute_node(script, for_loop.range.lower) {
                dprint("failed to evaluate for_loop.range.lower!\n");
                return false;
            }
            if !execute_node(script, for_loop.range.upper) {
                dprint("failed to evaluate for_loop.range.upper!\n");
                return false;
            }
            
            control_type := for_loop.range.lower.value_type;
            upper := stack_pop_any(script, control_type);
            lower := stack_pop_any(script, control_type);
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
                
                if !execute_node(script, for_loop.statement) {
                    dprint("failed to evaluate for loop statement!\n");
                    return false;
                }
            }
            return true;
            
          case .LIST;
            _it_index: int;
            for_loop.it_index_decl.value_pointer = *_it_index;
            for for_loop.list {
                _it_index = it_index;
                if !execute_node(script, it)  return false;
                for_loop.it_decl.value_pointer = stack_pop_any(script, for_loop.it_decl.value_type, true).value_pointer;
                if !execute_node(script, for_loop.statement) {
                    dprint("failed to evaluate for loop statement!\n");
                    return false;
                }
            }
            return true;
        }
        
        return false;
    }
    
    assert(false);
    return false;
}

