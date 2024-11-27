

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
        dbg_print("executing statement #%\n", it_index);
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
*/
execute_node :: (script: *Script, node: *Node) -> bool {
    dbg_print("evaluating node %: %\n", node, node.node_type);
    dbg_print_push_indent();
    
    dbg_print("stack_pointer offset: %  (%)\n", script.stack_ptr - script.stack_base, script.stack_ptr);
    if node.flags & .IS_LVALUE  dbg_print("expecting lvalue\n");
    
    if !node.value_type {
        dbg_print("Warning: node has no set value type!\n");
    }
    
    if node.node_type == {
      case .DIRECTIVE;
        return true; // Directives are run as soon as they are parsed, so just skip this node.
        
        
      case .LITERAL;
        literal := cast(*Node_Literal) node;
        if literal.literal_type == {
          case .STRING;
            if !stack_push_any(script, literal.text, node.flags & .IS_LVALUE != 0)  return false;
            return true;
            
          case .NUMBER;
            value: u64 = 0; // just empty space.  TODO: get rid of need for this by storing value as resolved type
            remap_data(Any.{ literal.value_type, *value }, literal.number); 
            if !stack_push_any(script, Any.{ literal.value_type, *value })  return false;
            return true;
            
          case .STRUCT;
            if !stack_push_any(script, Any.{ literal.value_type, literal.aggr.value_pointer })  return false;
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
            
          case .ARRAY;
            log("Error: array literals not yet implemented.");
        }
        return false;
        
        
      case .IDENTIFIER;
        identifier  := cast(*Node_Identifier) node;
        if identifier.resolved_type == {
          case .DECLARATION;
            declaration := identifier.declaration;
            variable_any := Any.{ identifier.value_type, declaration.value_pointer };
            dbg_print("returning internal variable with value % at %\n", variable_any, variable_any.value_pointer);
            if !stack_push_any(script, variable_any, identifier.flags & .IS_LVALUE != 0)  return false;
            return true;
            
          case .EXTERNAL_VARIABLE;
            variable := *script.variables[identifier.variable_index];
            dbg_print("returning external variable with value % at %\n", variable.binding, variable.binding.value_pointer);
            if !stack_push_any(script, variable.binding, identifier.flags & .IS_LVALUE != 0)  return false;
            return true;
            
          case .EXTERNAL_PROCEDURE;
            procedure := *script.procedures[identifier.procedure_index];
            dbg_print("returning external procedure with type % at %\n", as_type(procedure.type), procedure.ptr);
            if !stack_push_any(script, procedure.ptr)  return false;
            return true;
        }
        return false;

        
      case .OPERATION;
        operation := cast(*Node_Operation) node;
        
        if is_aggr(operation.value_type) {
            if !stack_push_any(script, Any.{ operation.value_type, operation.return_ptr })  return false;         
        } else {
            zero: s64 = 0;
            if !stack_push_any(script, Any.{ operation.value_type, *zero })  return false;  // TODO: this is silly. we need a stack_push_zeroes procedure or something
        }
        
        if !execute_node(script, operation.left )  return false;
        if !execute_node(script, operation.right)  return false;
        
        // TODO: maybe use the simpler version of execute_builtin_operation, but with this more dynamic stack popping
        //       this is probably a little bit slower, but it may be worth it to reduce code bloat
        //       the other benefit(?) of going this way is that we could do the swap args swizzle here, rather than swap them at the ast level
        // op    := BUILTIN_OPERATIONS[operation.operator_index];
        // right := stack_pop    (*script.stack_ptr, is_aggr(op.right));
        // left  := stack_pop    (*script.stack_ptr, is_aggr(op.left ));
        // ret   := stack_get_top( script.stack_ptr, is_aggr(op.ret  ));
        // if !execute_builtin_operation(operation.operator_index, left, right, ret)  return false;
        
        // dbg_print("operation: %\n", op);
        
        execute_builtin_operation(operation.operator_index, *script.stack_ptr);
        
        result_any := Any.{ 
            operation.value_type, 
            ifx is_aggr(operation.value_type) then operation.return_ptr else script.stack_ptr
        };
        dbg_print("returning result of binary operation: % %\n", as_type(operation.value_type), result_any);
        return true;
        
        
      case .PROCEDURE_CALL;
        procedure_call := cast(*Node_Procedure_Call) node;
        // TODO: this will have to be refactored later for bytecode (to push result ptr after popping args)
        
        if !execute_node(script, procedure_call.procedure_expression)  return false;
        proc_type  := cast(*Type_Info_Procedure) procedure_call.procedure_expression.value_type;
        assert(proc_type.type == .PROCEDURE);
        dbg_print("proc_type: %\n", as_type(proc_type));
        
        procedure  := stack_pop_any(script, proc_type); // TODO: really no need to get an any here. but we need to fix the older stack_pop proc
        // we have to grab the actual procedure pointer value now before overwriting with another stack push below
        proc_ptr   := (cast(**void)procedure.value_pointer).*;
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
            if !jai_call || !(proc_type.procedure_flags & .HAS_NO_CONTEXT) {
                dcArgPointer(script.dyncall_vm, *context);
            }
            
            return_type := ifx proc_type.return_types 
                then proc_type.return_types[0]
                else type_info(void);
            
            arg_ptr := arg_ptr_start;
            for arg_type: proc_type.argument_types {
                value_pointer := arg_ptr;
                if is_aggr(arg_type)  value_pointer = (cast(**void)value_pointer).*;
                if !push_argument(script.dyncall_vm, arg_type, value_pointer, true)  return false;
                arg_ptr += size_of(*void);
            }
            
            dcArgPointer(script.dyncall_vm, return_ptr);
            dcCallVoid(script.dyncall_vm, xx proc_ptr);
        }
        
        script.stack_ptr = arg_ptr_start;
        
        dbg_print("returning result of procedure call: %\n", Any.{ procedure_call.value_type, return_ptr });
        
        if procedure_call.flags & .DISCARD_VALUE {
            stack_pop_any(script, procedure_call.value_type);
        }
        return true;
        
        
      case .DOT;
        dot := cast(*Node_Dot) node;
        if dot.dot_type == {
          case .STRUCT_MEMBER;
            if !execute_node(script, dot.left)  return false;  
            struct_any := stack_pop_any(script, dot.left.value_type, true);
            member_any := Any.{ dot.value_type, struct_any.value_pointer + dot.member_offset };
            
            dbg_print("struct value ptr: %\n", struct_any.value_pointer);
            dbg_print("member value ptr: %\n", member_any.value_pointer);
            
            if !stack_push_any(script, member_any, (dot.flags & .IS_LVALUE) == .IS_LVALUE)  return false;
            return true;
          
        }
        return false;
        
      case .SUBSCRIPT;
        subscript := cast(*Node_Subscript) node;
        
        if !execute_node(script, subscript.base_expression)  return false;      
        if !execute_node(script, subscript.indexing_expression)  return false;      
        
        index_any := stack_pop_any(script, subscript.indexing_expression.value_type);
        base_any  := stack_pop_any(script, subscript.base_expression.value_type, true);
        
        ti_array := cast(*Type_Info_Array) subscript.base_expression.value_type;
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
        dbg_print("returning result of indexing operation: %\n", element);
        return true;
        
    
      case .DECLARATION;
        declaration := cast(*Node_Declaration) node;
        // if declaration.declaration_type != .INTERNAL_VARIABLE {
        //     log("Error: cannot execute a declaration node with declaration type of %", declaration.declaration_type);
        //     return false;
        // }
        
        // TODO: we need to check that the identifier does not refer to a previously declared variable
        if declaration.init_expression {
            if !execute_node(script, declaration.init_expression) {
                dbg_print("failed to evaluate init expression of declaration statement!\n");
                return false;
            }
        
            r_value := stack_pop_any(script, declaration.init_expression.value_type);
            l_value := Any.{ declaration.value_type, declaration.value_pointer };
            
            dbg_print("l_value.value_pointer: %\n", l_value.value_pointer);
            dbg_print("r_value.value_pointer: %\n", r_value.value_pointer);
            
            dbg_print("l_value: % % at %\n", as_type(l_value.type), l_value, l_value.value_pointer);
            dbg_print("r_value: % % at %\n", as_type(r_value.type), r_value, r_value.value_pointer);
            
            memcpy(l_value.value_pointer, r_value.value_pointer, r_value.type.runtime_size);
        } else {
            l_value := Any.{ declaration.value_type, declaration.value_pointer };
            memset(l_value.value_pointer, 0, l_value.type.runtime_size);
        }
        return true;
        
        
      case .ASSIGNMENT;
        assignment := cast(*Node_Assignment) node;
        
        if !execute_node(script, assignment.left) {
            dbg_print("failed to evaluate left side of assignment statement!\n");
            return false;
        }
        if !execute_node(script, assignment.right) {
            dbg_print("failed to evaluate right side of assignment statement!\n");
            return false;
        }
        
        r_value := stack_pop_any(script, assignment.right.value_type);
        l_value := stack_pop_any(script, assignment.left.value_type, true);
        
        dbg_print("l_value.value_pointer: %\n", l_value.value_pointer);
        dbg_print("r_value.value_pointer: %\n", r_value.value_pointer);
        
        dbg_print("l_value: % % at %\n", as_type(l_value.type), l_value, l_value.value_pointer);
        dbg_print("r_value: % % at %\n", as_type(r_value.type), r_value, r_value.value_pointer);
        
        memcpy(l_value.value_pointer, r_value.value_pointer, r_value.type.runtime_size);
        return true;
        
        
      case .BLOCK;
        block := cast(*Node_Block) node;
        for block.statements {
            dbg_print("executing statement #%\n", it_index);
            if !execute_node(script, it)  return false;
        }
        return true;
        
        
      case .IF_STATEMENT;
        if_statement := cast(*Node_If_Statement) node;
        if !execute_node(script, if_statement.condition) {
            dbg_print("failed to evaluate left side of assignment statement!\n");
            return false;
        }
        
        condition := stack_pop_any(script, if_statement.condition.value_type);
        dbg_print("condition: % %\n", as_type(condition.type), condition);
        
        condition_as_bool: bool;
        any_to_bool(condition_as_bool, condition);
        if condition_as_bool {
            if !execute_node(script, if_statement.statement) {
                dbg_print("failed to evaluate left side of assignment statement!\n");
                return false;
            }
        }
        return true;
        
        
      case .WHILE_LOOP;
        while_loop := cast(*Node_While_Loop) node;
        while loop := true {
            if !execute_node(script, while_loop.condition) {
                dbg_print("failed to evaluate left side of assignment statement!\n");
                return false;
            }
            
            condition := stack_pop_any(script, while_loop.condition.value_type);
            dbg_print("condition: % %\n", as_type(condition.type), condition);
            
            condition_as_bool: bool;
            any_to_bool(condition_as_bool, condition);
            if !condition_as_bool  break loop;
            
            if !execute_node(script, while_loop.statement) {
                dbg_print("failed to evaluate left side of assignment statement!\n");
                return false;
            }
        }
        return true;
        
        
      case .FOR_LOOP;
        for_loop := cast(*Node_For_Loop) node;
        if for_loop.control_type == {
          case .ARRAY;
            if !execute_node(script, for_loop.array_expression) {
                dbg_print("failed to evaluate for loop control node!\n");
                return false;
            }
            
            ti_array := cast(*Type_Info_Array) for_loop.array_expression.value_type;
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
                    dbg_print("failed to evaluate left side of assignment statement!\n");
                    return false;
                }
            }
            return true;
            
          case .RANGE;
            if !execute_node(script, for_loop.range.lower) {
                dbg_print("failed to evaluate for_loop.range.lower!\n");
                return false;
            }
            if !execute_node(script, for_loop.range.upper) {
                dbg_print("failed to evaluate for_loop.range.upper!\n");
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
                    dbg_print("failed to evaluate left side of assignment statement!\n");
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





execute_directive :: (script: *Script, directive: *Node_Directive) -> bool {
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
    
    for directive.arguments  if !execute_node(script, it)  return false;
    
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
        if is_aggr(arg_type)  value_pointer = (cast(**void)value_pointer).*;
        if !push_argument(script.dyncall_vm, arg_type, value_pointer, true)  return false;
        arg_ptr += size_of(*void);
    }
    
    dcArgPointer(script.dyncall_vm, return_ptr);
    dcCallVoid(script.dyncall_vm, xx procedure.ptr);
    
    script.stack_ptr = arg_ptr_start;
    
    result := cast(*bool) stack_pop_any(script, return_type).value_pointer;
    return result.*;
}