

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
        dbg_print("Error: cannot execute script before AST has been constructed!\n");
        return false;
    }
    if !(script.flags & .TYPECHECKED) {
        dbg_print("Error: cannot execute script before AST has been typechecked!\n");
        return false;
    }
    
    script.stack_ptr = script.stack_base;
    
    for script.ast_root.statements {
        dbg_print("executing statement #%\n", it_index);
        if !execute_node(script, it)  return false;
    }
    
    sentinel := cast(*type_of(STACK_SENTINEL_VALUE)) (script.stack_base + script.stack_size);
    if sentinel.* != STACK_SENTINEL_VALUE {
        dbg_print("ERROR: sentinel value was overwritten! was %\n", sentinel.*);
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
execute_node :: (script : *Script, node: *Node) -> bool {
    dbg_print("evaluating node %: %\n", node, node.node_type);
    dbg_print_push_indent();
    
    dbg_print("stack_pointer offset: %  (%)\n", script.stack_ptr - script.stack_base, script.stack_ptr);
    if node.flags & .IS_LVALUE  dbg_print("expecting lvalue\n");
    
    if !node.value_type {
        dbg_print("BIG TIME ERROR: node has no set value type!\n");
    }
    
    if node.node_type == {
      case .NUMBER;
        number := cast(*Node_Number) node;
        value: u64 = 0; // just empty space
        remap_data(Any.{ number.value_type, *value }, number.float_value); // TODO: get rid of need for this by storing value as resolved type
        stack_push_any(script, Any.{ number.value_type, *value });
        return true;
        
        
      case .IDENTIFIER;
        identifier  := cast(*Node_Identifier) node;
        if identifier.resolved_type == {
          case .DECLARATION;
            declaration := identifier.declaration;
            variable_any := Any.{ identifier.value_type, declaration.value_pointer };
            dbg_print("returning internal variable with value % at %\n", variable_any, variable_any.value_pointer);
            stack_push_any(script, variable_any, identifier.flags & .IS_LVALUE != 0);
            return true;
            
          case .EXTERNAL_VARIABLE;
            variable := *script.variables[identifier.variable_index];
            dbg_print("returning external variable with value % at %\n", variable.binding, variable.binding.value_pointer);
            stack_push_any(script, variable.binding, identifier.flags & .IS_LVALUE != 0);
            return true;
            
          case .EXTERNAL_PROCEDURE;
            procedure := *script.procedures[identifier.procedure_index];
            dbg_print("returning external procedure with type % at %\n", as_type(procedure.type), procedure.ptr);
            stack_push_any(script, procedure.ptr);
            return true;
        }
        return false;

        
      case .OPERATION;
        operation := cast(*Node_Operation) node;
        
        if is_aggr(operation.value_type) {
            stack_push_any(script, Any.{ operation.value_type, operation.return_ptr });         
        } else {
            zero: s64 = 0;
            stack_push_any(script, Any.{ operation.value_type, *zero });  // TODO: this is silly. we need a stack_push_zeroes procedure or something
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
        //       this will require passing script to do_dyncall so that we can push to stack from within that function
        //       and then we might as well just pass the entire node as well so that we dont have to pass return ptr explicitly.
        //       I am considering just inlining the do_dyncall code here, but it is already relatively wide on its own...
        //       maybe we inline it for now so it works all good and stuff and then un-inline it later?
        //       but we are going to need to use is from the bytecode as well, and also possibly from the eval procs.
        
        if !execute_node(script, procedure_call.procedure_expression)  return false;
        proc_type  := procedure_call.procedure_expression.value_type;
        dbg_print("proc_type: %\n", as_type(proc_type));
        
        procedure  := stack_pop_any(script, proc_type); // TODO: really no need to get an any here. but we need to fix the older stack_pop proc
        // we have to grab the actual procedure pointer value now before overwriting with another stack push below
        proc_ptr   := (cast(**void)procedure.value_pointer).*;
        return_ptr := script.stack_ptr;
        
        if is_aggr(procedure_call.value_type) {
            stack_push_any(script, Any.{ procedure_call.value_type, procedure_call.return_ptr });         
        } else {
            zero: s64 = 0;
            stack_push_any(script, Any.{ procedure_call.value_type, *zero });
        }
        arg_ptr := script.stack_ptr;
        
        for procedure_call.arguments  if !execute_node(script, it)  return false;
        
        // script.stack_ptr -= procedure_call.arg_count * PTR_SIZE;
        if !do_dyncall(script.dyncall_vm, proc_ptr, xx proc_type, arg_ptr, return_ptr) {
        // if !do_dyncall(script.dyncall_vm, procedure.ptr, procedure.type, script.stack_ptr, procedure_call.dst) {
            log("Failed while trying to call procedure '%'!", as_type(procedure.type));
            return false;
        }
        
        script.stack_ptr = arg_ptr;
        return true;
        
        
      case .MEMBER;
        member := cast(*Node_Member) node;
        
        if !execute_node(script, member.base_expression)  return false;  
        base_any   := stack_pop_any(script, member.base_expression.value_type, true);
        member_any := Any.{ member.value_type, base_any.value_pointer + member.offset };
        
        dbg_print("base value ptr: %\n", base_any.value_pointer);
        dbg_print("member value ptr: %\n", member_any.value_pointer);
        
        stack_push_any(script, member_any, (member.flags & .IS_LVALUE) == .IS_LVALUE);
        return true;
        
        
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
        if !remap_int(index_as_s64, index_any) {
            dbg_print("ERROR REMAPPING INDEXING EXPRESSION TO INT\n");
        }
        
        array_count, array_data := get_array_count_and_data(base_any.value_pointer, xx base_any.type);
        
        // TODO: better method of doing runtime indexing checks ?
        if index_as_s64 < 0 || index_as_s64 >= array_count {
            log("Runtime Error: array index '%' was out of bounds! Max index is %.", index_as_s64, array_count-1);
            return false;
        }
        
        element := Any.{ node.value_type, array_data + index_as_s64 * node.value_type.runtime_size };
        stack_push_any(script, element, node.flags & .IS_LVALUE == .IS_LVALUE);
        dbg_print("returning result of indexing operation: %\n", element);
        return true;
        
    
      case .DECLARATION;
        declaration := cast(*Node_Declaration) node;
        // if declaration.declaration_type != .INTERNAL_VARIABLE {
        //     log("Error: cannot execute a declaration node with declaration type of %", declaration.declaration_type);
        //     return false;
        // }
        
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

/*
    Because we know that all arguments will be contiguous on the stack, we simply bump the stack pointer for each argument and push.
    Of course, we still need to know the argument types, so we need the *Type_Info_Procedure.
*/
do_dyncall :: (vm: *DCCallVM, proc_ptr: *void, proc_type: *Type_Info_Procedure, arg_ptr: *void, return_ptr: *void) -> bool {
    dcReset(vm);
    
    assert(proc_type.return_types.count <= 1);
    
    jai_call :: true;
    assert(proc_type.procedure_flags & .IS_C_CALL == 0);
    
    if !jai_call || !(proc_type.procedure_flags & .HAS_NO_CONTEXT) {
        dcArgPointer(vm, *context);
    }
    
    return_type := ifx proc_type.return_types 
        then proc_type.return_types[0]
        else type_info(void);
    
    dbg_print("calling proc %\n", as_type(proc_type));
    
    #if DEBUG {
        context.dbg_print_indent += 1;
        defer context.dbg_print_indent -= 1;
    }
    
    dbg_print("args: ");
    
    for arg_type: proc_type.argument_types {    
        if arg_type.type == {
          case .POINTER;
            dcArgPointer(vm, (cast(*DCpointer)arg_ptr).*);
            
          case .ENUM; #through;
          case .BOOL; #through;
          case .INTEGER;
            if arg_type.runtime_size == {
              case 1; dcArgInt8 (vm, (cast(*DCint8 )arg_ptr).*); 
              case 2; dcArgInt16(vm, (cast(*DCint16)arg_ptr).*); 
              case 4; dcArgInt32(vm, (cast(*DCint32)arg_ptr).*); 
              case 8; dcArgInt64(vm, (cast(*DCint64)arg_ptr).*); 
              case;
                log("invalid integer size!\n");
                return false;
            }
            
          case .FLOAT;
            if arg_type.runtime_size == {
              case 4; dcArgFloat32(vm, (cast(*DCfloat32)arg_ptr).*); 
              case 8; dcArgFloat64(vm, (cast(*DCfloat64)arg_ptr).*); 
              case;
                log("invalid float size!\n");
                return false;
            }
            
          case .STRUCT;
            if jai_call {
                dcArgPointer(vm, (cast(**void)arg_ptr).*); 
            } else {
                ag := find_or_make_and_register_DCaggr(cast(*Type_Info_Struct)arg_type);
                if !ag  return false;
                dcArgAggr(vm, ag, (cast(**void)arg_ptr).*);
            }
            
          case .ARRAY;
            ti_array := cast(*Type_Info_Array) arg_type;
            if ti_array.array_type == {
              case .VIEW;
                ag := find_or_make_and_register_DCaggr(type_info(Array_View_64));
                if !ag  return false;
                dcArgAggr(vm, ag, (cast(**void)arg_ptr).*);
                
              case .RESIZABLE;
                ag := find_or_make_and_register_DCaggr(type_info(Resizable_Array));
                if !ag  return false;
                dcArgAggr(vm, ag, (cast(**void)arg_ptr).*);
                
              case;
                if !jai_call {
                    log("Error: cannot pass a fixed array to a #c_call procedure!\n");
                    return false;
                }
                
                ag := find_or_make_and_register_DCaggr(xx ti_array);
                if !ag  return false;
                dcArgAggr(vm, ag, (cast(**void)arg_ptr).*);
            }
            
          case; assert(false, "ERROR: Unimplemented case, unsupported type.");
        }
        
        if is_aggr(arg_type) {
            dbg_print("% % ", as_type(arg_type), Any.{arg_type, (cast(**void)arg_ptr).*});
            arg_ptr += size_of(*void);
        } else {
            dbg_print("% % ", as_type(arg_type), Any.{arg_type, arg_ptr});
            arg_ptr += size_of(*void);
        }
    }
    
    dbg_print("\n");
    
    // TODO: this is dumb, we should just pass the right pointer to this procedure...
    if is_aggr(return_type) {
        dcArgPointer(vm, (cast(**void)return_ptr).*);
    } else {
        dcArgPointer(vm, return_ptr);
    }
    
    dcCallVoid(vm, xx proc_ptr);
    
    if is_aggr(return_type) {
        dbg_print("returning result of procedure: %\n", Any.{ return_type, (cast(**void)return_ptr).* });
    } else {
        dbg_print("returning result of procedure: %\n", Any.{ return_type, return_ptr });
    }
    
    return true;
}


