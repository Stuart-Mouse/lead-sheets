
/*
    Directives may have as many arguments as they wish, but they must have at least two:
        1. The first argument must be a *Script (receives the currently executing script).
        2. The second argument must be a *void (or any other pointer type) for a user user_data pointer. 
           This is just something I assume many directives will want, so it's part of the official interface.
           Also, note again that the pointer can be of any type.
    
    Directives may have either 1 or 2 return values. 
    The first may be either a *Node (which will be assumed to have proper storage duration, as the user should just allocate nodes into the script's pool)
        or any other type, in which case the value will be inserted as a literal.
    The last (second, or if only one, then the only) return value must be a bool which denotes whether some error has occured and we need to abort parsing.
    Maybe in the future we will get a little more sophisticated about how directives work, but I think this interface should cover must use cases.
    
    
    
    TODO: 
        directives taking nodes as arguments instead of values
        operator-style directives
        
        
*/

Directive :: struct {
    using #as proc: External_Procedure;
    user_data: [] Any;
}

register_directive :: (script: *Script, name: string, procedure: Any, user_data: ..Any) -> bool {
    ti_proc := procedure.type.(*Type_Info_Procedure);
    if ti_proc.type != .PROCEDURE {
        log("Error: procedure provided was not actually a procedure.");
        return false;
    }
    if ti_proc.procedure_flags & .IS_C_CALL {
        log("Error: #c_call procedures are not currently supported as directives.");
        return false;
    }
    if ti_proc.argument_types.count < 1 {
        log("Error: directive must take at least 1 argument for Script pointer.");
        return false;
    }
    if ti_proc.argument_types[0] != type_info(*Script) {
        log("Error: directive must take a *Script as the first argument.");
        return false;
    }
    if ti_proc.argument_types.count < user_data.count + 1 {
        log("Error: too many user_data values provided to directive.");
        return false;
    }
    for user_data {
        // TODO: allow using #as, maybe also coerce integers/floats
        if it.type != ti_proc.argument_types[it_index + 1] {
            log("Error: type of user_data value % does not match type of corresponding directive argument.", it_index + 1);
            return false;
        }
    }
    if ti_proc.return_types.count > 2
    || ti_proc.return_types[ti_proc.return_types.count-1] != type_info(bool) {
        log("Error: directive must return a bool signifying success as last return value.");
        return false;
    }
    for script.directives {
        if it.name == name {
            log("Error: directives do not support overloading.");
            return false;
        }
    }
    array_add(*script.directives, .{
        name = name,
        type = xx procedure.type,
        ptr  = procedure.value_pointer.(**void).*,
        user_data = user_data
    });
    
    return true;
}


evaluate_directive :: (script: *Script, directive: *Node_Directive) -> bool {
    procedure := script.directives[directive.directive_index];
    ti_proc   := procedure.type;
    
    // index of first argument to procedure which is not already filled in by *Script and user user_data values
    min_arg_index := 1 + procedure.user_data.count;
    
    if ti_proc.argument_types.count != directive.arguments.count + min_arg_index {
        log("Error: incorrect number of arguments provided for directive call. Expected %, got %.\n", ti_proc.argument_types.count - min_arg_index, directive.arguments.count);
        return false;
    }
    
    for directive.arguments {
        // skip typechecking node if argument type intends to receive *Node
        expected_type := ti_proc.argument_types[it_index + min_arg_index];
        if expected_type == type_info(*Node)  continue;
        
        argument_type := typecheck_node(script, it);
        if argument_type == null {
            log("Error: failed to evaluate argument % for directive call.", it+1, );
        }
        if argument_type != expected_type {
            log("Error: type mismatch on argument % for directive call. Expected %, got %.\n", it+1, as_type(expected_type), as_type(argument_type));
            return false;
        }
    }
    
    return_value:   Any;
    return_success: bool;
    if ti_proc.return_types.count > 1 {
        return_value = New_Any(ti_proc.return_types[0],, temp);
    }
    
    arguments_anys := NewArray(directive.arguments.count, Any,, temp);
    for directive.arguments {
        if ti_proc.argument_types[it_index] == type_info(*Node) {
            arguments_anys[it_index] = it; // pass *Node itself
        } else {
            arg_any, ok := evaluate_node(script, it);
            if !ok  return false;
            arguments_anys[it_index] = arg_any;
        }
    }
    
    dcReset(script.dyncall_vm);
    if !(ti_proc.procedure_flags & .HAS_NO_CONTEXT) {
        dcArgPointer(script.dyncall_vm, *context);
    }
    
    if !push_argument(script.dyncall_vm, type_info(*Script), *script)  return false;
    
    for procedure.user_data 
        if !push_argument(script.dyncall_vm, it.type, it.value_pointer, true)  
            return false;
    
    for arguments_anys 
        if !push_argument(script.dyncall_vm, it.type, it.value_pointer, true)  
            return false;
    
    if return_value.type != null {
        dcArgPointer(script.dyncall_vm, return_value.value_pointer);
    }
    dcArgPointer(script.dyncall_vm, *return_success);
    dcCallVoid(script.dyncall_vm, procedure.ptr);
    
    if return_success {
        if return_value.type == {
          case null; // do nothing!
          
          case type_info(*Node);
            // NOTE: node being pointed to is already assumed to be in the script's pool and have valid storage duration
            directive.runtime_node = return_value.value_pointer.(**Node).*;
            
          case;
            directive.runtime_node = make_literal(script, return_value);
        }
    }
    return return_success;
}

