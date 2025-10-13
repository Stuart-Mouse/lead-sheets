
/*
    Directives are just procedures that are evaluated as soon as they are parsed.
    Syntactically, all directives follow the form of a procedure call, but the name of the directive is prefixed with a `#`.
    
    Directives have a very flexible interface, allowing the user to pass as many arguments as they wish, 
        and optionally, to fill in some or all of those arguments with user data values.
    Directives can receive arguments directly as a *Node, manipulate those nodes, and return new nodes to replace the directive at runtime.
    Because they receive a pointer to the script, directives can theoretically be used to implement custom parsing logic (although I have not tried this yet myself).
    
    
    Interface Rules:
        Currently, only native Jai procedures are supported (not #c_call). This will likely change in the future.
        Unlike other procedures, directives do not support overloading (because the interface is already so loosey-goosey).
        The first argument must always be a *Directive_Call, which receives the currently executing script and the directive node as context.
        The next N arguments are then filled in by the provided user data values.
        The remaining arguments are to be provided as parameters to the directive call in the script text itself.
        User data values are provided as Any's, so the underlying values must remain valid for the lifetime of the script.
        All user data values are typechecked against the procedure signature, because I don't trust you (or myself) not to slip up.
        Whether an expression passed as a parameter to a directive is received as an evaluated value or as raw nodes is determined by the formal type of argument.
            If the argument's formal type is *Node, then the raw nodes will be provided, otherwise the argument expression will be evaluated and typechecked before being passed to the directive.
        The directive may return either one or two values:
            The first value is optional, and may be of any type. 
                If it is a *Node, that node will replace the directive for the purposes of typechecking and runtime execution. 
                Otherwise, the returned value will be inserted as a literal.
            The second (or only) value must be a bool which represent the success or failure of the directive.
                If this value is false, the script will cease parsing and report the error.
    
    IMPORTANT: 
        Directives are always called twice, once during parsing and once during typechecking.
        In order to ensure that the main logic of your directive is not doubyl executed, you need to check the 'phase' value passed in the Directive_Call.
    
    
    TODO: 
        directive should be able to accept Any as a valid argument type as well
            will require changes in dyncall. I guess I overlooked Any's as procedure arguments
        operator-style directives
        option to sub-out or reset directive user_data if need be
            user can do this manually if they're getting that technical
        
        consider if we should just remove the extra complication around return types setting the replacement node and just let the user do that manually in the directive body
            since the user is given a pointer to the directive node, he can just set it himself...
*/

Script_Phase :: enum { PARSE; TYPECHECK; /*.LOWER_TO_BYTECODE;*/ };


Directive :: struct {
    using #as base: External_Procedure;
    user_data: [] Any;
}

Directive_Call :: struct {
    phase:          Script_Phase;
    script:         *Script;
    directive_node: *Node_Directive;
}


can_register_directive :: (proc_info: *Type_Info_Procedure) -> bool {
    if proc_info.procedure_flags & .IS_C_CALL {
        log("Error: #c_call procedures are not currently supported as directives.");
        return false;
    }
    if proc_info.argument_types.count < 1 {
        log("Error: directive must take at least 1 argument for Script pointer.");
        return false;
    }
    if proc_info.argument_types[0] != type_info(*Directive_Call) {
        log("Error: directive must take a *Directive_Call as the first argument.");
        return false;
    }
    if proc_info.return_types.count < 1
    || proc_info.return_types.count > 2
    || proc_info.return_types[0] != type_info(bool) {
        log("Error: directive must return a bool signifying success as first return value.");
        return false;
    }
    return true;
}

#if USING_DYNCALL {
    register_directive :: (script: *Script, name: string, procedure: Any, user_data: ..Any) {
        assert(procedure.type.type == .PROCEDURE && can_register_directive(xx procedure.type));
        
        for script.directives {
            if it.name == name {
                assert(false, "Error: directives do not support overloading.");
            }
        }
        
        proc_info := procedure.type.(*Type_Info_Procedure);
        if proc_info.argument_types.count < user_data.count + 1 {
            log("Error: too many user_data values provided to directive.");
            return;
        }
        for user_data {
            // TODO: allow using #as, maybe also coerce integers/floats
            // if proc_info.argument_types[it_index + 1] != type_info(Any)
            // && it.type != proc_info.argument_types[it_index + 1] {
            if it.type != proc_info.argument_types[it_index + 1] {
                assert(false, "Error: type of user_data value % does not match type of corresponding directive argument. % vs %", it_index + 1, as_type(it.type), as_type(proc_info.argument_types[it_index + 1]));
            }
        }
        
        array_add(*script.directives, .{
            name      = name,
            procedure = Any_Proc.from(procedure),
            user_data = user_data
        });
    }
} else {
    // TODO: could use #caller_code or soemthing to statically check that user_data arguments are of proper types
    register_directive :: inline (script: *Script, name: string, procedure: $P, user_data: ..Any) #modify {
        proc_info := P.(*Type_Info_Procedure);
        if proc_info.type != .PROCEDURE  return false, "'procedure' must be a procedure! What are you stupid?";
        if !can_register_directive(proc_info)  return false, tprint("Unable to register procedure of type: %", P);
        return true;
    } {
        for script.directives {
            if it.name == name {
                assert(false, "Error: directives do not support overloading.");
            }
        }
        
        proc_info := P.(*Type_Info_Procedure);
        for user_data {
            // TODO: allow using #as, maybe also coerce integers/floats
            // if proc_info.argument_types[it_index + 1] != type_info(Any)
            // && it.type != proc_info.argument_types[it_index + 1] {
            if it.type != proc_info.argument_types[it_index + 1] {
                assert(false, "Error: type of user_data value % does not match type of corresponding directive argument. % vs %", it_index + 1, as_type(it.type), as_type(proc_info.argument_types[it_index + 1]));
            }
        }
        
        register_procedure_type(P);
        array_add(*script.directives, .{
            procedure = Any_Proc.from(procedure),
            name      = name,
            user_data = user_data
        });
    }
}


evaluate_directive :: (script: *Script, directive: *Node_Directive, phase: Script_Phase) -> bool {
    procedure := script.directives[directive.directive_index];
    directive_call := Directive_Call.{ phase, script, directive };
    
    // ===== typecheck call =====
    
    first_arg_index := 1 + procedure.user_data.count;
    if procedure.argument_types.count != directive.arguments.count + first_arg_index {
        log("Error: Incorrect number of arguments provided for directive call. Expected % total, % were pre-filled by user_data, got %.\n", procedure.argument_types.count, procedure.user_data.count, directive.arguments.count);
        return false;
    }
    
    for directive.arguments {
        // skip typechecking node if argument type intends to receive *Node
        expected_type := procedure.argument_types[it_index + first_arg_index];
        if expected_type == type_info(*Node)  continue;
        
        argument_type := typecheck_node(script, it);
        if has_error(script) {
            log("Error: failed to evaluate argument % for directive call:\n\t%", it_index+1, format_error(script));
            return false;
        }
        if argument_type != expected_type {
            log("Error: type mismatch on argument % for directive call. Expected %, got %.\n", it_index+1, as_type(expected_type), as_type(argument_type));
            return false;
        }
    }
    
    
    // ===== prepare arguments =====
    
    arguments := NewArray(procedure.argument_types.count, Any,, temp);
    {
        arguments[0] = *directive_call;
        argument_index := 1;
        
        for procedure.user_data {
            arguments[argument_index] = it;
            argument_index += 1;
        }
        
        for *directive.arguments {
            if procedure.argument_types[it_index + first_arg_index] == type_info(*Node) {
                arguments[argument_index] = it.*; // pass *Node itself
            } else {
                arguments[argument_index] = script.my_evaluate_node(script, it.*);
                if has_error(script)  return false;
            }
            argument_index += 1;
        }
    }
    
    
    // ===== prepare return values =====
    
    // TODO: allow multiple user return values?
    return_success: bool;
    return_value:   Any;
    return_values:  [] Any;
    
    if procedure.return_types.count > 1 {
        return_value = New_Any(procedure.return_types[1],, temp);
        return_values = Any.[ return_success, return_value ];
    } else {
        return_values = Any.[ return_success ];
    }
    
    
    // ===== perform call =====
    
    #if USING_DYNCALL {
        if !do_dyncall(script.dyncall_vm, to_any(procedure), arguments, return_values) {
            set_general_error(script, "Failed while trying to make dyncall.");
            return false;
        }
    } else {
        if !try_calling_procedure_with_wrapper(procedure.proc_info, procedure.pointer, arguments, return_values) {
            log("Error: Unable to find wrapper for procedure of type: %", as_type(procedure.proc_info));
            return false;
        }
    }
    
    
    // ===== handle return values =====
    
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

