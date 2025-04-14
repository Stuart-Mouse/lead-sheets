
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
        
        
        maybe directives should accept both a parse and typecheck procedure
            for example, a directive may want to wait until typechecking to execute so that it has access to the full AST
            instead, will pass an enum value in Directive_Call representing which phase of parsing/typechecking we are in
            
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

register_directive :: (script: *Script, name: string, procedure: Any, user_data: ..Any) -> bool, int {
    for script.directives {
        if it.name == name {
            log("Error: directives do not support overloading.");
            return false, -1;
        }
    }
    ti_proc := procedure.type.(*Type_Info_Procedure);
    if ti_proc.type != .PROCEDURE {
        log("Error: procedure provided was not actually a procedure.");
        return false, -1;
    }
    if ti_proc.procedure_flags & .IS_C_CALL {
        log("Error: #c_call procedures are not currently supported as directives.");
        return false, -1;
    }
    if ti_proc.argument_types.count < 1 {
        log("Error: directive must take at least 1 argument for Script pointer.");
        return false, -1;
    }
    if ti_proc.argument_types[0] != type_info(*Directive_Call) {
        log("Error: directive must take a *Script as the first argument.");
        return false, -1;
    }
    if ti_proc.argument_types.count < user_data.count + 1 {
        log("Error: too many user_data values provided to directive.");
        return false, -1;
    }
    for user_data {
        // TODO: allow using #as, maybe also coerce integers/floats
        // if ti_proc.argument_types[it_index + 1] != type_info(Any)
        // && it.type != ti_proc.argument_types[it_index + 1] {
        if it.type != ti_proc.argument_types[it_index + 1] {
            log("Error: type of user_data value % does not match type of corresponding directive argument. % vs %", it_index + 1, as_type(it.type), as_type(ti_proc.argument_types[it_index + 1]));
            return false, -1;
        }
    }
    if ti_proc.return_types.count < 1
    || ti_proc.return_types.count > 2
    || ti_proc.return_types[ti_proc.return_types.count-1] != type_info(bool) {
        log("Error: directive must return a bool signifying success as last return value.");
        return false, -1;
    }
    for script.directives {
        if it.name == name {
            log("Error: directives do not support overloading.");
            return false, -1;
        }
    }
    array_add(*script.directives, .{
        name      = name,
        procedure = Any_Proc.from(procedure),
        user_data = user_data
    });
    return true, script.directives.count-1;
}


evaluate_directive :: (script: *Script, directive: *Node_Directive, phase: Script_Phase) -> bool {
    procedure := script.directives[directive.directive_index];
    ti_proc   := procedure.type;
    
    // ===== typecheck call =====

    // index of first argument to procedure which is not already filled in by *Script and user user_data values
    min_arg_index := 1 + procedure.user_data.count;
    
    // TODO: provide a better error message here, explaining that X number of arguments were filled in by the user_data, etc.
    if ti_proc.argument_types.count != directive.arguments.count + min_arg_index {
        log("Error: incorrect number of arguments provided for directive call. Expected %, got %.\n", ti_proc.argument_types.count - min_arg_index, directive.arguments.count);
        return false;
    }
    
    print("directive.arguments: %\n", directive.arguments);
    
    for directive.arguments {
        // skip typechecking node if argument type intends to receive *Node
        expected_type := ti_proc.argument_types[it_index + min_arg_index];
        print("expecting: %\n", as_type(expected_type));
        if expected_type == type_info(*Node)  continue;
        
        argument_type, error := typecheck_node(script, it);
        if !!error {
            log("Error: failed to evaluate argument % for directive call:\n\t%", it_index+1, format_error(error));
        }
        if argument_type != expected_type {
            log("Error: type mismatch on argument % for directive call. Expected %, got %.\n", it_index+1, as_type(expected_type), as_type(argument_type));
            return false;
        }
    }
    
    return_value:   Any;
    return_success: bool;
    if ti_proc.return_types.count > 1 {
        return_value = New_Any(ti_proc.return_types[0],, temp);
    }
    
    arguments_anys := NewArray(directive.arguments.count, Any,, temp);
    // NOTE: we have to iterate by pointer here, otherwise we get a bug when trying to take an Any of the iterator value.
    for *directive.arguments {
        if ti_proc.argument_types[it_index + min_arg_index] == type_info(*Node) {
            print("arguments_anys[%] = %\n", it_index, it);  
            arguments_anys[it_index] = it.*; // pass *Node itself
            print("arguments_anys: %\n", arguments_anys);
        } else {
            arg_any, ok := evaluate_node(script, it.*);
            if !ok  return false;
            arguments_anys[it_index] = arg_any;
        }
    }
    
    print("arguments_anys: %\n", arguments_anys);
    
    
    // ===== perform call =====
    
    directive_call := Directive_Call.{ phase, script, directive };
    
    dcReset(script.dyncall_vm);
    if !(ti_proc.procedure_flags & .HAS_NO_CONTEXT) {
        dcArgPointer(script.dyncall_vm, *context);
    }
    
    if !push_argument(script.dyncall_vm, type_info(Directive_Call), *directive_call)  return false;
    
    for procedure.user_data {
        if !push_argument(script.dyncall_vm, it.type, it.value_pointer, true)  return false;
    }
    for arguments_anys {
        if !push_argument(script.dyncall_vm, it.type, it.value_pointer, true)  return false;
    }
    
    if return_value.type != null {
        dcArgPointer(script.dyncall_vm, return_value.value_pointer);
    }
    dcArgPointer(script.dyncall_vm, *return_success);
    dcCallVoid(script.dyncall_vm, procedure.pointer);
    
    
    // ===== handle returns =====
    
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

