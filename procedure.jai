
External_Procedure :: struct {
    name:   string;
    type:   *Type_Info_Procedure;
    ptr:    *void;
}

// adds a procedure to the given script's context
register_procedure :: (script: *Script, name: string, procedure: Any) {
    assert(procedure.type.type == .PROCEDURE);
    
    ti_proc := procedure.type.(*Type_Info_Procedure);
    assert(ti_proc.return_types.count <= 1);
    assert(ti_proc.procedure_flags & .IS_C_CALL == 0);
    
    array_add(*script.procedures, .{
        name = name,
        type = xx procedure.type,
        ptr  = procedure.value_pointer.(**void).*
    });
}

/*
    Directives may have as many arguments as they wish, but they must have at least two:
        1. The first argument must be a *Script (receives the currently executing script).
        2. The second argument must be a *void (or any other pointer type) for a user data pointer. 
           This is just something I assume many directives will want, so it's part of the official interface.
           Also, note again that the pointer can be of any type.
    
    Directives may have either 1 or 2 return values. 
    The first may be either a *Node (which will be assumed to have proper storage duration, as the user should just allocate nodes into the script's pool)
        or any other type, in which case the value will be inserted as a literal.
    The last (second, or if only one, then the only) return value must be a bool which denotes whether some error has occured and we need to abort parsing.
    Maybe in the future we will get a little more sophisticated about how directives work, but I think this interface should cover must use cases.
    
    TODO: add logging here so user can see why directive is invalid
*/


Directive :: struct {
    using #as proc: External_Procedure;
    data: [] Any;
}

register_directive :: (script: *Script, name: string, procedure: Any, data: ..Any) -> bool {
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
    if ti_proc.argument_types.count < data.count + 1 {
        log("Error: too many data values provided to directive.");
        return false;
    }
    for data {
        // TODO: allow using #as, maybe also coerce integers/floats
        if it.type != ti_proc.argument_types[it_index + 1] {
            log("Error: type of data value % does not match type of corresponding directive argument.", it_index + 1);
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
        data = data
    });
    
    return true;
}


Procedure_Overload :: struct {
    proc_index:         int;
    coercion_distance:  int;                                    // a score assigned based on how 'far' we have to coerce arguments in order to make the candidate procedure match
    swap_args:          bool;
    reject_reason:      enum { NONE; ARG_COUNT; ARG_TYPE; };    // if .NONE, then candidate is still in the running
    reject_details: union {
        arg_count:  struct { expected: int; provided: int; };
        arg_type:   struct { index: int; expected_type: *Type_Info; argument: *Node; };
    };
}


get_procedure_overloads :: (script: *Script, name: string) -> [] Procedure_Overload {
    overloads: [..] Procedure_Overload;
    for *script.procedures  
        if it.name == name  
            array_add(*overloads, .{ proc_index = it_index });
    return overloads;
}


resolve_procedure_index :: (script: *Script, name: string, arguments: [] *Node, is_operator := false) -> (index: int, swap_args: bool) {
    if is_operator {
        assert(arguments.count == 2, "arguments.count was %", arguments.count);
    }
    
    overloads := get_procedure_overloads(script, name,, temp);
    if !overloads {
        print("Unable to resolve procedure '%'\n", name);
        print("No procedure found with the given identifier.\n");
    }
    
    // typecheck overloads and determine best choice
    best_overload_index     := -1; // indexes overloads, not script.procedures
    best_overload_distance  := S64_MAX; 
    tied_for_best           := 0;
    
    for *overloads {
        proc    := *script.procedures[it.proc_index];
        ti_proc := proc.type.(*Type_Info_Procedure);
        
        // typecheck arguments
        if ti_proc.argument_types.count != arguments.count {
            it.reject_reason = .ARG_COUNT;
            it.reject_details.arg_count = .{ 
                expected = ti_proc.argument_types.count, 
                provided = arguments.count 
            };
            continue;
        }
        
        // returns the coercion distance, else -1 if arguments cannot be made to match through implicit coercions alone
        typecheck_arguments :: (overload: *Procedure_Overload, expected_types: [] *Type_Info, arguments: [] *Node) -> bool {
            // the overall idea on the control flow here is just to `continue` if we are good, else we use the common reject case 
            for expected_type: expected_types {
                argument      := arguments[it_index];
                argument_type := argument.value_type;
                
                if expected_type == argument_type  continue; // ezpz match
                
                if argument_type == null {
                    // we allow arguments to fail typechecking on the first pass so that 
                    // if they are implicitly typed structs, they can still be resolved after overload resolution
                    if expected_type.type == .STRUCT  continue;
                } else {
                    if get_implicit_cast_distance(argument, expected_type) >= 0  continue;
                    if can_do_implicit_reference(argument, expected_type)  continue;
                }
                
                // else argument absolutely does not match expected type
                overload.reject_reason = .ARG_TYPE;
                overload.reject_details.arg_type = .{ 
                    index         = it_index + 1,
                    expected_type = expected_type, 
                    argument      = argument
                };
                return false;
            }
            
            return true;
        }
        
        ok := typecheck_arguments(it, ti_proc.argument_types, arguments);
        
        // fallback case to try reordering args for symmetric case
        if !ok && is_operator && (ti_proc.procedure_flags & .IS_SYMMETRIC) {
            assert(ti_proc.argument_types.count == 2);
            assert(arguments.count == 2);
            it.swap_args = true;
            ok = typecheck_arguments(it, ti_proc.argument_types, .[ arguments[1], arguments[0] ]);
        }
        if !ok  continue;
        
        if it.coercion_distance < best_overload_distance {
            best_overload_index    = it_index;
            best_overload_distance = it.coercion_distance;
            tied_for_best          = 0;
        }
        else if it.coercion_distance == best_overload_distance {
            tied_for_best += 1;
        }
    }
    
    if !tied_for_best && best_overload_index >= 0 {
        chosen_overload := overloads[best_overload_index];
        return chosen_overload.proc_index, chosen_overload.swap_args;
    }
    
    print("Unable to resolve procedure '%'\n", name);
    
    if tied_for_best {
        print("Could not decide between % matching overloads:\n\n", tied_for_best);
        for overloads {
            if it.reject_reason != .NONE 
            || it.coercion_distance != best_overload_distance 
                then continue;
                
            print("%(", name);
            proc := script.procedures[it.proc_index];
            ti_proc := proc.type.(*Type_Info_Procedure);
            for ti_proc.argument_types {
                if it_index != 0  print(", ");
                print("%", as_type(it));
            }
            print(")\n");
        }
    }
    
    if overloads.count > tied_for_best {
        print("% failing overload(s):\n\n", overloads.count);
        for overloads {
            if it.reject_reason == .NONE  continue;
            
            print("%(", name);
            proc := script.procedures[it.proc_index];
            ti_proc := proc.type.(*Type_Info_Procedure);
            for ti_proc.argument_types {
                if it_index != 0  print(", ");
                print("%", as_type(it));
            }
            print(")\n");
            
            if it.reject_reason == {
              case .ARG_COUNT;
                print(
                    "Mismatch on argument count: expected %, got %\n\n", 
                    it.reject_details.arg_count.expected, 
                    it.reject_details.arg_count.provided
                );
                
              case .ARG_TYPE;
                print(
                    "Type mismatch on argument %: expected %, got %\n\n", 
                    it.reject_details.arg_type.index, 
                    as_type(it.reject_details.arg_type.expected_type), 
                    as_type(it.reject_details.arg_type.argument.value_type)
                );
            }
        }
    }
    
    print("Argument types were: ");
    for arguments {
        if it_index != 0  print(", ");
        print("%", as_type(it.value_type));
    }
    print("\n");
    
    return -1, false;
}

