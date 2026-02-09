
External_Procedure :: struct {
    using #as procedure_info: Procedure_Info;
    pointer:    *void;
    name:       string;
}

to_any :: (proc: *External_Procedure) -> Any {
    return Any.{ proc.__type_info, *proc.pointer };
}

can_register_procedure :: (proc_info: *Type_Info_Procedure) -> bool {
    return (proc_info.return_types.count <= 1);
}

// To start with, I'm completely separating the dyncall and non-dyncall methods of calling procedures
// in the future I will try to re-integrate them, though that may be slightly tricky because of how Any is preferred over polymorphic parameters when doing overload resolution
// we might have to do some weird thing where we investiage #caller_code to check if 'procedure' is a constant and manually add the call for register_procedure_type
#if USING_DYNCALL {
    // TODO: This is currently unmaintained. I have not tested Lead Sheets with Dyncall in a hot minute...
    register_procedure :: (script: *Script, name: string, $procedure: Code, $scope := #caller_code) {
        // TODO: We will want to have logic for both constant and nonconstant procedure expressions.
        // NOTE: If the procedure is nonconstant, then 
        PROCEDURE      :: #insert,scope(scope) procedure;
        PROCEDURE_TYPE :: type_of(PROCEDURE);
        PROCEDURE_INFO :: #run get_procedure_info(PROCEDURE);
        
        #assert can_register_procedure(type_info(PROCEDURE_TYPE)) "Unable to register procedure.";
        
        register_procedure_type_for_dynamic_call(PROCEDURE_TYPE);
        array_add(*script.procedures, .{ info, xx PROCEDURE, name });
    }
} else {
    register_procedure :: (script: *Script, name: string, $procedure: Code, $scope := #caller_code) {
        PROCEDURE      :: #insert,scope(scope) procedure;
        PROCEDURE_TYPE :: type_of(PROCEDURE);
        PROCEDURE_INFO :: #run get_procedure_info(PROCEDURE, scope);
        
        #assert can_register_procedure(type_info(PROCEDURE_TYPE)) "Unable to register procedure.";
        
        register_procedure_type_for_dynamic_call(PROCEDURE_TYPE);
        array_add(*script.procedures, .{ PROCEDURE_INFO, xx PROCEDURE, name });
    }
}


format_procedure_overload :: (builder: *String_Builder, script: *Script, overload: Procedure_Overload_Report.Entry) #expand {
    proc := script.procedures[overload.proc_index];
    
    print(builder, "%(", proc.name);
    for proc.argument_types {
        if it_index != 0  print(builder, ", ");
        print(builder, "%", as_type(it));
    }
    print(builder, ")\n");
    
    if overload.reject_reason == {
      case .ARG_COUNT;
        print(builder, 
            "Mismatch on argument count: expected %, got %\n\n", 
            overload.reject_details.arg_count.expected, 
            overload.reject_details.arg_count.provided
        );
        
      case .ARG_TYPE;
        print(builder, 
            "Type mismatch on argument %: expected %, got %\n\n", 
            overload.reject_details.arg_type.index, 
            as_type(overload.reject_details.arg_type.expected_type), 
            as_type(overload.reject_details.arg_type.argument.value_type)
        );
    }
}

Procedure_Overload_Report :: struct {
    procedure_call:         *Node_Procedure_Call;
    overloads:              [] Entry;
    
    best_overload_index     := -1; // indexes overloads, not script.procedures
    best_overload_distance  := S64_MAX; 
    tied_for_best           := 0;
    
    Entry :: struct {
        proc_index:         int;
        coercion_distance:  int;                                    // a score assigned based on how 'far' we have to coerce arguments in order to make the candidate procedure match
        swap_args:          bool;
        reject_reason:      enum { NONE; ARG_COUNT; ARG_TYPE; };    // if .NONE, then candidate is still in the running
        reject_details: union {
            arg_count:  struct { expected: int; provided: int; };
            arg_type:   struct { index: int; expected_type: *Type_Info; argument: *Node; };
        };
    }
}

format_procedure_overload_report :: (builder: *String_Builder, script: *Script, using report: Procedure_Overload_Report) {
    if !tied_for_best && best_overload_index >= 0 {
        append(builder, "Succesfully resolved procedure overload.\n");
        print(builder, "Chosen overload: ");
        format_procedure_overload(builder, script, overloads[best_overload_index]);
    }
    
    print(builder, "Unable to resolve overloaded procedure.\n");
    
    if overloads.count == 0 {
        identifier := procedure_call.procedure_expression.(*Node_Identifier);
        assert(identifier.node_type == Node_Identifier);
        assert(identifier.identifier_type == .UNRESOLVED); // so we can access name member validly
        print(builder, "No procedures have been registered with identifier '%'\n\n", identifier.name);
    } else {
        if tied_for_best {
            print(builder, "Could not decide between % matching overloads:\n\n", tied_for_best);
            for overloads {
                if it.reject_reason != .NONE 
                || it.coercion_distance != best_overload_distance 
                    then continue;
                
                format_procedure_overload(builder, script, it);
            }
        }
        if overloads.count > tied_for_best {
            print(builder, "% failing overload(s):\n\n", overloads.count);
            for overloads {
                if it.reject_reason == .NONE  continue;
                format_procedure_overload(builder, script, it);
            }
        }
    }
    
    print(builder, "Provided argument types were: ");
    for procedure_call.arguments {
        if it_index != 0  print(builder, ", ");
        print(builder, "%", as_type(it.value_type));
    }
    print(builder, "\n");
}

get_procedure_overloads :: (script: *Script, name: string) -> [] Procedure_Overload_Report.Entry {
    overloads: [..] Procedure_Overload_Report.Entry;
    for *script.procedures  
        if it.name == name  
            array_add(*overloads, .{ proc_index = it_index });
    return overloads;
}

resolve_procedure_index :: (script: *Script, name: string, procedure_call: *Node_Procedure_Call) -> (index: int, swap_args: bool) {
    arguments := procedure_call.arguments;
    
    is_operator := procedure_call.flags & .OVERLOAD == .OVERLOAD;
    if is_operator {
        assert(arguments.count == 2, "arguments.count was %", arguments.count);
    }
    
    using,except(procedure_call) overload_report: Procedure_Overload_Report;
    overload_report.procedure_call = procedure_call;
    
    overloads = get_procedure_overloads(script, name,, temp);
    if !overloads {
        identifier := procedure_call.procedure_expression.(*Node_Identifier);
        assert(identifier.node_type == Node_Identifier);
        assert(identifier.identifier_type == .UNRESOLVED); // so we can access name member validly
        // set_typecheck_error(script, node = identifier, type = .UNRESOLVED_IDENTIFIER);
        set_typecheck_error(script, node = procedure_call, type = .FAILED_OVERLOAD_RESOLUTION);
        script.error.typecheck.procedure_overload_report = overload_report;
        return -1, false;
    }
    
    for *overloads {
        proc := *script.procedures[it.proc_index];
        
        // typecheck arguments
        if proc.argument_types.count != arguments.count {
            it.reject_reason = .ARG_COUNT;
            it.reject_details.arg_count = .{ 
                expected = proc.argument_types.count, 
                provided = arguments.count 
            };
            continue;
        }
        
        // returns the coercion distance, else -1 if arguments cannot be made to match through implicit coercions alone
        typecheck_arguments :: (script: *Script, overload: *Procedure_Overload_Report.Entry, expected_types: [] *Type_Info, arguments: [] *Node) -> bool {
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
                    if can_do_implicit_reference(script, argument, expected_type)  continue;
                    cast_distance := get_implicit_cast_distance(argument, expected_type);
                    if cast_distance >= 0 {
                        overload.coercion_distance = cast_distance;
                        continue;
                    }
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
        
        ok := typecheck_arguments(script, it, proc.argument_types, arguments);
        
        // fallback case to try reordering args for symmetric case
        if !ok && is_operator && (proc.procedure_flags & .IS_SYMMETRIC) {
            assert(proc.argument_types.count == 2);
            assert(arguments.count == 2);
            ok = typecheck_arguments(script, it, proc.argument_types, .[ arguments[1], arguments[0] ]);
            it.swap_args = ok;
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
    
    set_typecheck_error(script, node = procedure_call, type = .FAILED_OVERLOAD_RESOLUTION);
    script.error.typecheck.procedure_overload_report = overload_report;
    return -1, false;
}



/*
    Unfortunately, this is still necessary if we want to be able to call procedures that are not explicitly exposed as External_Procedure-s.
    This would not be a problem if we were just using dyncall, but unfortunately, we can't use dyncall in our WASM build.
    So for the time being, we will still need to use this system of registering procedure wrappers on a per-type basis.
*/

register_procedure_type_for_dynamic_call :: ($P: Type) #modify {
    return P.(*Type_Info).type == .PROCEDURE, tprint("P must be a procedure type, but it was %", P); 
} {
    for procedure_wrappers if it.type == xx P  return;
    array_add(*procedure_wrappers, .{ xx P, xx #bake_arguments call_procedure_checked(P = P) });
}

procedure_wrappers: [..] struct { type: *Type_Info; wrapper: Procedure_Call_Wrapper; };

try_calling_procedure_with_wrapper :: (proc_info: *Type_Info_Procedure, proc_ptr: *void, arguments: [] Any, return_values: [] Any) -> bool {
    for procedure_wrappers {
        if it.type == proc_info {
            return it.wrapper(proc_ptr, arguments, return_values);
        }
    }
    log("Error: % has not been registered for use in a dynamic call.", (*proc_info).(*Type).*);
    return false;
}