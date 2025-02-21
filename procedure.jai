
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

// TODO: maybe we want to add some procedure to sub out the data pointer for a directive?
//       for now this does not really matter since we only parse once, but maybe in the future we will want to do this
Directive :: struct {
    using #as proc: External_Procedure;
    data: *void;
}

register_directive :: (script: *Script, name: string, procedure: Any, data: *void) {
    assert(procedure.type.type == .PROCEDURE);
    
    ti_proc := procedure.type.(*Type_Info_Procedure);
    assert(ti_proc.return_types.count == 1);
    assert(ti_proc.return_types[0] == type_info(bool));
    
    // assert(ti_proc.argument_types.count >= 2);
    assert(ti_proc.argument_types[0] == type_info(*Script));
    // assert(ti_proc.argument_types[1] == type_info(*void));
    assert(ti_proc.procedure_flags & .IS_C_CALL == 0);
    
    array_add(*script.directives, .{
        name = name,
        type = xx procedure.type,
        ptr  = procedure.value_pointer.(**void).*,
        data = data
    });
}


Procedure_Overload :: struct {
    proc_index:         int;
    coercion_distance:  int;                                    // a score assigned based on how 'far' we have to coerce arguments in order to make the candidate procedure match
    swap_args:          bool;
    reject_reason:         enum { NONE; ARG_COUNT; ARG_TYPE; };    // if .NONE, then candidate is still in the running
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
    
    overloads := get_procedure_overloads(script, name);
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
                    if is_numeric_type(expected_type) 
                    && is_numeric_type(argument_type) {
                        if argument.node_type == Node_Literal
                        && argument.(*Node_Literal).literal_type == .NUMBER 
                            then continue;
                        
                        // check if both are the same type of type
                        // if so, we can do a runtime coercion to widen the type, and mark that coercion distance accordingly
                        if expected_type.type == argument_type.type {
                            if expected_type.type == {
                              case .FLOAT;  
                                if expected_type.runtime_size >= argument_type.runtime_size {
                                    overload.coercion_distance += expected_type.runtime_size - argument_type.runtime_size;
                                    continue;
                                }
                                
                              case .INTEGER;
                                // factor signedness into size
                                // TODO: maybe we should actually weight signedness changes higher than size changes?
                                //       maybe float up-casting should be weighed more than int upcasting?
                                //       currently, s8 -> u16 would rate as less distance than u8 -> u16, which seems really wrong
                                expected_size := expected_type.runtime_size + expected_type.(*Type_Info_Integer).signed.(int);
                                argument_size := argument_type.runtime_size + argument_type.(*Type_Info_Integer).signed.(int);
                                if expected_size >= argument_size {
                                    overload.coercion_distance += expected_size - argument_size;
                                    continue;
                                }
                            }
                        }
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

