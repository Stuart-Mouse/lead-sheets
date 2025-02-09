
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


resolve_procedure_index :: (script: *Script, name: string, argument_types: [] *Type_Info, is_operator := false) -> (index: int, swap_args: bool) {
    // for error reporting purposes, collect all procedures whose names match, and reason for type mismatch
    // maybe we will put this behind some debug flag later
    Procedure_Resolution_Error :: struct {
        proc: *External_Procedure;
        error_type: enum { NONE; ARG_COUNT; ARG_TYPE; };
        details: union {
            arg_count : struct { expected: int; provided: int; };
            arg_type  : struct { index: int; expected: *Type_Info; provided: *Type_Info; };
        };
    }
    
    failing_overloads: [..] Procedure_Resolution_Error;
    defer array_free(failing_overloads);
    
    if is_operator {
        assert(argument_types.count == 2, "argument_types.count was %", argument_types.count);
    }
    
    proc_list := script.procedures;
    for *proc, proc_index: proc_list {
        if proc.name != name  continue proc;
        ti_proc := proc.type.(*Type_Info_Procedure);
        
        // typecheck arguments
        if ti_proc.argument_types.count != argument_types.count {
            error := Procedure_Resolution_Error.{ proc = proc, error_type = .ARG_COUNT };
            error.details.arg_count = .{ 
                expected = ti_proc.argument_types.count, 
                provided = argument_types.count 
            };
            array_add(*failing_overloads, error);
            
            continue proc;
        }
        
        typecheck_arguments :: (script_proc: *External_Procedure, expected_list: [] *Type_Info, provided_list: [] *Type_Info) -> (Procedure_Resolution_Error, bool) {
            for expected: expected_list {
                provided := provided_list[it_index];
                if expected != provided {
                    error := Procedure_Resolution_Error.{ proc = script_proc, error_type = .ARG_TYPE };
                    error.details.arg_type = .{ 
                        index    = it_index + 1,
                        expected = expected, 
                        provided = provided
                    };
                    return error, false;
                }
            }
            return .{}, true;
        }
        
        swap_args := false;
        error, ok := typecheck_arguments(proc, ti_proc.argument_types, argument_types);
        
        // fallback case to try reordering args for symmetric case
        if !ok && is_operator {
            if (ti_proc.procedure_flags & .IS_SYMMETRIC) {
                assert(ti_proc.argument_types.count == 2);
                assert(argument_types.count == 2);
                
                swap_args = true;
                error, ok = typecheck_arguments(proc, ti_proc.argument_types, .[ argument_types[1], argument_types[0] ]);
            }
        }
        
        if !ok {
            array_add(*failing_overloads, error);
            continue proc;
        }
        
        return proc_index, swap_args;
    }
    
    print("Unable to resolve procedure '%'\n", name);
    if failing_overloads {
        print("% failing overload(s):\n\n", failing_overloads.count);
        
        for failing_overloads {
            ti_proc := it.proc.type.(*Type_Info_Procedure);
            
            print("%(", name);
            for ti_proc.argument_types {
                if it_index != 0  print(", ");
                print("%", as_type(it));
            }
            print(")\n");
            
            if it.error_type == {
              case .ARG_COUNT;
                print(
                    "Mismatch on argument count: expected %, got %\n\n", 
                    it.details.arg_count.expected, 
                    it.details.arg_count.provided
                );
                
              case .ARG_TYPE;
                print(
                    "Type mismatch on argument %: expected %, got %\n\n", 
                    it.details.arg_type.index, 
                    as_type(it.details.arg_type.expected), 
                    as_type(it.details.arg_type.provided)
                );
            }
        }
    } else {
        print("No procedure found with the given identifier.\n");
    }
    
    print("Argument types were: ");
    for argument_types {
        if it_index != 0  print(", ");
        print("%", as_type(it));
    }
    print("\n");
    
    return -1, false;
}





