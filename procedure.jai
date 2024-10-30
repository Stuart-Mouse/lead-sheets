
Script_Procedure :: struct {
    name : string;
    type : *Type_Info;
    ptr  : *void;
}

// adds a procedure to the given script's context
register_procedure :: (script: *Script, name: string, procedure: Any) {
    assert(procedure.type.type == .PROCEDURE);
    array_add(*script.procedures, .{
        name = name,
        type = procedure.type,
        ptr  = (cast(**void)procedure.value_pointer).*
    });
}

register_operator :: (script: *Script, name: string, procedure: Any) {
    assert(procedure.type.type == .PROCEDURE);
    array_add(*script.operators, .{
        name = name,
        type = procedure.type,
        ptr  = (cast(**void)procedure.value_pointer).*
    });
}

// The Any here actually points to the pointer in the given Script_Procedure.
// This could be problematic if we were to modify the script's procedures array and cause a realloc, but that should never happen.
make_procedure_any :: (script_proc: Script_Procedure) -> Any {
    return Any.{ script_proc.type, *script_proc.ptr }; 
}


resolve_procedure :: (script: *Script, name: string, argument_types: [] *Type_Info) -> (Any, bool) {
    // for error reporting purposes, collect all procedures whose names match, and reason for type mismatch
    // maybe we will put this behind some debug flag later
    Procedure_Resolution_Error :: struct {
        proc    : *Script_Procedure;
        reason  : enum { ARG_COUNT; ARG_TYPE; };
        details : union {
            arg_count : struct { expected: int; provided: int; };
            arg_type  : struct { index: int; expected: *Type_Info; provided: *Type_Info; };
        };
    }
    
    failing_overloads: [..] Procedure_Resolution_Error;
    defer array_free(matching_overloads);
    
    for proc: *script.procedures {
        if proc.name != node.procedure_call.name  continue;
        ti_proc := cast(*Type_Info_Procedure) proc.type;
        
        // typecheck arguments
        if ti_proc.argument_types.count != argument_types.count {
            error := Procedure_Resolution_Error.{ proc = proc, reason = .ARG_COUNT };
            error.details.arg_count = .{ 
                expected = ti_proc.argument_types.count, 
                provided = argument_types.count 
            };
            array_add(*matching_overloads, error);
            
            continue proc;
        }
        for expected: ti_proc.argument_types {
            provided := argument_types[it_index];
            if expected != provided {
                error := Procedure_Resolution_Error.{ proc = proc, reason = .ARG_TYPE };
                error.details.arg_type = .{ 
                    index    = it_index,
                    expected = ti_proc.argument_types.count, 
                    provided = argument_types.count 
                };
                array_add(*matching_overloads, error);
                
                continue proc;
            }
        }
        
        return it, true;
    }
    
    print("Unable to resolve procedure '%'\n", name);
    if failing_overloads {
        print("% failing overload(s):\n\n", failing_overloads.count);
        
        for failing_overloads {
            ti_proc := cast(*Type_Info_Procedure) it.proc.type;
            
            print("%(", name);
            for ti_proc.argument_types {
                if it_index != 0  print(", ");
                print("%", type_info_to_type(it));
            }
            print(")\n");
            
            if reason == {
              case .ARG_COUNT;
                print(
                    "Mismatch on argument count: expected %, got %\n\n", 
                    it.details.arg_count.expected, 
                    it.details.arg_count.provided
                );
                
              case .ARG_TYPE;
                print(
                    "Type mismatch on argument %: expected %, got %\n\n", 
                    it.details.arg_type, 
                    type_info_to_type(it.details.arg_type.expected), 
                    type_info_to_type(it.details.arg_type.provided)
                );
            }
        }
    }
    
    return Any.{}, false;
}



// wraps calls to Dyncall further so that we can provide special checks and error relevant to Lead Sheets
// call_procedure :: (script: *Script, proc: Script_Procedure, param_nodes: [] int) -> (Any, bool) {
//     if !proc.ptr || !proc.type  return false;
    
//     // 
    
//     params := NewArray(node.procedure_call.params.count, Any,, allocator = temp);
//     for node.procedure_call.params {
//         params[it_index] = evaluate_expression(script, expr, it);
//     }
    
//     return DYNAMIC_JAI_CALL(make_procedure_any(proc), );
// }





