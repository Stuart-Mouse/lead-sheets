
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

// The Any here actually points to the pointer in the given Script_Procedure.
// This could be problematic if we were to modify the script's procedures array and cause a realloc, but that should never happen.
make_procedure_any :: (script_proc: Script_Procedure) -> Any {
    return Any.{ script_proc.type, *script_proc.ptr }; 
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



