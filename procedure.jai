
Script_Procedure :: struct {
    name : string;
    proc : #type ([]Internal_Variable_Value) -> (Internal_Variable_Value, bool);
}

/*
    Wrapping Jai procedures for use in scripts.
    Jonathan Blow suggested using LibDynCall, which seems like a pretty good solution, but requires the C calling convention.
    If using the x64 backend, that means we can only use #c_call procedures
    If using LLVM backend, then all Jai procs still get translated to use the C calling convention.
    
    For Jai procedures that get translated to C proc headers:
        multiple returns are passed by pointer after arguments
        all structs and arrays passed by pointer
        
    Probably better to just only use #c_call procedures for now, since that is more straightforward.
    Not to mention that we may not be able to rely on the LLVM backend in perpetuity.
    
    Wrapping procedures through some macro and code gen feels really hacky and ultimately cannot be done runtime dynamically.
    
*/


// wrap_procedure_for_use_in_scripts :: ($proc: Any) -> (Script_Procedure, bool)  {
//     proc_ti :: cast(*Type_Info_Procedure) proc.type;
//     proc_t  : Type : (cast(*Type)*proc_ti).*;
    
//     // check argument type 
//     for (ti_proc.argument_types) {
        
//     }
    
//     __generate_procedure_body :: () -> string {
//         sb: String_Builder;
        
//         print_to_builder(*sb, "(cast(%)proc.value_pointer)(", );
//         for (ti_proc.argument_types) {
            
//         }
//     }
    
    
// }

