
/*
    currently binary operations are just sugar for procedure calls
    
    later, I definitely want to add some optimization to remove the dynamic function call overhead for the most simple/common operators  
    For these "builtin" operators, we have some things to figure out
    we want to typecheck operators in the same way we typecheck procedures, before runtime of the script
    perhaps for operators, if it is a builtin operation, then we will leave the operator *proc null as a signifier that we should try a builtin op
    then if we are unable to find such an operation, we will have to error out
*/

// set_basic_operators :: (script: *Script) {
//     {
//         operator + :: (a: $T, b: T) -> type_of(a + b) { return a + b; }
//         operator - :: (a: $T, b: T) -> type_of(a - b) { return a - b; }
//         operator * :: (a: $T, b: T) -> type_of(a * b) { return a * b; }
//         operator / :: (a: $T, b: T) -> type_of(a / b) { return a / b; }
        
//         register_operator(script, "+", #procedure_of_call operator + (cast(float)1.0, cast(float)1.0));
//         register_operator(script, "-", #procedure_of_call operator - (cast(float)1.0, cast(float)1.0));
//         register_operator(script, "*", #procedure_of_call operator * (cast(float)1.0, cast(float)1.0));
//         register_operator(script, "/", #procedure_of_call operator / (cast(float)1.0, cast(float)1.0));
//     }
    
//     register_operator(script, "+", #procedure_of_call operator + (Vector2.{}, Vector2.{}));
//     register_operator(script, "-", #procedure_of_call operator - (Vector2.{}, Vector2.{}));
//     register_operator(script, "*", #procedure_of_call operator * (Vector2.{}, Vector2.{}));
//     register_operator(script, "/", #procedure_of_call operator / (Vector2.{}, Vector2.{}));
    
//     register_operator(script, "*", #procedure_of_call operator * (Vector2.{}, cast(float)1.0));
//     register_operator(script, "/", #procedure_of_call operator / (Vector2.{}, cast(float)1.0));
// }

/*
    We need to specify the return type here so that we can look it up at runtime for the purpose of type-checking scripts.
    It would probably be possible to remove the ened for this, but it would complicate certain sections of code.
*/
BUILTIN_OPERATIONS :: (
    struct { operator_ident: string; left: Type; right: Type; ret: Type; is_symmetric: bool; }
).[
    .{ "+", float, float, float, false },
    .{ "-", float, float, float, false },
    .{ "*", float, float, float, false },
    .{ "/", float, float, float, false },
    
    .{ "+", Vector2, Vector2, Vector2, false },
    .{ "-", Vector2, Vector2, Vector2, false },
    .{ "*", Vector2, Vector2, Vector2, false },
    .{ "/", Vector2, Vector2, Vector2, false },
    .{ "*", Vector2, float,   Vector2, true  },
    .{ "/", Vector2, float,   Vector2, false },
];


resolve_builtin_operation :: (operator_ident: string, left_type: *Type_Info, right_type: *Type_Info) -> (index: int, swap_args: bool) {
    // TODO: could be faster if we generate a switch case, but not going to worry about that for now
    for BUILTIN_OPERATIONS {
        if it.operator_ident != operator_ident  continue;
        
        if cast(*Type_Info) it.left  == left_type 
        && cast(*Type_Info) it.right == right_type {
            return it_index, false;
        }
        
        if it.is_symmetric 
        && cast(*Type_Info) it.left  == right_type 
        && cast(*Type_Info) it.right == left_type {
            return it_index, true;
        }
    }
    return -1, false;
}


exec_builtin_operation :: (oper_idx: int, left: Any, right: Any, ret: Any) -> bool {
    // generate switch case for all builtin operations
    #insert -> string {
        builder: String_Builder;
        
        FORMAT_STRING :: #string DONE
      case %1;
        assert(left.type == type_info(%3));
        assert(right.type == type_info(%4));
        assert(ret.type == type_info(%2));
        (cast(*%2)ret.value_pointer).* = (cast(*%3)left.value_pointer).* %5 (cast(*%4)right.value_pointer).*;
        return true;
DONE;
        
        append(*builder, "    if oper_idx == {\n");
        for BUILTIN_OPERATIONS {
            print_to_builder(*builder, FORMAT_STRING, it_index, it.ret, it.left, it.right, it.operator_ident);
        }
        append(*builder, "    }\n");
        
        return builder_to_string(*builder);
    }
    
    return false;
}


// exec_builtin_operation :: (oper_idx: int, left: Any, right: Any) -> (ret: Any, ok: bool) {
//     ret_ti := cast(*Type_Info) BUILTIN_OPERATIONS[oper_idx].ret;
//     ret := Dynamic_New_Any(ret_ti,, temp);
    
//     // generate switch case for all builtin operations
//     #insert -> string {
//         builder: String_Builder;
        
//         FORMAT_STRING :: #string DONE
//       case %1;
//         assert(left.type == type_info(%3));
//         assert(right.type == type_info(%4));
//         assert(ret.type == type_info(%2));
//         (cast(*%2)ret.value_pointer).* = (cast(*%3)left.value_pointer).* %5 (cast(*%4)right.value_pointer).*;
//         return ret, true;
// DONE;
        
//         append(*builder, "    if oper_idx == {\n");
//         for BUILTIN_OPERATIONS {
//             print_to_builder(*builder, FORMAT_STRING, it_index, it.ret, it.left, it.right, it.operator_ident);
//         }
//         append(*builder, "    }\n");
        
//         return builder_to_string(*builder);
//     }
    
//     return Any.{}, false;
// }

