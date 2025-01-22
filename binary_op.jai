
/*
    We need to specify the return type here so that we can look it up at runtime for the purpose of type-checking scripts.
    It would probably be possible to remove the need for this, but it would complicate certain sections of code.
    
    NOTE: For builtin unary operations, we just specify right type as null.
*/
BUILTIN_OPERATIONS :: (
    struct { operator_ident: string; left: Type; right: Type; ret: Type; is_symmetric: bool; }
).[
    // UNARY    (we have to init by name here because we can't use null as a value of Type)
    .{ operator_ident = "-", left = int,   ret = int   },
    .{ operator_ident = "-", left = float, ret = float },
    
    // BINARY
    .{ "+", int, int, int, false },
    .{ "-", int, int, int, false },
    .{ "*", int, int, int, false },
    .{ "/", int, int, int, false },
    
    .{ ">",  int, int, bool, false },
    .{ "<",  int, int, bool, false },
    .{ "==", int, int, bool, false },
    .{ ">=", int, int, bool, false },
    .{ "<=", int, int, bool, false },
    
    .{ "+", float, float, float, false },
    .{ "-", float, float, float, false },
    .{ "*", float, float, float, false },
    .{ "/", float, float, float, false },
    
    .{ ">",  float, float, bool, false },
    .{ "<",  float, float, bool, false },
    .{ "==", float, float, bool, false },
    .{ ">=", float, float, bool, false },
    .{ "<=", float, float, bool, false },
    
    .{ "&&", bool, bool, bool, false },
    .{ "||", bool, bool, bool, false },
    
    .{ "+", Vector2, Vector2, Vector2, false },
    .{ "-", Vector2, Vector2, Vector2, false },
    .{ "*", Vector2, Vector2, Vector2, false },
    .{ "/", Vector2, Vector2, Vector2, false },
    .{ "*", Vector2, float,   Vector2, true  },
    .{ "/", Vector2, float,   Vector2, false },
    
    .{ "==", Vector2, Vector2, bool, false },
    
    
];

resolve_builtin_operation :: (operator_ident: string, left_type: *Type_Info, right_type: *Type_Info) -> (index: int, swap_args: bool) {
    for BUILTIN_OPERATIONS {
        if it.operator_ident != operator_ident  continue;
        
        if it.left .(*Type_Info) == left_type 
        && it.right.(*Type_Info) == right_type {
            return it_index, false;
        }
        
        if it.is_symmetric 
        && it.left .(*Type_Info) == right_type 
        && it.right.(*Type_Info) == left_type {
            return it_index, true;
        }
    }
    return -1, false;
}

// execute_builtin_operation :: (oper_idx: int, left: *void, right: *void, ret: *void) -> bool {
//     // generate switch case for all builtin operations
//     #insert -> string {
//         builder: String_Builder;
        
//         FORMAT_STRING :: #string DONE
//       case %1;
//         ret.(*%2).* = left.(*%3).* %5 right.(*%4).*;
//         return true;
// DONE;
        
//         append(*builder, "    if oper_idx == {\n");
//         for BUILTIN_OPERATIONS {
//             print_to_builder(*builder, FORMAT_STRING, it_index, it.ret, it.left, it.right, it.operator_ident);
//         }
//         append(*builder, "    }\n");
        
//         return builder_to_string(*builder);
//     }
    
//     assert(false, "Called execute_builtin_operation with an invalid operation index.");
//     return false;
// }

// this version may not be worth it
// because we will still probably need the other version for eval procs
// and this one creates more code bloat
// but, it does bake the stack pops so that we don't have to lookup the operator types 
// this is not a big deal for the exec procs, but for the bytecode, it could be a considerable performance loss 
// in any case, I will probably leave code to use the other version commented out in execute_node so I at least know the we have the option to get rid of this at any time
execute_builtin_operation :: (script: *Script, oper_idx: int) {
    
    dbg_print("stack_ptr: %\n", script.stack_ptr);
    
    // generate switch case for all builtin operations
    #insert -> string {
        builder: String_Builder;
        
        append(*builder, "if oper_idx == {\n");
        for BUILTIN_OPERATIONS {
            print_to_builder(*builder, "case %;\n", it_index);
            
            // unary operations
            // TODO: may have to disambiguate later on prefix/postfix
            if it.right.(*void) == null {
                print_to_builder(*builder, "  left  := stack_pop (script, %).(*%);\n",  is_aggr(it.left),  it.left);
                print_to_builder(*builder, "  ret   := stack_peek(script, %).(*%);\n", is_aggr(it.ret),   it.ret);
                print_to_builder(*builder, "  ret.* = %left.*;\n", it.operator_ident);
            }
            // binary operations
            else {
                print_to_builder(*builder, "  right := stack_pop (script, %).(*%);\n",  is_aggr(it.right), it.right);
                print_to_builder(*builder, "  left  := stack_pop (script, %).(*%);\n",  is_aggr(it.left),  it.left);
                print_to_builder(*builder, "  ret   := stack_peek(script, %).(*%);\n", is_aggr(it.ret),   it.ret);
                
                print_to_builder(*builder, "  ret.* = left.* % right.*;\n", it.operator_ident);
            }
            
            append(*builder, "  return;\n");
        }
        append(*builder, "}\n");
        
        return builder_to_string(*builder);
    }
    
    assert(false, "Called execute_builtin_operation with an invalid operation index.");
    // return false;
}

