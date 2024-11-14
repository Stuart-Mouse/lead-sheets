
/*
    We need to specify the return type here so that we can look it up at runtime for the purpose of type-checking scripts.
    It would probably be possible to remove the need for this, but it would complicate certain sections of code.
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

Operation_Flags :: enum_flags { SWAP_ARGS; IS_BUILTIN; };

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

execute_builtin_operation :: (oper_idx: int, left: *void, right: *void, ret: *void) -> bool {
    // generate switch case for all builtin operations
    #insert -> string {
        builder: String_Builder;
        
        FORMAT_STRING :: #string DONE
      case %1;
        (cast(*%2)ret).* = (cast(*%3)left).* %5 (cast(*%4)right).*;
        return true;
DONE;
        
        append(*builder, "    if oper_idx == {\n");
        for BUILTIN_OPERATIONS {
            print_to_builder(*builder, FORMAT_STRING, it_index, it.ret, it.left, it.right, it.operator_ident);
        }
        append(*builder, "    }\n");
        
        return builder_to_string(*builder);
    }
    
    assert(false, "unreachable");
    return false;
}

execute_builtin_operation :: (oper_idx: int, stack_ptr: **void) -> bool {

    dbg_print("stack_ptr: %\n", (cast(**void)stack_ptr).*);

    // generate switch case for all builtin operations
    #insert -> string {
        builder: String_Builder;
        
//         FORMAT_STRING :: #string DONE
//       case %1;
//         right := pop_value(stack_ptr, %4);
//         left  := pop_value(stack_ptr, %3);
//         ret   := pop_value(stack_ptr, %2);
//         ret.*  = left.* %5 right.*;
//         return true;
// DONE;
        
        append(*builder, "if oper_idx == {\n");
        for BUILTIN_OPERATIONS {
            left_is_aggr  := (cast(*Type_Info)it.left ).type == .STRUCT;
            right_is_aggr := (cast(*Type_Info)it.right).type == .STRUCT;
            ret_is_aggr   := (cast(*Type_Info)it.ret  ).type == .STRUCT;
            
            print_to_builder(*builder, "case %;\n", it_index);
            print_to_builder(*builder, "  dbg_print(\"op: % % %\\n\");\n", it.left, it.operator_ident, it.right);
            
            print_to_builder(*builder, "  right := cast(*%) stack_pop(stack_ptr, %);\n", it.right, right_is_aggr);
            print_to_builder(*builder, "  left  := cast(*%) stack_pop(stack_ptr, %);\n", it.left, left_is_aggr);
            print_to_builder(*builder, "  ret   := cast(*%) stack_pop(stack_ptr, %);\n", it.ret, ret_is_aggr);
            
            print_to_builder(*builder, "  dbg_print(\"left  (%%): % %%\\n\", left,  left.*);\n", it.left);
            print_to_builder(*builder, "  dbg_print(\"right (%%): % %%\\n\", right, right.*);\n", it.right);
            
            print_to_builder(*builder, "  ret.* = left.* % right.*;\n", it.operator_ident);
            print_to_builder(*builder, "  dbg_print(\"ret   (%%): % %%\\n\", ret,   ret.*);\n", it.ret);
            
            print_to_builder(*builder, "  stack_push(stack_ptr, xx *ret, %);\n", ret_is_aggr);
            
            append(*builder, "  return true;\n");
        }
        append(*builder, "}\n");
        
        return builder_to_string(*builder);
    }
    
    assert(false, "unreachable");
    return false;
}

