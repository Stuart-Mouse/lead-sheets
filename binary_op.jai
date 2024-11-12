
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

// this version takes only a single pointer (pointing to ret, left, and right, in that order)
// thi swill be used during execution of bytecode, since we can safely presume that return value and arguments will be in proper location
execute_builtin_operation :: (oper_idx: int, stack_ptr_ptr: **void, swap_args := false) -> bool {
    pop :: inline (stack_ptr_ptr: **void, T: Type) -> *T {
        stack_ptr_ptr.* -= size_of(T);
        return cast(*T) stack_ptr_ptr.*;
    }
    
    // generate switch case for all builtin operations
    #insert -> string {
        builder: String_Builder;
        
        FORMAT_STRING :: #string DONE
      case %1;
        right := pop(stack_ptr_ptr, %4);
        left  := pop(stack_ptr_ptr, %3);
        if swap_args  left, right = xx right, xx left;
        ret   := cast(*%2) stack_ptr_ptr.*;
        ret.*  = left.* %5 right.*;
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


