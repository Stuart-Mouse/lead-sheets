

// TODO: probably remove the need for Internal_Variable_Value and instead use Any
// OR, if we really want to enumerate all the types that can be used in this way, we need to add some means to dyncall to get the return values stored directly into this union
// We can probably do both at some point tbh

Internal_Variable_Value :: Tagged_Union(float32, Vector2);

// Custom code to set tagged union using data types not explicitly allowed in union
// We have this so that the user can use any integer/float types as external variables in scripts
set_internal_variable_value :: (ivv: *Internal_Variable_Value, value: Any) -> bool {
    if set_any(ivv, value)  return false;
    
    f32_value, ok := get_numeric_value_as_f32(value);
    if !ok  return false;

    set(ivv, f32_value);
    return true;
}

set_any_from_internal_variable_value :: (dst: Any, ivv: *Internal_Variable_Value) -> bool {
    if !dst.value_pointer  return false;

    value := get_as_any(ivv);
    if !value.value_pointer {
        print("failed to get internal variable as any\n");
        return false;
    }
    
    print("dst type: %, value type: %\n", (cast(*Type)*dst.type).*, (cast(*Type)*value.type).*);
    if dst.type == value.type {
        memcpy(dst.value_pointer, value.value_pointer, dst.type.runtime_size);
        return true;
    }
    
    if value.type == cast(*Type_Info)float32 {
        return set_numeric_value_from_f32(dst, (cast(*float32)value.value_pointer).*);
    }
    
    print("failed to set any from internal variable\n");
    
    return false;
}

Binary_Operation_Signature :: struct { 
    operation : u8;
    operands  : [2]Type; 
    returns   : Type; 
};

DEFINED_OPERATIONS :: Binary_Operation_Signature.[
    .{ #char "+", .[ float32, float32 ], float32 },
    .{ #char "+", .[ Vector2, Vector2 ], Vector2 },
    .{ #char "-", .[ float32, float32 ], float32 },
    .{ #char "-", .[ Vector2, Vector2 ], Vector2 },
    .{ #char "*", .[ float32, float32 ], float32 },
    .{ #char "*", .[ Vector2, float32 ], Vector2 },
    .{ #char "*", .[ Vector2, Vector2 ], Vector2 },
    .{ #char "/", .[ float32, float32 ], float32 },
    .{ #char "/", .[ Vector2, float32 ], Vector2 },
    .{ #char "/", .[ Vector2, Vector2 ], Vector2 },
];

/*
    TODO: in the two below procedures, we should probably just inline the __generate_body procs
    we should proabbyl also just iterate over all the items in the defined operations array each time, rather than constructing the temporary dynamic arrays
*/

typecheck_binary_operation :: (
    op  :  u8,
    a   :  Type, 
    b   :  Type
) -> Type, bool {
    __generate_body :: (op: u8) -> string #compile_time {
        sigs: [..] Binary_Operation_Signature;
        defer array_free(sigs);
        
        for DEFINED_OPERATIONS 
            if it.operation == op 
                array_add(*sigs, it);
        
        FORMAT_STRING := #string DONE
        if (a == %1 && b == %2)  return %3, true;
        if (b == %1 && a == %2)  return %3, true;
        DONE
        
        sb: String_Builder;
        for sigs {
            print_to_builder(*sb, FORMAT_STRING, 
                it.operands[0], it.operands[1], it.returns
            );
        }
        return builder_to_string(*sb);
    }
    
    // generate switch case for all possible operations, 
    // so that 'op' param to this proc can be non-constant
    #insert -> string {
        ops: [..] u8;
        defer array_free(ops);
            
        for DEFINED_OPERATIONS  array_add_if_unique(*ops, it.operation);
            
        sb: String_Builder;
        append(*sb, "if op == {\n");
        for ops {
            print_to_builder(*sb, "    case #char \"%1\"; #insert #run __generate_body(#char \"%1\");\n", string.{ 1, *it });
        }
        append(*sb, "}\n");
        return builder_to_string(*sb);
    }
    
    return false;
}

do_binary_operation :: (
    op  :  u8,
    out : *Internal_Variable_Value, 
    a   :  Internal_Variable_Value, 
    b   :  Internal_Variable_Value
) -> bool {
    __generate_body :: (op: u8) -> string #compile_time {
        sigs: [..] Binary_Operation_Signature;
        defer array_free(sigs);
        
        for DEFINED_OPERATIONS 
            if it.operation == op 
                array_add(*sigs, it);
        
        /*
            This could probably be much more efficient if we generate a switch case, 
                but for now this will function and I proably won't have to debug it.
        */
        FORMAT_STRING := #string DONE
        if (a.tag == %1 && b.tag == %2) {
            result := a._%1 %4 b._%2;
            #assert(type_of(result) == %3);
            set(out, result); 
            return true;
        }
        if (b.tag == %1 && a.tag == %2) {
            result := b._%1 %4 a._%2;
            #assert(type_of(result) == %3);
            set(out, result); 
            return true;
        }
        DONE
        
        sb: String_Builder;
        for sigs {
            print_to_builder(*sb, FORMAT_STRING, 
                it.operands[0], it.operands[1], it.returns, string.{ 1, *op }
            );
        }
        return builder_to_string(*sb);
    }
    
    // generate switch case for all possible operations, 
    // so that 'op' param to this proc can be non-constant
    #insert -> string {
        ops: [..] u8;
        defer array_free(ops);
            
        for DEFINED_OPERATIONS  array_add_if_unique(*ops, it.operation);
            
        sb: String_Builder;
        append(*sb, "if op == {\n");
        for ops {
            print_to_builder(*sb, "    case #char \"%1\"; #insert #run __generate_body(#char \"%1\");\n", string.{ 1, *it });
        }
        append(*sb, "}\n");
        return builder_to_string(*sb);
    }
    
    return false;
}
