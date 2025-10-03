
get_prefix_operator :: (token: Token) -> bool, int {
    for operator_table {
        if it.kind != .PREFIX  continue;
        if it.token_type == token.type {
            return true, it_index;
        }
    }
    return false, 0;
}

get_postfix_operator :: (token: Token) -> bool, int {
    for operator_table {
        if it.kind != .POSTFIX  continue;
        if it.token_type == token.type {
            return true, it_index;
        }
    }
    return false, 0;
}

get_binary_operator :: (token: Token) -> bool, int {
    for operator_table {
        if is_unary(it) continue;
        if it.token_type == token.type {
            return true, it_index;
        }
    }
    return false, -1;
}


// TODO: use token_type instead of identifier
// then we can also a separate lookup table to get the text for an operator from the token_type
operator_table :: Operator.[
    .{ .ASSIGNMENT, .ASSIGN_EQUAL,  "=",  1, .NONE },
    .{ .ASSIGNMENT, .PLUS_EQUALS,   "+=", 1, .NONE },
    .{ .ASSIGNMENT, .MINUS_EQUALS,  "-=", 1, .NONE },
    .{ .ASSIGNMENT, .OR_EQUALS,     "|=", 1, .NONE },
    .{ .ASSIGNMENT, .AND_EQUALS,    "&=", 1, .NONE },
    .{ .ASSIGNMENT, .XOR_EQUALS,    "^=", 1, .NONE },
    
    .{ .BINARY, .LOGICAL_AND, "&&", 2, .NONE },
    .{ .BINARY, .LOGICAL_OR,  "||", 2, .NONE },
    
    .{ .BINARY, .COMPARE_EQUAL,             "==", 3, .NONE },
    .{ .BINARY, .GREATER_THAN_OR_EQUAL_TO,  ">=", 3, .NONE },
    .{ .BINARY, .LESS_THAN_OR_EQUAL_TO,     "<=", 3, .NONE },
    .{ .BINARY, .GREATER_THAN,              ">",  3, .NONE },
    .{ .BINARY, .LESS_THAN,                 "<",  3, .NONE },
    
    .{ .BINARY, .BINARY_OR,  "|",  4, .NONE },
    .{ .BINARY, .BINARY_AND, "&",  4, .NONE },
    .{ .BINARY, .BINARY_NOT, "~",  4, .NONE },
    .{ .BINARY, .BINARY_XOR, "^",  4, .NONE },
    
    .{ .BINARY, .ADD, "+",  5, .NONE },
    .{ .BINARY, .SUB, "-",  5, .NONE },
    .{ .BINARY, .MUL, "*",  6, .NONE },
    .{ .BINARY, .DIV, "/",  6, .NONE },
    
    .{ .PREFIX, .SUB,        "-",  7, .NONE },
    .{ .PREFIX, .BINARY_NOT, "~",  7, .NONE },
    
    // added for LSD files
    .{ .PREFIX, .BINARY_AND, "&",  8, .NONE },
    .{ .PREFIX, .MUL,        "*",  8, .NONE },
    .{ .PREFIX, .DOLLAR,     "$",  8, .NONE },
];

Operator :: struct {
    kind:           Kind;
    token_type:     Token_Type;
    token_text:     string;
    precedence:     int;
    flags:          Flags;
    
    Kind :: enum { UNINITIALIZED; BINARY; PREFIX; POSTFIX; ASSIGNMENT; };
    Flags :: enum_flags { NONE :: 0; DIRECTIVE :: 1; };
}

is_unary :: inline (kind: Operator.Kind) -> bool { return kind == .PREFIX || kind == .POSTFIX; }

is_unary :: inline (op: Operator) -> bool { return is_unary(op.kind); }

// this is dumb, but it must be done (for now)
#insert -> string {
    builder: String_Builder;

    append(*builder, #string JAI
    BUILTIN_OPERATIONS :: (
        struct { type: Operator.Kind; identifier: string; left: Type; right: Type; ret: Type; is_symmetric: bool; }
    ).[
    JAI);
    
    integer_template :: #string JAI
    .{ .BINARY, "+", %1, %1, %1, false },
    .{ .BINARY, "-", %1, %1, %1, false },
    .{ .BINARY, "*", %1, %1, %1, false },
    .{ .BINARY, "/", %1, %1, %1, false },
    
    .{ .BINARY, ">",  %1, %1, bool, false },
    .{ .BINARY, "<",  %1, %1, bool, false },
    .{ .BINARY, "==", %1, %1, bool, false },
    .{ .BINARY, ">=", %1, %1, bool, false },
    .{ .BINARY, "<=", %1, %1, bool, false },
    
    .{ .BINARY, "|", %1, %1, %1, false },
    .{ .BINARY, "&", %1, %1, %1, false },
    .{ .BINARY, "^", %1, %1, %1, false },
    
    .{ .ASSIGNMENT, "+=", %1, %1, %1, false },
    .{ .ASSIGNMENT, "-=", %1, %1, %1, false },
    .{ .ASSIGNMENT, "|=", %1, %1, %1, false },
    .{ .ASSIGNMENT, "&=", %1, %1, %1, false },
    .{ .ASSIGNMENT, "^=", %1, %1, %1, false },
    
    .{ type = .PREFIX, identifier = "~", left = %1,   ret = %1   },
    JAI;
    
    for (string.["u8", "s8", "u16", "s16", "u32", "s32", "u64", "s64"]) {
        print(*builder, integer_template, it);
    }
    
    
    signed_integer_template :: #string JAI
    .{ type = .PREFIX, identifier = "-", left = %1,   ret = %1   },
    JAI;
    
    for (string.["s8", "s16", "s32", "s64"]) {
        print(*builder, signed_integer_template, it);
    }
    
    append(*builder, #string JAI
        .{ .BINARY, "+", float, float, float, false },
        .{ .BINARY, "-", float, float, float, false },
        .{ .BINARY, "*", float, float, float, false },
        .{ .BINARY, "/", float, float, float, false },
        
        .{ .BINARY, ">",  float, float, bool, false },
        .{ .BINARY, "<",  float, float, bool, false },
        .{ .BINARY, "==", float, float, bool, false },
        .{ .BINARY, ">=", float, float, bool, false },
        .{ .BINARY, "<=", float, float, bool, false },
        
        .{ .BINARY, "&&", bool, bool, bool, false },
        .{ .BINARY, "||", bool, bool, bool, false },
        
        .{ .BINARY, "+", Vector2, Vector2, Vector2, false },
        .{ .BINARY, "-", Vector2, Vector2, Vector2, false },
        .{ .BINARY, "*", Vector2, Vector2, Vector2, false },
        .{ .BINARY, "/", Vector2, Vector2, Vector2, false },
        .{ .BINARY, "*", Vector2, float,   Vector2, true  },
        .{ .BINARY, "/", Vector2, float,   Vector2, false },
        
        .{ .BINARY, "==", Vector2, Vector2, bool, false },
        
        
        // PREFIX    (right type will be ignored)
        .{ type = .PREFIX, identifier = "-", left = float,   ret = float },
        .{ type = .PREFIX, identifier = "-", left = Vector2, ret = Vector2 },
        
        
        // ASSIGNMENT
        // TODO: should we just assert that left and ret are the same type?
        
        .{ .ASSIGNMENT, "+=", float, float, float, false },
        .{ .ASSIGNMENT, "-=", float, float, float, false },
        
        .{ .ASSIGNMENT, "+=", Vector2, Vector2, Vector2, false },
        .{ .ASSIGNMENT, "-=", Vector2, Vector2, Vector2, false },
        .{ .ASSIGNMENT, "*=", Vector2, Vector2, Vector2, false },
        .{ .ASSIGNMENT, "/=", Vector2, Vector2, Vector2, false },
        .{ .ASSIGNMENT, "*=", Vector2, float,   Vector2, false },
        .{ .ASSIGNMENT, "/=", Vector2, float,   Vector2, false },
    ];
    JAI);
    
    return builder_to_string(*builder);
}

// TODO: we should probably adapt some of the same ideas from the improved procedure overload resolution and adapt that to binary operators
//       for example, if we know the type of one side of the operation, but not the other, then perhaps we can use the side we do know to intelligently hint a type for the other side?
resolve_builtin_operation :: (
    identifier:     string, 
    type:           Operator.Kind, 
    left_type:      *Type_Info, 
    right_type:     *Type_Info
) -> (
    index:          int, 
    type:           Operator.Kind, 
    swap_args:      bool
) {
    for BUILTIN_OPERATIONS {
        if it.identifier != identifier || it.type != type  continue;
        
        if it.type == {
          case .BINARY;
            if it.left .(*Type_Info) == left_type 
            && it.right.(*Type_Info) == right_type {
                return it_index, .BINARY, false;
            }
        
            if it.is_symmetric 
            && it.left .(*Type_Info) == right_type 
            && it.right.(*Type_Info) == left_type {
                return it_index, .BINARY, true;
            }
          
          case .PREFIX;
            if it.left.(*Type_Info) == left_type  return it_index, .PREFIX, false;
            
          case .ASSIGNMENT;
            if it.left .(*Type_Info) == left_type 
            && it.right.(*Type_Info) == right_type  return it_index, .ASSIGNMENT, false;
        }
    }
    return -1, .BINARY, false;
}

execute_builtin_operation :: (oper_idx: int, left: *void, right: *void, ret: *void) -> bool {
    // generate switch case for all builtin operations
    #insert -> string {
        builder: String_Builder;
        
        append(*builder, "if oper_idx == {\n");
        for BUILTIN_OPERATIONS {
            print_to_builder(*builder, "  case %;\n", it_index);
            if it.type == {
              case .PREFIX;
                print_to_builder(*builder, "    ret.(*%1).* = %2left.(*%1).*;\n", it.left, it.identifier);
              case .BINARY;
                print_to_builder(*builder, "    ret.(*%).* = left.(*%).* % right.(*%).*;\n", it.ret, it.left, it.identifier, it.right);
              case .ASSIGNMENT;
                print_to_builder(*builder, "    left.(*%1).* %2 right.(*%3).*;\n", it.left, it.identifier, it.right);
            }
            append(*builder, "    return true;\n");
        }
        append(*builder, "}\n");
        
        return builder_to_string(*builder);
    };
    
    assert(false, "Called execute_builtin_operation with an invalid operation index.");
    return false;
}

// this version may not be worth it
// because we will still probably need the other version for eval procs
// and this one creates more code bloat
// but, it does bake the stack pops so that we don't have to lookup the operator types 
// this is not a big deal for the exec procs, but for the bytecode, it could be a considerable performance loss 
// in any case, I will probably leave code to use the other version commented out in execute_node so I at least know the we have the option to get rid of this at any time
execute_builtin_operation :: (script: *Script, oper_idx: int) {
    
    dprint("stack_ptr: %\n", script.stack_ptr);
    
    // generate switch case for all builtin operations
    #insert -> string {
        builder: String_Builder;
        
        append(*builder, "if oper_idx == {\n");
        for BUILTIN_OPERATIONS {
            print_to_builder(*builder, "case %;\n", it_index);
            
            if it.type == {
              case .PREFIX;
                print_to_builder(*builder, "  left := stack_pop (script, %).(*%);\n", is_aggr(it.left),  it.left);
                print_to_builder(*builder, "  ret  := stack_peek(script, %).(*%);\n", is_aggr(it.ret),   it.ret);
                print_to_builder(*builder, "  ret.(*%1).* = %2left.(*%1).*;\n", it.left, it.identifier);
                
              case .BINARY;
                print_to_builder(*builder, "  right := stack_pop (script, %).(*%);\n", is_aggr(it.right), it.right);
                print_to_builder(*builder, "  left  := stack_pop (script, %).(*%);\n", is_aggr(it.left),  it.left);
                print_to_builder(*builder, "  ret   := stack_peek(script, %).(*%);\n", is_aggr(it.ret),   it.ret);
                
                print_to_builder(*builder, "  ret.* = left.* % right.*;\n", it.identifier);
                
              case .ASSIGNMENT;
                print_to_builder(*builder, "  right := stack_pop (script, %).(*%);\n", is_aggr(it.right), it.right);
                print_to_builder(*builder, "  left  := stack_pop (script, %).(*%);\n", true, it.left);
                print_to_builder(*builder, "  left.(*%).* % right.(*%).*;\n", it.left, it.identifier, it.right);
            }
            
            append(*builder, "  return;\n");
        }
        append(*builder, "}\n");
        
        return builder_to_string(*builder);
    }
    
    assert(false, "Called execute_builtin_operation with an invalid operation index.");
    // return false;
}

