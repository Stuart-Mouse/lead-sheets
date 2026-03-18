/*
    Operators
    
    I am currently in the process of cleaning up and refactoring how operators are handled.
    The goal is that we can reorganize sufficiently to implement 'comile-time' operators that behave in a similar way to directives.
    
    In order to do this, I think I will need to first cleanly separate the idea of operators as a lexical construct from operators as a particular action performed on some operand.
    I think for now I will just differntiate with the term 'operator' vs 'operation' where the operator is the lexical construct and the operation is the overloadable, typed construct.
    
    Stages of Operator Resolution
    
    lexing operators
        concerns only a sequence of characters, no disambiguation over operator's lexical type (prefix, postfix, binary)
    
    parsing operators
        now we care about the lexical type of the operator, as well as disambiguating between standard binary operators and assignment operators
        at this stage, we go from a generalized OPERATOR-type token to an index into the operator table
        our only new error case is in checking whether the operator is being used correctly lexically
    
    typechecking operators
        operator needs to resolve to some concrete operations between two typed operands
        We now move form using an operator_index to using an index to a builtin operation or to some overload procedure
    
    TODO: figure out if we want "=" to remain a special case or not. perhaps we should just let people override it in the same way as other operators
    TODO: now that I've done all the work to split unary and binary operators, go back and think about whether that was really a good idea or not
          because it seems we really don't actually want to make dot and subscript be binary operators, and I'm not sure what the change gains us
*/

// check if a token is an operator, and optionally check if it is a specific operator
is_operator :: inline (token: Token, text := "") -> bool {
    return token.type == .OPERATOR && (!text || token.text == text);
}

get_operator_info :: inline (script: *Script, operator_id: Operator_ID) -> *Operator_Info { 
    return *script.operator_table[operator_id]; 
}

get_prefix_operator :: (script: *Script, token: Token) -> bool, int {
    for script.operator_table {
        if it.lexical_type != .PREFIX  continue;
        if it.token == token.text {
            return true, it_index;
        }
    }
    return false, 0;
}

get_postfix_operator :: (script: *Script, token: Token) -> bool, int {
    for script.operator_table {
        if it.lexical_type != .POSTFIX  continue;
        if it.token == token.text {
            return true, it_index;
        }
    }
    return false, 0;
}

get_binary_operator :: (script: *Script, token: Token) -> bool, int {
    ok, index := get_binary_operator(script, token.text);
    return ok, index;
}

get_binary_operator :: (script: *Script, token: string) -> bool, int {
    for script.operator_table {
        if is_unary(it) continue;
        if it.token == token {
            return true, it_index;
        }
    }
    return false, -1;
}

default_operator_table :: Operator_Info.[
    .{ .ASSIGNMENT, "=",  1, "" },
    .{ .ASSIGNMENT, "+=", 1, "" },
    .{ .ASSIGNMENT, "-=", 1, "" },
    .{ .ASSIGNMENT, "|=", 1, "" },
    .{ .ASSIGNMENT, "&=", 1, "" },
    .{ .ASSIGNMENT, "^=", 1, "" },
    
    .{ .BINARY, "&&", 2, "" },
    .{ .BINARY, "||", 2, "" },
    
    .{ .BINARY, "==", 3, "" },
    .{ .BINARY, "!=", 3, "" },
    .{ .BINARY, ">=", 3, "" },
    .{ .BINARY, "<=", 3, "" },
    .{ .BINARY, ">",  3, "" },
    .{ .BINARY, "<",  3, "" },
    
    .{ .BINARY, "|",  4, "" },
    .{ .BINARY, "&",  4, "" },
    .{ .BINARY, "~",  4, "" },
    .{ .BINARY, "^",  4, "" },
    
    .{ .BINARY, "+",  5, "" },
    .{ .BINARY, "-",  5, "" },
    .{ .BINARY, "*",  6, "" },
    .{ .BINARY, "/",  6, "" },
    
    .{ .PREFIX, "-",  7, "" },
    .{ .PREFIX, "~",  7, "" },
    .{ .PREFIX, "!",  7, "" },
];

Operator_ID :: int;

Operator_Info :: struct {
    lexical_type:       Lexical_Type;
    token:              string;
    precedence:         int;
    directive_name:     string;
    
    Lexical_Type :: enum { UNINITIALIZED; BINARY; PREFIX; POSTFIX; ASSIGNMENT; };
}

is_unary :: inline (lexical_type: Operator_Info.Lexical_Type) -> bool { return lexical_type == .PREFIX || lexical_type == .POSTFIX; }
is_unary :: inline (op: Operator_Info) -> bool { return is_unary(op.lexical_type); }

is_binary :: inline (lexical_type: Operator_Info.Lexical_Type) -> bool { return lexical_type == .BINARY || lexical_type == .ASSIGNMENT; }
is_binary :: inline (op: Operator_Info) -> bool { return is_binary(op.lexical_type); }

register_operator :: (script: *Script, lexical_type: Operator_Info.Lexical_Type, token: string, precedence: int, directive := "") -> bool {
    for script.operator_table {
        if it.lexical_type == lexical_type && it.token = token {
            log("Error: Cannot register % operator '%'. This operator already exists in the script's operator table.", lexical_type, token);
            return false;
        }
    }
    array_add(*script.operator_table, .{ lexical_type, token, precedence, directive });
    return true;
}









/*
    Builtin Operations
    
    In order to make a lot of basic operations a little bit less slow, we have a big switch/case to handle so-called 'builtin' operations.
    These builtin operations are basically just your essential numeric and binary operations for int, float, bool, and the like.
    Builtin operators are referred to by an id, which is simply an index to a massive switch/case.
    
    Right now, we have this big dumb #insert that builds the table of builtin operators (since this table needs to be constant).
    I would like to simplify it in the future, but it is what it is for right now.
    
    Whenever I next work on builtin operators I will likely improve this table and also make it so that the user can somehow add their own builtin operations.
    (Probably, I will just use the sign bit of the index to signify that we should consult the user's provided table and execution procedure.)
    
    
    TODO: consider whether we should have some 'invalid' zero value for Builtin_Operation_ID
*/

Builtin_Operation_ID :: int;

// this is dumb, but it must be done (for now)
#insert -> string {
    builder: String_Builder;

    append(*builder, #string JAI
    BUILTIN_OPERATIONS :: (
        struct { type: Operator_Info.Lexical_Type; identifier: string; left: Type; right: Type; result: Type; is_symmetric: bool; }
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
    .{ .BINARY, "!=", %1, %1, bool, false },
    .{ .BINARY, ">=", %1, %1, bool, false },
    .{ .BINARY, "<=", %1, %1, bool, false },
    
    .{ .BINARY, "|", %1, %1, %1, false },
    .{ .BINARY, "&", %1, %1, %1, false },
    .{ .BINARY, "^", %1, %1, %1, false },
    
    .{ .ASSIGNMENT, "+=", %1, %1, void, false },
    .{ .ASSIGNMENT, "-=", %1, %1, void, false },
    .{ .ASSIGNMENT, "|=", %1, %1, void, false },
    .{ .ASSIGNMENT, "&=", %1, %1, void, false },
    .{ .ASSIGNMENT, "^=", %1, %1, void, false },
    
    .{ type = .PREFIX, identifier = "~", left = %1,   result = %1   },
    .{ type = .PREFIX, identifier = "!", left = %1,   result = bool },
    JAI;
    
    for (string.["u8", "s8", "u16", "s16", "u32", "s32", "u64", "s64"]) {
        print(*builder, integer_template, it);
    }
    
    
    signed_integer_template :: #string JAI
    .{ type = .PREFIX, identifier = "-", left = %1,   result = %1   },
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
        .{ .BINARY, "!=", float, float, bool, false },
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
        .{ .BINARY, "!=", Vector2, Vector2, bool, false },
        
        
        // PREFIX    (right type will be ignored)
        .{ type = .PREFIX, identifier = "-", left = float,   result = float },
        .{ type = .PREFIX, identifier = "!", left = float,   result = bool  },
        .{ type = .PREFIX, identifier = "!", left = bool,    result = bool  },
        
        .{ type = .PREFIX, identifier = "-", left = Vector2, result = Vector2 },
        
        
        // ASSIGNMENT
        // TODO: should we just assert that left and result are the same type?
        
        .{ .ASSIGNMENT, "+=", float, float, void, false },
        .{ .ASSIGNMENT, "-=", float, float, void, false },
        
        .{ .ASSIGNMENT, "+=", Vector2, Vector2, void, false },
        .{ .ASSIGNMENT, "-=", Vector2, Vector2, void, false },
        .{ .ASSIGNMENT, "*=", Vector2, Vector2, void, false },
        .{ .ASSIGNMENT, "/=", Vector2, Vector2, void, false },
        .{ .ASSIGNMENT, "*=", Vector2, float,   void, false },
        .{ .ASSIGNMENT, "/=", Vector2, float,   void, false },
    ];
    JAI);
    
    return builder_to_string(*builder);
}

// TODO: we should probably adapt some of the same ideas from the improved procedure overload resolution and adapt that to binary operators
//       for example, if we know the type of one side of the operation, but not the other, then perhaps we can use the side we do know to intelligently hint a type for the other side?
resolve_builtin_operation :: (
    identifier:     string, 
    type:           Operator_Info.Lexical_Type, 
    left_type:      *Type_Info, 
    right_type:     *Type_Info
) -> (
    id:             Builtin_Operation_ID, 
    swap_args:      bool
) {
    for BUILTIN_OPERATIONS {
        if it.identifier != identifier || it.type != type  continue;
        
        if it.type == {
          case .BINARY;
            if it.left .(*Type_Info) == left_type 
            && it.right.(*Type_Info) == right_type {
                return it_index, false;
            }
        
            if it.is_symmetric 
            && it.left .(*Type_Info) == right_type 
            && it.right.(*Type_Info) == left_type {
                return it_index, true;
            }
          
          case .PREFIX;
            if it.left.(*Type_Info) == left_type  return it_index, false;
            
          case .ASSIGNMENT;
            if it.left .(*Type_Info) == left_type 
            && it.right.(*Type_Info) == right_type  return it_index, false;
        }
    }
    return -1, false;
}

execute_builtin_operation :: (id: Builtin_Operation_ID, left: *void, right: *void, result: *void) -> bool {
    // generate switch case for all builtin operations
    #insert -> string {
        builder: String_Builder;
        
        append(*builder, "if id == {\n");
        for BUILTIN_OPERATIONS {
            print_to_builder(*builder, "  case %;\n", it_index);
            if it.type == {
              case .PREFIX;
                print_to_builder(*builder, "    result.(*%3).* = %2left.(*%1).*;\n", it.left, it.identifier, it.result);
              case .BINARY;
                print_to_builder(*builder, "    result.(*%).* = left.(*%).* % right.(*%).*;\n", it.result, it.left, it.identifier, it.right);
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
