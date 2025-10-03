
/*
    Operators
    
    I am currently in the process of cleaning up and refactoring how operators are handled.
    The goal is that we can reorganize sufficiently to implement 'comile-time' operators that behave in a similar way to directives.
    
    In order to do this, I think I will need to first cleanly separate the idea of operators as a lexical construct from operators as a particular action performed on some operand.
    I think for now I will just differntiate with the term 'operator' vs 'operation' where the operator is the lexical construct and the operation is the overloadable, typed construct.
    
    
    TODO: 
        allow user to extend the operator table
            init scripts with default operator table values
            user can register new operators in similar way to procedures or directives
        
        update lexer so that operators are parsed based on the script's operator table
        
        maybe even make a generalized "OPERATOR" token type since how would we identify such tokens otherwise?
            if doing this, then token may as well store the operator id (we will have it from doing the operator table lookup in the lexer)
            alternatively, we still use unique tokens for each operator type, but then we must go about implementing the user token thing
                otherwise user is still restricted to those tokens which already exist, obviously
            
            and even if the user can add new tokens in some generalized way, we have the problem that the lexer still needs to be able to handle those new tokens
                so then we get roped into adding either lexer callbacks or making lex_next_token totally overridable (NOTE: this is already the case, actually)
                but still, we don't really want to force the user to override lex_next_token just to add a new operator
                overriding the lexer proc or adding new tokens entirely is a very heavy-weight thing and probably also means the user now has added all new parsing and typechecking logic too
                I want the user to only have to go as deep as they really need to in order to add new functionality, yknow?
            
        
        update operation node to support replacement node in a similar way to directives
        
        refactor operand swapping so that it only occurs during execution / evaluation, not on AST
        
*/

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

// TODO: remove flags and instead use proc pointer for on_parse and on_typecheck
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

execute_builtin_operation :: (id: Builtin_Operation_ID, left: *void, right: *void, ret: *void) -> bool {
    // generate switch case for all builtin operations
    #insert -> string {
        builder: String_Builder;
        
        append(*builder, "if id == {\n");
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


// TODO: consider whether to remove this!
// this version may not be worth it
// because we will still probably need the other version for eval procs
// and this one creates more code bloat
// but, it does bake the stack pops so that we don't have to lookup the operator types 
// this is not a big deal for the exec procs, but for the bytecode, it could be a considerable performance loss 
// in any case, I will probably leave code to use the other version commented out in execute_node so I at least know the we have the option to get rid of this at any time
execute_builtin_operation :: (script: *Script, id: Builtin_Operation_ID) {
    
    dprint("stack_ptr: %\n", script.stack_ptr);
    
    // generate switch case for all builtin operations
    #insert -> string {
        builder: String_Builder;
        
        append(*builder, "if id == {\n");
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

