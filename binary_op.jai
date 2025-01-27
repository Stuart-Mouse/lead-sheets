
/*
    We need to specify the return type here so that we can look it up at runtime for the purpose of type-checking scripts.
    It would probably be possible to remove the need for this, but it would complicate certain sections of code.
    
    
    How to implement assignment operators
    the issue is that we really just want one generic assignment method, memcpy
    for the operators liek += and -=, resolution will have to work a bit differnetly
        we still want to be able to overload these, but then failing that, we want to search for the standard + and - operators respectively
        this also complicates execution
        like we have the operator_type to tell us 
        
    
    also, during parsing there will definitely be some new concerns
    like, what happens if someone tries to write
    `a := b = c;`
    we have to catch that the expression `b = c` is only valid as the root node of a statement
    
*/

get_operator_precedence :: (identifier: string) -> int {
    for operator_table  {
        if it.identifier == identifier {
            return it.precedence;
        }
    }
    return -1;
}

// TODO: probably remove this, I don't think there's any good reason to do the whole bit set thing here just to check if a node is n operator
// token_is_binary_operator :: (token: Token) -> bool {
//     binary_operators :: #run make_bit_set(Token_Type.ADD | .SUB | .MUL | .DIV | .LOGICAL_AND | .LOGICAL_OR | .LESS_THAN | .GREATER_THAN | .LESS_THAN_OR_EQUAL_TO | .GREATER_THAN_OR_EQUAL_TO | .COMPARE_EQUAL);
//     return check_bit(binary_operators, token.type);
// }

// TODO: expect_unary is a stupid hack
//       
token_is_operator :: (token: Token, expect_unary := false) -> bool, Operator_Type, int {
    if token.text == "="  return true, .ASSIGNMENT, 1;
    
    // TODO: we should probably still check the token type and not just the text
    for operator_table  {
        if expect_unary && it.operator_type != .UNARY  continue;
        if it.identifier == token.text {
            return true, it.operator_type, it.precedence;
        }
    }
    return false, xx 0, 0;
}

operator_table :: struct { identifier: string; operator_type: Operator_Type; precedence: int; } .[
    .{ "=",  .ASSIGNMENT, 1 },
    .{ "+=", .ASSIGNMENT, 1 },
    .{ "-=", .ASSIGNMENT, 1 },
    
    .{ "&&", .BINARY, 2 },
    .{ "||", .BINARY, 2 },
     
    .{ "==", .BINARY, 3 },
    .{ ">=", .BINARY, 3 },
    .{ "<=", .BINARY, 3 },
    .{ ">",  .BINARY, 3 },
    .{ "<",  .BINARY, 3 },
    
    .{ "+",  .BINARY, 5 },
    .{ "-",  .BINARY, 5 },
    .{ "*",  .BINARY, 6 },
    .{ "/",  .BINARY, 6 },
    
    .{ "-",  .UNARY,  7 },
];


Operator_Type :: enum { BINARY; UNARY; ASSIGNMENT; };


BUILTIN_OPERATIONS :: (
    struct { type: Operator_Type; identifier: string; left: Type; right: Type; ret: Type; is_symmetric: bool; }
).[
    // BINARY
    .{ .BINARY, "+", int, int, int, false },
    .{ .BINARY, "-", int, int, int, false },
    .{ .BINARY, "*", int, int, int, false },
    .{ .BINARY, "/", int, int, int, false },
    
    .{ .BINARY, ">",  int, int, bool, false },
    .{ .BINARY, "<",  int, int, bool, false },
    .{ .BINARY, "==", int, int, bool, false },
    .{ .BINARY, ">=", int, int, bool, false },
    .{ .BINARY, "<=", int, int, bool, false },
    
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
    
    
    // UNARY    (right type will be ignored)
    .{ type = .UNARY, identifier = "-", left = int,   ret = int   },
    .{ type = .UNARY, identifier = "-", left = float, ret = float },
    
    
    // ASSIGNMENT
    // TODO: should we just assert that left and ret are the same type?
    .{ .ASSIGNMENT, "+=", int, int, int, false },
    .{ .ASSIGNMENT, "-=", int, int, int, false },
    
    .{ .ASSIGNMENT, "+=", float, float, float, false },
    .{ .ASSIGNMENT, "-=", float, float, float, false },
    
    .{ .ASSIGNMENT, "+=", Vector2, Vector2, Vector2, false },
    .{ .ASSIGNMENT, "-=", Vector2, Vector2, Vector2, false },
    .{ .ASSIGNMENT, "*=", Vector2, Vector2, Vector2, false },
    .{ .ASSIGNMENT, "/=", Vector2, Vector2, Vector2, false },
    .{ .ASSIGNMENT, "*=", Vector2, float,   Vector2, false },
    .{ .ASSIGNMENT, "/=", Vector2, float,   Vector2, false },
];

resolve_builtin_operation :: (
    identifier:     string, 
    type:           Operator_Type, 
    left_type:      *Type_Info, 
    right_type:     *Type_Info
) -> (
    index:          int, 
    type:           Operator_Type, 
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
          
          case .UNARY;
            if it.left.(*Type_Info) == left_type  return it_index, .UNARY, false;
            
          case .ASSIGNMENT;
            if it.left .(*Type_Info) == left_type 
            && it.right.(*Type_Info) == right_type  return it_index, .ASSIGNMENT, false;
        }
    }
    return -1, .BINARY, false;
}

// execute_builtin_operation :: (oper_idx: int, left: *void, right: *void, ret: *void) -> bool {
//     // generate switch case for all builtin operations
//     #insert -> string {
//         builder: String_Builder;
        
//         append(*builder, "if oper_idx == {\n");
//         for BUILTIN_OPERATIONS {
//             print_to_builder(*builder, "  case %;\n", it_index);
//             if it.operator_type == {
//               case .UNARY;
//                 print_to_builder(*builder, "    ret.(*%1).* = %2left.(*%1).*;\n", it.left, it.operator_ident);
//               case .BINARY;
//                 print_to_builder(*builder, "    ret.(*%).* = left.(*%).* % right.(*%).*;\n", it.ret, it.left, it.operator_ident, it.right);
//               case .ASSIGNMENT;
//                 print_to_builder(*builder, "    ret.(*%1).* = left.(*%1).* %2 right.(*%3).*;\n", it.left, it.operator_ident, it.right);
//             }
//             append(*builder, "    return true;\n");
//         }
//         append(*builder, "}\n");
        
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
            
            if it.type == {
              case .UNARY;
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

