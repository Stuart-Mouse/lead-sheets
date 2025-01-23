
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

get_operator_precedence :: (op: string) -> int {
    for operator_precedence_table  {
        if it.op == op {
            return it.prec;
        }
    }
    return -1;
}

// TODO: probably remove this, I don't think there's any good reason to do the whole bit set thing here just to check if a node is n operator
// token_is_binary_operator :: (token: Token) -> bool {
//     binary_operators :: #run make_bit_set(Token_Type.ADD | .SUB | .MUL | .DIV | .LOGICAL_AND | .LOGICAL_OR | .LESS_THAN | .GREATER_THAN | .LESS_THAN_OR_EQUAL_TO | .GREATER_THAN_OR_EQUAL_TO | .COMPARE_EQUAL);
//     return check_bit(binary_operators, token.type);
// }

// TODO: could instead just search the operator precedence table? Then that table should use a token_type to identify the operator instead of a string...

token_is_binary_operator :: (token: Token) -> bool {
    return token.type == .ADD
        || token.type == .SUB
        || token.type == .MUL
        || token.type == .DIV
        || token.type == .LOGICAL_AND
        || token.type == .LOGICAL_OR
        || token.type == .LESS_THAN
        || token.type == .GREATER_THAN
        || token.type == .LESS_THAN_OR_EQUAL_TO
        || token.type == .GREATER_THAN_OR_EQUAL_TO
        || token.type == .COMPARE_EQUAL;
}

// TODO: may need to differentiate prefix and postfix later on...
//       this will mean adding another flag to the below operator table
token_is_unary_operator :: (token: Token) -> bool {
    return token.type == .SUB;
}

operator_precedence_table :: struct { op: string; unary := false; prec: int; } .[
    .{ op = "&&", prec = 1 },
    .{ op = "||", prec = 1 },
    
    .{ op = "==", prec = 2 },
    .{ op = ">=", prec = 2 },
    .{ op = "<=", prec = 2 },
    .{ op = ">",  prec = 2 },
    .{ op = "<",  prec = 2 },
    
    .{ op = "+",  prec = 5 },
    .{ op = "-",  prec = 5 },
    .{ op = "*",  prec = 6 },
    .{ op = "/",  prec = 6 },
    
    .{ op = "-",  prec = 7, unary = true },
];


Operator_Type :: enum { BINARY; UNARY; ASSIGNMENT; };


BUILTIN_OPERATIONS :: (
    struct { operator_ident: string; left: Type; right: Type; ret: Type; is_symmetric: bool; }
).[
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
    
    
    // UNARY    (right type will be ignored)
    .{ operator_ident = "-", left = int,   ret = int   },
    .{ operator_ident = "-", left = float, ret = float },
    
    
    // ASSIGNMENT
    
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

execute_builtin_operation :: (oper_idx: int, left: *void, right: *void, ret: *void) -> bool {
    // generate switch case for all builtin operations
    #insert -> string {
        builder: String_Builder;
        
        append(*builder, "if oper_idx == {\n");
        for BUILTIN_OPERATIONS {
            print_to_builder(*builder, "  case %;\n", it_index);
            if it.right.(*void) == null {
                print_to_builder(*builder, "    ret.(*%).* = %left.(*%).*;\n", it.ret, it.operator_ident, it.left);
            } else {
                print_to_builder(*builder, "    ret.(*%).* = left.(*%).* % right.(*%).*;\n", it.ret, it.left, it.operator_ident, it.right);
            }
            append(*builder, "    return true;\n");
        }
        append(*builder, "}\n");
        
        return builder_to_string(*builder);
    }
    
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
                print_to_builder(*builder, "  left  := stack_pop (script, %).(*%);\n", is_aggr(it.left),  it.left);
                print_to_builder(*builder, "  ret   := stack_peek(script, %).(*%);\n", is_aggr(it.ret),   it.ret);
                print_to_builder(*builder, "  ret.* = %left.*;\n", it.operator_ident);
            }
            // binary operations
            else {
                print_to_builder(*builder, "  right := stack_pop (script, %).(*%);\n", is_aggr(it.right), it.right);
                print_to_builder(*builder, "  left  := stack_pop (script, %).(*%);\n", is_aggr(it.left),  it.left);
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

