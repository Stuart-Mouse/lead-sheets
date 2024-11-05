
#import "Basic";
#import "Math";
#import "String";
#import "Hash_Table";

#import "Data_Packer";
#import "dyncall";

#load "token.jai";
#load "ast.jai";
#load "procedure.jai";
#load "binary_op.jai";

/*
    Right now, it looks like we could probably just do typechecking as we construct the script. 
    However, I'm not sure if that's the best long-term idea.
    But, in any case it should ucrrently be so simple to do so, that there would be very little trouble to factor it out later.
    So for now I will do it that way, even though that may be tentative.
    Doing it this way for now also makes it easier to have a 'compile an run' sort of option that does it all in one call
    
    It may be better to just store indexes to variables, procedures, and operators on each node instead of duplicating the data on the node or using a pointer
    main reason being for variables, so that we can sub out the variables' values before running the script, since presumably we may want to run a script multiple times and our memory could move around
    
    
    in the process of changin stuff to use indices...
    problem iwht resolve proc, since we also need to get back flags (whether to swap args) to store on node
    no biggie, but worth noting ig
    
    
    
    we currently handle procedure overloading because of operators, but I may actaully remove proc overloading 
        on the one hand, could be useful for matrix operations
        on the other hand, could complicate pattern matching on the ast for expression analysis
    
    also, we need to add additional flag for operator now in order to support builtin operators
    since we are doing the type checking while creating ast, we need to encapsulate the typechecking an execution for this nicely
*/


// need to remember how to not have to specify the count explicitly here
operator_precedence_table : [4] struct { op: u8; prec: int; } : .[
    .{ #char "+", 1 },
    .{ #char "-", 1 },
    .{ #char "*", 2 },
    .{ #char "/", 2 },
];

Source_Location :: struct {
    line, char: int;
}

External_Variable :: struct {
    name    : string;
    binding : Any;
}

Script :: struct {
    ast         : AST;
    variables   : [..] External_Variable;
    procedures  : [..] Script_Procedure;
    operators   : [..] Script_Procedure; // TODO: enumerate what operators can actually be defined, prevent user registering disallowed operators
    flags       : Flags;
    
    // TOOD: replace resizable arrays with views, use resizable in script constructor instead?
    // these really will not need to or even be able to change after ast has been created
    
    Flags :: enum_flags {
        AST_CONSTRUCTED;
    }
}

free_script :: (using script: *Script) {
    array_free(variables);
    memset(*variables, 0, size_of(Resizable_Array));
    array_free(procedures);
    memset(*procedures, 0, size_of(Resizable_Array));
    array_free(operators);
    memset(*operators, 0, size_of(Resizable_Array));
    free_ast(*ast);
}

// ensures no name conflicts
set_variable :: (script: *Script, name: string, any: Any) {
    for *script.variables {
        if it.name == name {
            it.binding = any;
            return;
        }
    }
    
    array_add(*script.variables, .{ name, any });
}


Script_Constructor :: struct {
    using #as script : *Script;
    tokenizer : Tokenizer;
}

construct_script_from_source_string :: (script: *Script, source: string) -> bool {
    success: bool;
    defer if !success  free_script(script);
        
    constructor: Script_Constructor;
    constructor.script = script;
    constructor.tokenizer.file = source;
    if !consume_token(*constructor.tokenizer) {
        return false;
    }
    
    parse_statement :: (using ctxt: *Script_Constructor) -> bool {
        // for now, we only have assignement statements, so just do that
        // this is all very much hardcoded for that one case
        
        stmt: AST.Statement;
        
        stmt.type == .ASSIGNMENT;
        
        ident_token, ok := get_token(*tokenizer);
        if !ok || ident_token.type != .IDENTIFIER {
            return false;
        }
        
        equals_token:, ok = get_token(*tokenizer);
        if !ok || equals_token.type != .EQUALS 
            return false;
        
        print("parsed identifier '%' for left side of assignment statement\n", ident_token.text);
        stmt.assignment.left = ident_token.text;
        
        print("parsing expression for right side of assignment statement\n");
        stmt.assignment.right.root = parse_expression(ctxt, 0);
        if stmt.assignment.right.root == -1  return false;
        
        semicolon_token:, ok = get_token(*tokenizer);
        if !ok || semicolon_token.type != .SEMICOLON 
            return false;
        
        array_add(*script.ast.statements, stmt);
                
        return true;
    }
    
    while true {
        if !parse_statement(*constructor) {
            return false;
        } 
        if peek_token(*constructor.tokenizer).type == .EOF {
            break;
        }
    }
    
    script.flags |= .AST_CONSTRUCTED;
    
    success = true;
    return true;
}

resolve_variable :: (using ctxt: *Script_Constructor, ident: string) -> int {
    for * script.variables {
        if it.name == ident {
            return it_index;
        }
    }
    return -1;
}

resolve_identifier :: (using ctxt: *Script_Constructor, ident: string) -> int {
    print("resolve_identifier(ident = '%')\n", ident);
    
    token := peek_token(*tokenizer);
    // don't need to check if next token was invalid rn, only need to know if it was or wasn't an open paren
    
    if token.type != .OPEN_PAREN {
        var_idx := resolve_variable(ctxt, ident);
        if var_idx == -1 {
            dbg_print("Unable to resolve variable '%'!\n", ident);
            return -1;
        }
        
        node := array_add(*ast.nodes);
        node.* = .{
            type = .VARIABLE,
            variable_index = var_idx,
        };
        return ast.nodes.count - 1;
    } 
    else {
        if !consume_token(*tokenizer)  return -1; // consume OPEN_PAREN just peeked
        
        param_nodes: [..] int; // TODO: later we will allocate this array somewhere contiguous with other script constants
        while true {
            node_i := parse_expression(ctxt, 0);
            if node_i == -1  return -1;
            
            print("added node % to param list\n", sprint_ast_node(ctxt, ast.nodes[node_i]));
            array_add(*param_nodes, node_i);
            
            token = peek_token(*tokenizer);
            print("next token after expression in param list: %\n", sprint_token(token));
            if token.type != .COMMA  break;
            if !consume_token(*tokenizer)  return -1; // consume COMMA just peeked
        }
        
        token, ok := get_token(*tokenizer);
        if !ok  return -1;
        if token.type != .CLOSE_PAREN {
            print("Error, expected a close paren at the end of parameter list!\n");
            return -1;
        }
        
        node := array_add(*ast.nodes);
        node.type = .PROCEDURE_CALL;
        node.procedure_call.params = param_nodes;
        
        argument_types := NewArray(param_nodes.count, *Type_Info,, temp);
        for param_nodes {
            value_type := node_get_value_type(ctxt, ast.nodes[it]);
            assert(value_type != null);
            argument_types[it_index] = value_type;
        }
        
        node.procedure_call.index = resolve_procedure_index(ctxt, ident, argument_types);
        if node.procedure_call.index == -1  return -1;
        
        return ast.nodes.count - 1;
    }
    
    print("Unreachable");
    return -1;
}

// TODO: we really need to have a way to properly peek a token

get_operator_precedence :: (op: u8) -> int {
    for operator_precedence_table 
        if it.op == op
            return it.prec;
            
    return -1;
}

token_is_binary_operator :: (token: Script_Token) -> bool {
    return token.type == .ADD ||
           token.type == .SUB ||
           token.type == .MUL ||
           token.type == .DIV;
}

parse_leaf :: (using ctxt: *Script_Constructor) -> int {
    token, ok := get_token(*tokenizer);
    if !ok  return -1;
    
    do_unary_minus := token.type == .SUB;
    if do_unary_minus {
        token, ok = get_token(*tokenizer);
        if !ok  return -1;
    }
    
    node_i := -1;
    if token.type == {
        case .OPEN_PAREN;
            print("encountered open paren...\n");
            node_i = parse_expression(ctxt, 0);
            if node_i == -1  return -1;
            
            token, ok = get_token(*tokenizer);
            if !ok  return -1;
            
            if token.type != .CLOSE_PAREN {
                print("Error, expected a closing paren!\n");
                return -1;
            }
            print("consumed corresponding close paren\n");
    
        case .NUMBER;
            node := array_add(*ast.nodes);
            node.* = .{
                type   = .NUMBER,
                number = token.number,
            };
            node_i = ast.nodes.count - 1;
            
        case .IDENTIFIER;
            // instead of resolving identifiers completely at AST contruction time, 
            // we only need to check if the identifier was a variable or a procedure
            node_i = resolve_identifier(ctxt, token.text);
            
        case;
            print("Unexpected token '%'. Expected a number or identifier.\n", sprint_token(token));
            assert(false);
    }
    
    if do_unary_minus {
        // makes a binary op of -1 * node
        neg := array_add(*ast.nodes);
        neg.* = .{
            type   = .NUMBER,
            number = -1
        };
        neg_i := ast.nodes.count - 1;
        
        node_i = create_binary_operation(ctxt, "*", neg_i, node_i);
    }
    
    return node_i;
}

parse_expression :: (using ctxt: *Script_Constructor, min_prec: int) -> int {
    print("parse_expression(min_prec = %)\n", min_prec); 

    left_i := parse_leaf(ctxt);
    if left_i == -1  return -1;
    
    print("parsed a leaf: %\n", sprint_ast_node(ctxt, ast.nodes[left_i]));
    
    while true {
        node_i := parse_increasing_precedence(ctxt, left_i, min_prec);
        if node_i == -1      return -1;
        if node_i == left_i  break;
        
        left_i = node_i;
    }
    
    return left_i;
}

parse_increasing_precedence :: (using ctxt: *Script_Constructor, left_i: int, min_prec: int) -> int {
    print("parse_increasing_precedence(left_i = %, min_prec = %)\n", left_i, min_prec); 

    next_token := peek_token(*tokenizer);
    
    if !token_is_binary_operator(next_token) {
        print("peeked token %, but it was not an operator. Returning node % to caller.\n",
            sprint_token(next_token), 
            sprint_ast_node(ctxt, ast.nodes[left_i]), 
        );
        return left_i;
    }
    
    next_token_prec := get_operator_precedence(next_token.text[0]);
    if next_token_prec < min_prec {
        print("peeked token %, but it was of lower precedence % than min_prec %. Returning node % to caller.\n",
            sprint_token(next_token), 
            next_token_prec,
            min_prec,
            sprint_ast_node(ctxt, ast.nodes[left_i]), 
        );
        return left_i;
    }
    
    if !consume_token(*tokenizer)  return -1;
    
    right_i := parse_expression(ctxt, next_token_prec);
    if right_i == -1  return -1;
    
    return create_binary_operation(ctxt, next_token.text, left_i, right_i);
}

// TODO: we will need to add logic here for builtin operators
create_binary_operation :: (using ctxt: *Script_Constructor, operator_ident: string, left_i: int, right_i: int) -> int {
    print("creating binary operation % % %\n", sprint_ast_node(ctxt, ast.nodes[left_i]), operator_ident, sprint_ast_node(ctxt, ast.nodes[right_i]));
    
    left_ti  := node_get_value_type(ctxt, ast.nodes[left_i ]);
    right_ti := node_get_value_type(ctxt, ast.nodes[right_i]);
    
    swap_args, is_builtin := false;
    
    oper_idx:, swap_args = resolve_builtin_operation(operator_ident, left_ti, right_ti);
    if oper_idx >= 0 {
        is_builtin = true;
        print("is builtin, idx %\n", oper_idx);
    } else {
        oper_idx, swap_args = resolve_procedure_index(ctxt, operator_ident, .[ left_ti, right_ti ], true);
        print("resolved to operator index %\n", oper_idx);
        if oper_idx == -1  return -1;
    }
    
    node := array_add(*ast.nodes);
    node.type = .OPERATION;
    node.operation = .{
        left  = left_i,
        right = right_i,
        index = oper_idx,
    };
    
    if swap_args   node.operation.flags |= .SWAP_ARGS; 
    if is_builtin  node.operation.flags |= .IS_BUILTIN;  
    
    print("operation flags: %\n", node.operation.flags);
    
    return ast.nodes.count - 1;
}


// cycles between skipping whitespace and comments until next character is neither
// TODO: add *int param to increment line count 
skip_whitespace_and_comments :: (file: *string) -> bool {
    if file.count == 0 return false;
    while true {
        while is_whitespace(file.*[0]) {
            if !advance(file) return false;
        }
        if file.*[0] == #char "#" {
            while file.*[0] != #char "\n" {
                if !advance(file) return false;
            }
        }
        else return true;
    }
    return true;
}

is_whitespace :: inline (char: u8) -> bool {
  return char == #char " "
      || char == #char "\t"
      || char == #char "\r"
      || char == #char "\n";
}

get_numeric_value_as_f32 :: (any: Any) -> (float32, bool) {
    if any.type.type == {
        case .INTEGER;
            ti_int := cast(*Type_Info_Integer) any.type;
            if ti_int.signed {
                if any.type.runtime_size == {
                    case 1; return xx (cast(*s8 )any.value_pointer).*, true;
                    case 2; return xx (cast(*s16)any.value_pointer).*, true;
                    case 4; return xx (cast(*s32)any.value_pointer).*, true;
                    case 8; return xx (cast(*s64)any.value_pointer).*, true;
                    case  ; return 0, false;
                }
            } else {
                if any.type.runtime_size == {
                    case 1; return xx (cast(*u8 )any.value_pointer).*, true;
                    case 2; return xx (cast(*u16)any.value_pointer).*, true;
                    case 4; return xx (cast(*u32)any.value_pointer).*, true;
                    case 8; return xx (cast(*u64)any.value_pointer).*, true;
                    case  ; return 0, false;
                }
            }
        case .FLOAT;
            if any.type.runtime_size == {
                case 4; return xx (cast(*float32)any.value_pointer).*, true;
                case 8; return xx (cast(*float64)any.value_pointer).*, true;
                case  ; return 0, false;
            }
    }
    return 0, false;
}

set_numeric_value_from_f32 :: (any: Any, value: float32) -> bool {
    if any.type.type == {
        case .INTEGER;
            ti_int := cast(*Type_Info_Integer) any.type;
            if ti_int.signed {
                if any.type.runtime_size == {
                    case 1; (cast(*s8 )any.value_pointer).* = xx value; return true;
                    case 2; (cast(*s16)any.value_pointer).* = xx value; return true;
                    case 4; (cast(*s32)any.value_pointer).* = xx value; return true;
                    case 8; (cast(*s64)any.value_pointer).* = xx value; return true;
                    case  ; return false;
                }
            } else {
                if any.type.runtime_size == {
                    case 1; (cast(*u8 )any.value_pointer).* = xx value; return true;
                    case 2; (cast(*u16)any.value_pointer).* = xx value; return true;
                    case 4; (cast(*u32)any.value_pointer).* = xx value; return true;
                    case 8; (cast(*u64)any.value_pointer).* = xx value; return true;
                    case  ; return false;
                }
            }
        case .FLOAT;
            if any.type.runtime_size == {
                case 4; (cast(*float32)any.value_pointer).* = xx value; return true;
                case 8; (cast(*float64)any.value_pointer).* = xx value; return true;
                case  ; return false;
            }
    }
    return false;
}



#scope_module

advance :: inline (file: *string, amount := 1) -> bool {
    _amount := min(amount, file.count);
    file.data  += _amount;
    file.count -= _amount;
    return file.count > 0;
}

memzero :: (t: *$T) { memset(t, 0, size_of(T)); };


array_find_where :: (array: []$T, $$cond: (T, *void) -> bool, data: *void) -> int #expand {
    for array  if cond(it, data)  return it_index;
}

is_numeric_type :: (t: Type) -> bool {
    ti := cast(*Type_Info)t;
    return ti.type == .INTEGER || ti.type == .FLOAT;
}

is_numeric_type :: (ti: *Type_Info) -> bool {
    return ti.type == .INTEGER || ti.type == .FLOAT;
}

type_from_info :: inline (ti: *Type_Info) -> Type { return (cast(*Type)*ti).*; }


// copied from how_to/170_modify.jai
Dynamic_New :: (ti: *Type_Info, initialized := true) -> *void {
    size := ti.runtime_size;
    if size < 0 return null;   // Maybe they gave us a weird polymorphic type for some reason.

    memory := alloc(size);

    if initialized {
        if ti.type == .STRUCT {
            // Structs can have initializers. If the initializer is null, the struct is to be zeroed.
            tis := cast(*Type_Info_Struct) ti;
            if tis.initializer  tis.initializer(memory);
            else                memset(memory, 0, size);
        } else {
            // Non-structs always default to 0.
            // @Incomplete: Here we are not handling variants, or arrays of things that might have initializers.
            memset(memory, 0, size);
        }
    }

    return memory;
}

Dynamic_New_Any :: inline (ti: *Type_Info, initialized := true) -> Any {
    memory := Dynamic_New(ti, initialized);
    return ifx memory then Any.{ ti, memory } else Any.{};
}


type_info_to_type :: inline (ti: *Type_Info) -> Type {
    return (cast(*Type)*ti).*;
}