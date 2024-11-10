
#import "Basic";
#import "Math";
#import "String";
#import "Hash_Table";
#import "Flat_Pool";

#import "Data_Packer";
#import "dyncall";

#load "token.jai";
#load "ast.jai";
#load "procedure.jai";
#load "binary_op.jai";
#load "typecheck.jai";



// need to remember how to not have to specify the count explicitly here
operator_precedence_table :: struct { op: string; prec: int; } .[
    .{ "+", 1 },
    .{ "-", 1 },
    .{ "*", 2 },
    .{ "/", 2 },
];

Source_Location :: struct {
    line, char: int;
}

Script_Variable :: struct {
    name    : string;
    binding : Any;
}

Script :: struct {
    ast         : AST;
    flags       : Flags;
    variables   : [..] Script_Variable;
    procedures  : [..] Script_Procedure;
    operators   : [..] Script_Procedure; // TODO: enumerate what operators can actually be defined, prevent user registering disallowed operators
    
    pool      : Flat_Pool;
    allocator : Allocator;
    // vm: *DCCallVM; // TODO: use common vm for all dyncall calls, will require changes to dyncall interface or custom code here in lead sheets
    
    Flags :: enum_flags {
        AST_CONSTRUCTED;
        AST_TYPECHECKED;
    }
}

free_script :: (using script: *Script) {
    array_free(variables);
    array_free(procedures);
    array_free(operators);
    free_ast(*ast);
    fini(*pool);
    
    script.* = .{};
}

alloc_node :: (script: *Script) -> *AST.Node {
    return New(AST.Node,, script.allocator);
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
    using #as script: *Script;
    
    tokenizer: Tokenizer;
}

construct_script_from_source_string :: (script: *Script, source: string) -> bool {
    success: bool;
    defer if !success  free_script(script);
    
    // setup allocator. maybe move this to init proc later?
    script.allocator.proc = flat_pool_allocator_proc;
    script.allocator.data = *script.pool;
    
    constructor: Script_Constructor;
    constructor.script = script;
    constructor.tokenizer.file = source;
    if !consume_token(*constructor.tokenizer) {
        return false;
    }
    
    parse_statement :: (using ctxt: *Script_Constructor) -> bool {
        statement: AST.Statement;
        statement.type == .ASSIGNMENT;
        
        dbg_print("parsing expression for left side of assignment statement\n");
        statement.assignment.left.root = parse_expression(ctxt, 0);
        if statement.assignment.left.root == null  return false;
        
        equals_token, ok := get_token(*tokenizer);
        if !ok || equals_token.type != .EQUALS {
            return false;
        }
        
        dbg_print("parsing expression for right side of assignment statement\n");
        statement.assignment.right.root = parse_expression(ctxt, 0);
        if statement.assignment.right.root == null  return false;
        
        semicolon_token:, ok = get_token(*tokenizer);
        if !ok || semicolon_token.type != .SEMICOLON {
            return false;
        }
        
        array_add(*script.ast.statements, statement);
        
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

resolve_variable :: (script: *Script, ident: string) -> int {
    for *script.variables {
        if it.name == ident {
            return it_index;
        }
    }
    return -1;
}

resolve_identifier :: (using ctxt: *Script_Constructor, ident: string) -> *AST.Node {
    dbg_print("resolve_identifier(ident = '%')\n", ident);
    
    token := peek_token(*tokenizer);
    // don't need to check if next token was invalid rn, only need to know if it was or wasn't an open paren
    
    if token.type != .OPEN_PAREN {  
        node := alloc_node(ctxt);
        node.type = .VARIABLE;
        node.src_text = ident;
        return node;
    } 
    else {
        if !consume_token(*tokenizer)  return null; // consume OPEN_PAREN just peeked
        
        first_arg_node : *AST.Node;
        prev_arg_node  : *AST.Node;
        arg_count      :=  0;
        while true {
            node := parse_expression(ctxt, 0);
            if node == null  return null;
            
            if first_arg_node == null  first_arg_node = node;
            if prev_arg_node  != null  prev_arg_node.next_arg_node = node;
            prev_arg_node = node;
            arg_count += 1;
            
            token = peek_token(*tokenizer);
            dbg_print("next token after expression in param list: %\n", sprint_token(token));
            if token.type != .COMMA  break;
            if !consume_token(*tokenizer)  return null; // consume COMMA just peeked
        }
        
        token, ok := get_token(*tokenizer);
        if !ok  return null;
        if token.type != .CLOSE_PAREN {
            dbg_print("Error, expected a close paren at the end of parameter list!\n");
            return null;
        }
        
        node := alloc_node(ctxt);
        node.type = .PROCEDURE_CALL;
        node.src_text = ident;
        node.procedure_call.first_arg_node = first_arg_node;
        node.procedure_call.arg_count = arg_count;
        return node;
    }
    
    assert(false, "Unreachable");
    return null;
}


get_operator_precedence :: (op: string) -> int {
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

parse_leaf :: (using ctxt: *Script_Constructor) -> *AST.Node {
    token, ok := get_token(*tokenizer);
    if !ok  return null;
    
    do_unary_minus := token.type == .SUB;
    if do_unary_minus {
        token, ok = get_token(*tokenizer);
        if !ok  return null;
    }
    
    dbg_print("parsing leaf ");
    if do_unary_minus  dbg_print("(with unary minus) ");
    dbg_print("token: %\n", token);
    
    dbg_print("token.text was %\n", (cast(*Array_View_64)*token.text).*);
    
    node: *AST.Node;
    if token.type == {
        case .OPEN_PAREN;
            dbg_print("encountered open paren...\n");
            node = parse_expression(ctxt, 0);
            if node == null  return null;
            
            token, ok = get_token(*tokenizer);
            if !ok  return null;
            
            if token.type != .CLOSE_PAREN {
                dbg_print("Error, expected a closing paren!\n");
                return null;
            }
            dbg_print("consumed corresponding close paren\n");
            
        case .NUMBER;
            node = alloc_node(ctxt);
            node.type   = .NUMBER;
            node.number = token.number;
            assert(node != null);
            
        case .IDENTIFIER;
            // instead of resolving identifiers completely at AST contruction time, 
            // we only need to check if the identifier was a variable or a procedure
            node = resolve_identifier(ctxt, token.text);
            assert(node != null);
            
        case;
            dbg_print("Unexpected token '%'. Expected a number or identifier.\n", sprint_token(token));
            assert(false);
    }
    
    // makes a binary op of -1 * node
    if do_unary_minus {
        neg := alloc_node(ctxt);
        neg.type   = .NUMBER;
        neg.number = -1;
        node = create_binary_operation(ctxt, "*", neg, node);
    }
    
    return node;
}

parse_expression :: (using ctxt: *Script_Constructor, min_prec: int) -> *AST.Node {
    dbg_print("parse_expression(min_prec = %)\n", min_prec); 

    left := parse_leaf(ctxt);
    if left == null  return null;
    
    dbg_print("parsed a leaf: %\n", sprint_ast_node(ctxt, left));
    
    while true {
        node := parse_increasing_precedence(ctxt, left, min_prec);
        if node == null  return null;
        if node == left  break;
        left = node;
    }
    
    return left;
}

parse_increasing_precedence :: (using ctxt: *Script_Constructor, left: *AST.Node, min_prec: int) -> *AST.Node {
    dbg_print("parse_increasing_precedence(left_i = %, min_prec = %)\n", left, min_prec); 
    
    next_token := peek_token(*tokenizer);
    
    if !token_is_binary_operator(next_token) {
        dbg_print("peeked token %, but it was not an operator. Returning node % to caller.\n",
            sprint_token(next_token), 
            sprint_ast_node(ctxt, left), 
        );
        return left;
    }
    
    next_token_prec := get_operator_precedence(next_token.text);
    if next_token_prec < min_prec {
        dbg_print("peeked token %, but it was of lower precedence % than min_prec %. Returning node % to caller.\n",
            sprint_token(next_token), 
            next_token_prec,
            min_prec,
            sprint_ast_node(ctxt, left), 
        );
        return left;
    }
    
    if !consume_token(*tokenizer)  return null;
    
    right := parse_expression(ctxt, next_token_prec);
    if right == null  return null;
    
    return create_binary_operation(ctxt, next_token.text, left, right);
}


create_binary_operation :: (using ctxt: *Script_Constructor, operator_ident: string, left: *AST.Node, right: *AST.Node) -> *AST.Node {
    dbg_print("creating binary operation % % %\n", 
    sprint_ast_node(ctxt, left), operator_ident, 
    sprint_ast_node(ctxt, right));
    node := alloc_node(ctxt);
    node.type = .OPERATION;
    node.src_text = operator_ident;
    node.operation = .{
        left  = left,
        right = right,
    };
    return node;
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