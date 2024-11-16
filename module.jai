
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

Script_Variable :: struct {
    name    : string;
    binding : Any;
}

Script :: struct {
    ast         : AST;
    flags       : Flags;
    
    // TODO: we may actually not need to store these on script as dynamic arrays after all
    //       these can go on script constructor, then we actually should copy only what we need onto node
    //       we will still want to have slice for variables, but we can duplicate the Any's for variables actually used into script's pool
    variables   : [..] Script_Variable;
    procedures  : [..] Script_Procedure;
    
    pool       : Flat_Pool;
    allocator  : Allocator;
    
    stack_base : *void;
    stack_size : int;
    
    stack_ptr  : *void; // state by execution procs
    
    dyncall_vm : *DCCallVM;
    
    Flags :: enum_flags {
        AST_CONSTRUCTED;
        AST_TYPECHECKED;
    }
}

STACK_SENTINEL_VALUE: u64 : 0x0123_4567_89AB_CDEF;

free_script :: (using script: *Script) {
    array_free(variables);
    array_free(procedures);
    free_ast(*ast);
    fini(*pool);
    if script.dyncall_vm  dcFree(script.dyncall_vm);
    
    script.* = .{};
}

alloc_node :: (script: *Script) -> *AST.Node {
    return New(AST.Node,, script.allocator);
}

// the by_ptr arguments are primarily to ease code gen for the builtin binary ops
// but it also allows us to more breifly capture the pattern of checking if the value is a struct 
// by_ptr should always be true for aggregate (struct/array) types, or really any type larger than a register
// these have to be macros so that we can modify the stack pointer and get the address of the value pointer itself

PTR_SIZE :: size_of(*void);

// TODO: if we passed *Script instead of just stack pointer, then we could do runtime checks to make sure we don't exhaust stack space allocated
stack_push :: (stack_ptr: **void, value_pointer: **void, $$by_ptr := false) -> *void {
    ret := value_pointer;
    if by_ptr {
        memcpy(stack_ptr.*, value_pointer, PTR_SIZE);
    } else {
        if value_pointer.* != null {
            ret = value_pointer.*;
            memcpy(stack_ptr.*, value_pointer.*, PTR_SIZE);
        } else {
            dbg_print("WARNING: value_pointer.* was null!\n");
        }
    }
    
    stack_ptr.* += PTR_SIZE;
    return ret;
}

stack_pop :: (stack_ptr: **void, $$by_ptr := false) -> *void {
    stack_ptr.* -= PTR_SIZE;
    if by_ptr {
        return (cast(**void)(stack_ptr.*)).*;
    } else {
        return stack_ptr.*;
    }
}

stack_get_top :: (stack_ptr: *void, $$by_ptr := false) -> *void {
    stack_ptr -= PTR_SIZE;
    if by_ptr {
        return (cast(**void)(stack_ptr)).*;
    } else {
        return stack_ptr;
    }
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
    lexer: Lexer;

    // May do something like this in order to extend parser / lexer for in other modules
    // resolve_identifier: struct { 
    //     proc: (string, *void) -> Any;
    //     data: *void;
    // }
}

construct_script_from_source_string :: (script: *Script, source: string) -> bool {
    success: bool;
    defer if !success  free_script(script);
    
    // setup allocator. maybe move this to init proc later?
    script.allocator.proc = flat_pool_allocator_proc;
    script.allocator.data = *script.pool;
    
    constructor: Script_Constructor;
    constructor.script = script;
    constructor.lexer.file = source;
    init_lexer(*constructor.lexer);
    
    while true {
        if !parse_statement(*constructor)  return false;
        if expect_token_type(*constructor.lexer, .EOF)  break;
    }
    
    script.flags |= .AST_CONSTRUCTED;
    
    dbg_print("\nTYPECHECKING SCRIPT\n");
    if !typecheck_script(*constructor) {
        print("failed to typecheck script!\n");
        return false;
    }
    
    // script.stack_size = stack_required;
    script.stack_size = 1024;
    script.stack_base = get(*script.pool, script.stack_size + size_of(type_of(STACK_SENTINEL_VALUE)));
    
    dbg_print("stack size: %\n", script.stack_size);
    dbg_print("stack base: %\n", script.stack_base);
    
    sentinel := cast(*type_of(STACK_SENTINEL_VALUE)) (script.stack_base + script.stack_size);
    sentinel.* = STACK_SENTINEL_VALUE;
    
    // allocate dyncall vm that will be reused
    script.dyncall_vm = dcNewCallVM(2048);
    dcMode(script.dyncall_vm, DC_CALL_C_DEFAULT);
    dcReset(script.dyncall_vm);
    
    success = true;
    return true;
}

resolve_variable :: (using ctxt: *Script_Constructor, ident: string) -> int {
    for *variables {
        if it.name == ident {
            return it_index;
        }
    }
    return -1;
}

parse_identifier :: (using ctxt: *Script_Constructor, ident: string) -> *AST.Node {
    dbg_print("parse_identifier(ident = '%')\n", ident);
    
    if expect_token_type(*lexer, .OPEN_PAREN) {
        first_arg_node : *AST.Node;
        prev_arg_node  : *AST.Node;
        arg_count      :=  0;
        if peek_token(*lexer).type != .CLOSE_PAREN {
            while true {
                node := parse_expression(ctxt, 0);
                if node == null  return null;
                
                if first_arg_node == null  first_arg_node = node;
                if prev_arg_node  != null  prev_arg_node.next = node;
                prev_arg_node = node;
                arg_count += 1;
                
                if !expect_token_type(*lexer, .COMMA)  break;
            }
        }
        
        if !expect_token_type(*lexer, .CLOSE_PAREN) {
            dbg_print("Error, expected a close paren at the end of parameter list!\n");
            return null;
        }
        
        node := alloc_node(ctxt);
        node.type = .PROCEDURE_CALL;
        node.src_text = ident;
        node.procedure_call.arg_nodes = first_arg_node;
        node.procedure_call.arg_count = arg_count;
        return node;
    }
    
    // else identifier must refer to a variable
    node := alloc_node(ctxt);
    node.type = .VARIABLE;
    node.src_text = ident;
    return node;
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
    token := get_token_or_return(*lexer, null);
    
    // TODO: refactor unary minus to use an actual unary operation
    do_unary_minus := token.type == .SUB;
    if do_unary_minus {
        token = get_token_or_return(*lexer, null);
    }
    
    node: *AST.Node;
    if token.type == {
      case .OPEN_PAREN;
        // dbg_print("encountered open paren...\n");
        node = parse_expression(ctxt, 0);
        if node == null  return null;
        
        token = get_token_or_return(*lexer, null);
        
        if token.type != .CLOSE_PAREN {
            dbg_print("Error, expected a closing paren!\n");
            return null;
        }
        // dbg_print("consumed corresponding close paren\n");
        
      case .NUMBER;
        node = alloc_node(ctxt);
        node.type   = .NUMBER;
        node.number = token.number;
        
      case .IDENTIFIER;
        // instead of resolving identifiers completely at AST contruction time, 
        // we only need to check if the identifier was a variable or a procedure
        node = parse_identifier(ctxt, token.text);
        if node == null  return null;
            
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


/*
    Notes on parse_expression and parse_increasing_precedence
    
    TODO: include link to stream where Jon talks about this
    
    these two procedures trade off back and forth in a somewhat unintuitive manner in order to construct an AST where operator precedence is respected
    this works based on an observation that when we construct the proper tree for either increasing or decreasing operator precedence 
    depending on wehter we are appending nodes linearly or recursively
    
    when we navigate a tree and execute it, we do it in (I think) postorder
    we evaluate left, then right, then self, then return up tree
    so if a node is left of self, it necessarily has higher precedence, or is in parens (bc it is executed first)
    
    parse_increasing_precedence is only relevant when parsing binary operators...?
        if so, maybe we can factor things a bit more intuitively
        
    
        
*/

parse_expression :: (using ctxt: *Script_Constructor, min_prec: int) -> *AST.Node {
    dbg_print("parse_expression(min_prec = %)\n", min_prec); 
    
    // left is also sort of the root node of expression, when re 
    // the reason we call it left is because if it is part of a binary operation, it will be the left side
    
    left := parse_leaf(ctxt);
    if left == null  return null;
    
    dbg_print("parsed a leaf: %\n", sprint_ast_node(ctxt, left));
    
    // going iteratively, precedence can only increase
    while true {
        node := parse_increasing_precedence(ctxt, left, min_prec);
        if node == null  return null;
        
        // op prec was below min_prec, or we could not parse next token.
        // not having being able to parse is fine though, 
        //     as it's probably just the end of the expression.
        // in any case, the caller should know what to expect.
        //     for example, note how parse_expression is called 
        //     after hitting open paren in parse_leaf.
        if node == left  break;
        
        // 'node' has replaced 'left' as the root node
        // original 'left' is now a child of 'node'
        left = node;
    }
    
    // left is returned as root node of expression
    // could be just a leaf, could be binary op, could be indexer
    return left;
}

// If we hit a binary operator but its precedence is too low, we return left back to caller.
parse_increasing_precedence :: (using ctxt: *Script_Constructor, left: *AST.Node, min_prec: int) -> *AST.Node {
    dbg_print("parse_increasing_precedence(left_i = %, min_prec = %)\n", left, min_prec); 
    
    next_token := peek_token(*lexer);
    
    // should this go here or up in main loop of parse_expression?
    // will require some refactoring to hoist up there
    if next_token.type == {
      case .DOT;
        get_token(*lexer);
        
        identifier := get_token(*lexer);
        if identifier.type != .IDENTIFIER {
            dbg_print("ERROR: expected identifier, got %\n", identifier);
            return null;
        }
        
        node := alloc_node(ctxt);
        node.type = .MEMBER;
        node.member = .{
            base       = left,
            identifier = identifier.text,
        };
        return node;
        
      case .OPEN_BRACKET;
        get_token(*lexer);
        
        indexing_expr := parse_expression(ctxt, 0);
        if indexing_expr == null  return null;
        
        if !expect_token_type(*lexer, .CLOSE_BRACKET)  return null;
        
        node := alloc_node(ctxt);
        node.type = .SUBSCRIPT;
        node.subscript = .{
            base          = left,
            indexing_expr = indexing_expr
        };
        return node;
    }
    
    if token_is_binary_operator(next_token) {
        next_token_prec := get_operator_precedence(next_token.text);
        if next_token_prec <= min_prec {
            dbg_print("peeked token %, but it was of lower precedence % than min_prec %. Returning node % to caller.\n",
                sprint_token(next_token), 
                next_token_prec,
                min_prec,
                sprint_ast_node(ctxt, left), 
            );
            return left;
        }
        
        get_token(*lexer);
        
        right := parse_expression(ctxt, next_token_prec);
        if right == null  return null;
        
        return create_binary_operation(ctxt, next_token.text, left, right);
    }
    
    dbg_print("sadfasdfasdf\n");
    return left;    
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

parse_statement :: (using ctxt: *Script_Constructor) -> bool {
    statement: AST.Statement;
    statement.type == .ASSIGNMENT;
    
    dbg_print("parsing expression for left side of assignment statement\n");
    statement.assignment.left = parse_expression(ctxt, 0);
    if statement.assignment.left == null  return false;
    
    if !expect_token_type(*lexer, .EQUALS)  return false;
    
    dbg_print("parsing expression for right side of assignment statement\n");
    statement.assignment.right = parse_expression(ctxt, 0);
    if statement.assignment.right == null  return false;
    
    if !expect_token_type(*lexer, .SEMICOLON)  return false;
    
    array_add(*script.ast.statements, statement);
    
    return true;
}


// Local_Variable :: struct {
//     local_type: enum { CONSTANT; SUBEXPR_VAL; INTERMEDIATE; EXT_VAR; };
//     value: Any;
// }



#scope_module

memzero :: (t: *$T) { memset(t, 0, size_of(T)); };

is_numeric_type :: (t: Type) -> bool {
    ti := cast(*Type_Info)t;
    return ti.type == .INTEGER || ti.type == .FLOAT;
}

is_numeric_type :: (ti: *Type_Info) -> bool {
    return ti.type == .INTEGER || ti.type == .FLOAT;
}


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


as_type :: inline (ti: *Type_Info) -> Type {
    return (cast(*Type)*ti).*;
}




DEBUG :: true;

#if DEBUG {#add_context dbg_print_indent: int;}

dbg_print :: (format_string: string, args: .. Any, to_standard_error := false) -> bytes_printed: s64 {
    #if DEBUG {
        for 0..context.dbg_print_indent  print("  ");
        return print(format_string, ..args, to_standard_error);
    }
}

