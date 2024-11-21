
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



Script_Variable :: struct {
    name    : string;
    binding : Any;
}

Script :: struct {
    ast_root    : *Node; // root node should always be a .BLOCK
    flags       : Flags;
    
    // TODO: we may actually not need to store these on script as dynamic arrays after all
    //       these can go on script constructor, then we actually should copy only what we need onto node
    //       we will still want to have slice for variables, but we can duplicate the Any's for variables actually used into script's pool
    variables   : [..] Script_Variable;
    procedures  : [..] Script_Procedure;
    
    pool        : Flat_Pool;
    allocator   : Allocator;
    
    stack_base  : *void;
    stack_size  : int;
    stack_ptr   : *void; // state used by execution procs
    
    dyncall_vm  : *DCCallVM;
    
    Flags :: enum_flags {
        PARSED;
        TYPECHECKED;
    }
}

STACK_SENTINEL_VALUE: u64 : 0x0123_4567_89AB_CDEF;

free_script :: (using script: *Script) {
    array_free(variables);
    array_free(procedures);
    fini(*pool);
    if script.dyncall_vm  dcFree(script.dyncall_vm);
    
    script.* = .{};
}

alloc_node :: (using ctxt: *Script_Constructor, loc := #caller_location) -> *Node {
    node := New(Node,, allocator);
    node.enclosing_scope = current_enclosing_scope; // not sure if this is the place for this, but its the only sensible thing to do at this very moment
    dbg_print("allocated node % at %\n", node, loc);
    return node;
}

// the by_ptr arguments are primarily to ease code gen for the builtin binary ops
// but it also allows us to more breifly capture the pattern of checking if the value is a struct 
// by_ptr should always be true for aggregate (struct/array) types, or really any type larger than a register
// these have to be macros so that we can modify the stack pointer and get the address of the value pointer itself

PTR_SIZE :: size_of(*void);

// TODO: if we passed *Script instead of just stack pointer, then we could do runtime checks to make sure we don't exhaust stack space allocated
// TODO: this proceudre also should probably just assert false when value_pointer.* is null
// stack_push :: (stack_ptr: **void, value_pointer: **void, $$by_ptr := false) -> *void {
//     ret := value_pointer;
//     if by_ptr {
//         memcpy(stack_ptr.*, value_pointer, PTR_SIZE);
//     } else {
//         if value_pointer.* != null {
//             ret = value_pointer.*;
//             memcpy(stack_ptr.*, value_pointer.*, PTR_SIZE);
//         } else {
//             dbg_print("WARNING: value_pointer.* was null!\n");
//         }
//     }
    
//     stack_ptr.* += PTR_SIZE;
//     return ret;
// }


// TODO: we should also have some kind of assert that stack pointer is not less that stack base
stack_pop :: inline (stack_ptr: **void, $$by_ptr := false) -> *void {
    stack_ptr.* -= PTR_SIZE;
    if by_ptr {
        return (cast(**void)(stack_ptr.*)).*;
    } else {
        return stack_ptr.*;
    }
}

stack_get_top :: inline (stack_ptr: *void, $$by_ptr := false) -> *void {
    stack_ptr -= PTR_SIZE;
    if by_ptr {
        return (cast(**void)(stack_ptr)).*;
    } else {
        return stack_ptr;
    }
}

stack_push_any :: (script: *Script, any: Any, is_lvalue := false) -> bool {
    if script.stack_ptr >= script.stack_base + script.stack_size {
        dbg_print("Runtime Error: ran out of space on stack!\n");
        return false;
    }
    defer script.stack_ptr += PTR_SIZE;
    
    if is_aggr(any.type) || is_lvalue {
        (cast(**void)script.stack_ptr).* = any.value_pointer; // store the pointer itself instead of the value
    } else {
        assert(any.type.runtime_size <= PTR_SIZE);
        memcpy(script.stack_ptr, any.value_pointer, PTR_SIZE); // copy the value being pointed at for register-sized values (INT, FLOAT, ENUM)
    }
    return true;
}

stack_pop_any :: inline (script: *Script, ti: *Type_Info, is_lvalue := false) -> Any {
    assert(script.stack_ptr >= script.stack_base);

    script.stack_ptr -= PTR_SIZE;
    if is_aggr(ti) || is_lvalue {
        return Any.{ ti, (cast(**void)script.stack_ptr).* }; // additional dereference to get pointer to value
    } else {
        assert(ti.runtime_size <= PTR_SIZE);
        return Any.{ ti, script.stack_ptr };
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
    
    current_enclosing_scope: *Node;
    
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
    
    script.ast_root = alloc_node(*constructor);
    script.ast_root.type = .BLOCK;
    
    constructor.current_enclosing_scope = script.ast_root;
    
    script.ast_root.block.statements = parse_statement(*constructor);
    if !script.ast_root.block.statements  return false;
    
    previous := script.ast_root.block.statements;
    while !expect_token_type(*constructor.lexer, .EOF) {
        node := parse_statement(*constructor);
        if !node  return false;
        
        previous.next = node;
        previous      = node;
    }
    
    assert(constructor.current_enclosing_scope == script.ast_root);
    
    script.flags |= .PARSED;
    
    dbg_print("\nTYPECHECKING SCRIPT\n");
    if !typecheck_script(*constructor) {
        print("failed to typecheck script!\n");
        return false;
    }
    
    // TODO: we should have some way to configure the stack size that gets allocated. perhaps should be passed by user as a parameter
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

parse_statement :: (using ctxt: *Script_Constructor) -> *Node {
    next_token := peek_token(*lexer);
    if next_token.type == .IDENTIFIER {
        if next_token.text == {
          case "if";
            get_token(*lexer);
            
            node := alloc_node(ctxt);
            node.type = .IF_STATEMENT;
            node.enclosing_scope = current_enclosing_scope; // store prev scope
            
            node.if_statement.condition = parse_expression(ctxt, 0);
            if node.if_statement.condition == null  return null;
            
            current_enclosing_scope = node; // set new scope
            
            node.if_statement.statement = parse_statement(ctxt);
            if node.if_statement.statement == null  return null;
            
            current_enclosing_scope = node.enclosing_scope; // restore prev scope
            
            return node;
            
          case "while";
            get_token(*lexer);
            
            node := alloc_node(ctxt);
            node.type = .WHILE_LOOP;
            node.enclosing_scope = current_enclosing_scope; // store prev scope
            
            node.while_loop.condition = parse_expression(ctxt, 0);
            if node.while_loop.condition == null  return null;
            
            current_enclosing_scope = node; // set new scope
            
            node.while_loop.statement = parse_statement(ctxt);
            if node.while_loop.statement == null  return null;
            
            current_enclosing_scope = node.enclosing_scope; // restore prev scope
            
            return node;
            
          case "for";
            get_token(*lexer);
            
            node := alloc_node(ctxt);
            node.type = .FOR_LOOP;
            node.enclosing_scope = current_enclosing_scope; // store prev scope
            
            node.for_loop.control = parse_expression(ctxt, 0);
            if node.for_loop.control == null  return null;
            
            current_enclosing_scope = node; // set new scope
            
            node.for_loop.statement = parse_statement(ctxt);
            if node.for_loop.statement == null  return null;
            
            current_enclosing_scope = node.enclosing_scope; // restore prev scope
            
            return node;
        }
    }
    
    if expect_token_type(*lexer, .OPEN_BRACE) {
        node := alloc_node(ctxt);
        node.type = .BLOCK;
        node.enclosing_scope = current_enclosing_scope; // store prev scope
        current_enclosing_scope = node; // set new scope
        
        statement := parse_statement(ctxt);
        if !statement  return null;
        
        node.block.statements = statement;
        
        previous := statement;
        while !expect_token_type(*lexer, .CLOSE_BRACE) {
            statement = parse_statement(ctxt);
            if !statement  return null;
            
            previous.next = statement;
            previous      = statement;
        }
        
        current_enclosing_scope = node.enclosing_scope; // restore prev scope
        
        return node;
    }
    
    left := parse_expression(ctxt, 0);
    if left == null  return null;
    
    if expect_token_type(*lexer, .ASSIGN_EQUAL) {
        right := parse_expression(ctxt, 0);
        if right == null  return null;
        
        if !expect_token_type(*lexer, .SEMICOLON)  return null;

        node := alloc_node(ctxt);
        node.type = .ASSIGNMENT;
        node.assignment.left  = left;
        node.assignment.right = right;
        return node;
    }
    
    // check if expression can be used as a statement
    if expect_token_type(*lexer, .SEMICOLON) {
        if left.type == .PROCEDURE_CALL {
            left.flags |= .DISCARD_VALUE; // do not allocate space for result if its an aggregate type and do not push anythign to stack when executing
            return left;
        }
        
        return null;
    }
    
    return null;
}

resolve_variable :: (using ctxt: *Script_Constructor, ident: string) -> int {
    for variables {
        if it.name == ident {
            return it_index;
        }
    }
    return -1;
}

is_reserved_word :: (str: string) -> bool {
    if str == {
        case "if";        return true;
        case "for";       return true;
        case "while";     return true;
        case "it";        return true;
        case "it_index";  return true;
    }
    return false;
}

// searches from current_enclosing_scope
get_enclosing_for_loop :: (enclosing_scope: *Node) -> *Node {
    while enclosing_scope {
        if enclosing_scope.type == .FOR_LOOP  return enclosing_scope;
        enclosing_scope = enclosing_scope.enclosing_scope;
    }
    return null;
}


parse_identifier :: (using ctxt: *Script_Constructor, ident: string) -> *Node {
    dbg_print("parse_identifier(ident = '%')\n", ident);
    
    if ident == "it" {
        node := alloc_node(ctxt);
        node.type = .ITERATOR;
        node.src_text = ident;
        return node;
    }
    if ident == "it_index" {
        node := alloc_node(ctxt);
        node.type = .ITERATOR_INDEX;
        node.src_text = ident;
        return node;
    }
    
    // check first for reserved words
    if is_reserved_word(ident)  return null;
    
    
    variable_index := resolve_variable(ctxt, ident);
    if variable_index != -1 {
        node := alloc_node(ctxt);
        node.type = .VARIABLE;
        node.src_text = ident;
        return node;
    }
    
    // must be a procedure i guess?
    // TODO: we should probably at least search to see if there are any procedures with the given identifier
    
    node := alloc_node(ctxt);
    node.type = .PROCEDURE;
    node.src_text = ident;
    return node;
}


get_operator_precedence :: (op: string) -> int {
    for operator_precedence_table  {
        if it.op == op {
            return it.prec;
        }
    }
    return -1;
}


Bit_Field :: struct(MIN: int, MAX: int) {
    MASK_SIZE :: ((MAX - MIN + 1) / 8) + cast(int) ((MAX - MIN + 1) % 8 != 0);
    mask: [MASK_SIZE] u8;
}

check_bitfield :: (using bit_field: Bit_Field, check_bits: ..bit_field.T) -> bool {
    for check_bits {
        byte_index := (it - MIN) / 8;
        bit_index  := cast(u8) 1 << ((it - MIN) % 8);
        if it < MIN || it > MAX || bit_index & mask[byte_index] == 0 {
            // log("Bit % was not set", it);
            return false;
        }
    }
    return true;
}

set_bitfield :: (using bit_field: *Bit_Field, set_bits: ..bit_field.T) -> bool {
    for set_bits {
        if it < MIN || it > MAX { 
            // log("Failed to set bit %", it);
            return false;
        }
        byte_index := (it - MIN) / 8;
        bit_index  := cast(u8) 1 << ((it - MIN) % 8);
        mask[byte_index] |= bit_index;
    }
    return true;
}

// token_is_binary_operator :: (token: Script_Token) -> bool {
//     binary_operators :: #run make_bit_set(Script_Token_Type.ADD | .SUB | .MUL | .DIV | .LOGICAL_AND | .LOGICAL_OR | .LESS_THAN | .GREATER_THAN | .LESS_THAN_OR_EQUAL_TO | .GREATER_THAN_OR_EQUAL_TO | .COMPARE_EQUAL);
//     return check_bit(binary_operators, token.type);
// }

token_is_binary_operator :: (token: Script_Token) -> bool {
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

// need to remember how to not have to specify the count explicitly here
operator_precedence_table :: struct { op: string; prec: int; } .[
    .{ "&&", 1 },
    .{ "||", 1 },
    .{ "==", 2 },
    .{ ">=", 2 },
    .{ "<=", 2 },
    .{ ">",  2 },
    .{ "<",  2 },
    
    .{ "+", 5 },
    .{ "-", 5 },
    .{ "*", 6 },
    .{ "/", 6 },
];

parse_leaf :: (using ctxt: *Script_Constructor) -> *Node {
    dbg_print("parse_leaf()\n");
    dbg_print_push_indent();
    
    token := get_token_or_return(*lexer, null);
    
    node: *Node;
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
        
        
      case .NUMBER;
        node = alloc_node(ctxt);
        node.type   = .NUMBER;
        node.number = token.number;
        
        
      case .IDENTIFIER;
        // instead of resolving identifiers completely at this time, 
        // we only need to check if the identifier was a variable or a procedure
        node = parse_identifier(ctxt, token.text);
        if node == null  return null;
        
        
      case;
        dbg_print("Unexpected token '%'. Expected a number or identifier.\n", sprint_token(token));
        assert(false);
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

parse_expression :: (using ctxt: *Script_Constructor, min_prec: int) -> *Node {
    dbg_print("parse_expression(min_prec = %)\n", min_prec); 
    dbg_print_push_indent();
    
    // left is also sort of the root node of expression, when re 
    // the reason we call it left is because if it is part of a binary operation, it will be the left side
    
    
    // TODO: refactor unary minus to use an actual unary operation
    //       this is a really bad way to handle unary minus... 
    do_unary_minus := expect_token_type(*lexer, .SUB);
    
    left := parse_leaf(ctxt);
    if left == null  return null;
    
    // makes a binary op of -1 * node
    if do_unary_minus {
        neg := alloc_node(ctxt);
        neg.type   = .NUMBER;
        neg.number = -1;
        left = create_binary_operation(ctxt, "*", neg, left);
    }
    
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
        
        if node == left { 
            dbg_print("node == left,  break\n");
            break;
        }
        
        // 'node' has replaced 'left' as the root node
        // original 'left' is now a child of 'node'
        dbg_print("left = node\n");
        left = node;
    }
    
    // left is returned as root node of expression
    // could be just a leaf, could be binary op, could be indexer
    dbg_print("return left\n");
    return left;
}

// If we hit a binary operator but its precedence is too low, we return left back to caller.
parse_increasing_precedence :: (using ctxt: *Script_Constructor, left: *Node, min_prec: int) -> *Node {
    dbg_print("parse_increasing_precedence(left_i = %, min_prec = %)\n", left, min_prec); 
    dbg_print_push_indent();
    
    next_token := peek_token(*lexer);
    dbg_print("next_token: %\n", sprint_token(next_token));
    
    // should this go here or up in main loop of parse_expression?
    // will require some refactoring to hoist up there
    if next_token.type == {
      case .OPEN_PAREN;
        get_token(*lexer);
        dbg_print("parsing argument list\n");
        dbg_print_push_indent();
        
        first_arg_node : *Node;
        arg_count      :=  0;
        
        if peek_token(*lexer).type != .CLOSE_PAREN {
            first_arg_node = parse_expression(ctxt, 0);
            if first_arg_node == null  return null;
            arg_count = 1;
            
            dbg_print("parsed first arg\n");
            
            prev_arg_node := first_arg_node;
            while expect_token_type(*lexer, .COMMA) {
                dbg_print("parsing another arg\n");
                node := parse_expression(ctxt, 0);
                if node == null  return null;
                
                prev_arg_node.next = node;
                prev_arg_node = node;
                arg_count += 1;
            }
        }
        dbg_print("done parsing arg list, expecting close paren.\n");
        
        if !expect_token_type(*lexer, .CLOSE_PAREN) {
            dbg_print("Error, expected a close paren at the end of parameter list!\n");
            return null;
        }
        
        node := alloc_node(ctxt);
        node.type = .PROCEDURE_CALL;
        node.procedure_call = .{
            procedure_node = left,
            arg_nodes = first_arg_node,
            arg_count = arg_count
        };
        return node;
        
        
      case .DOT;
        get_token(*lexer);
        dbg_print("parsing struct member access\n");
        
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
        dbg_print("parsing array subscript\n");
        
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
        
        
      case .SPREAD;
        get_token(*lexer);
        dbg_print("parsing range\n");
        
        lower := left;
        upper := parse_expression(ctxt, 0);
        if upper == null  return null;
        
        node := alloc_node(ctxt);
        node.type = .RANGE;
        node.range = .{
            lower = lower,
            upper = upper
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
        
        dbg_print("return create_binary_operation\n");
        return create_binary_operation(ctxt, next_token.text, left, right);
    }
    
    dbg_print("return left\n");
    return left;    
}


create_binary_operation :: (using ctxt: *Script_Constructor, operator_ident: string, left: *Node, right: *Node) -> *Node {
    dbg_print(
        "creating binary operation % % %\n", 
        sprint_ast_node(ctxt, left), 
        operator_ident, 
        sprint_ast_node(ctxt, right)
    );
    node := alloc_node(ctxt);
    node.type = .OPERATION;
    node.src_text = operator_ident;
    node.operation = .{
        left  = left,
        right = right,
    };
    return node;
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
dbg_print_push_indent :: () #expand {
    #if DEBUG {
        context.dbg_print_indent += 1;
        `defer context.dbg_print_indent -= 1;
    }
}
