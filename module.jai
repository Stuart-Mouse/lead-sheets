
#import "Basic";
#import "Math";
#import "String";
#import "Hash_Table";
#import "Flat_Pool";
#import "Reflection";

#import "Data_Packer";
#import "dyncall";

#load "token.jai";
#load "node.jai";
#load "typecheck.jai";
#load "execute.jai";
#load "procedure.jai";
#load "binary_op.jai";



Script_Variable :: struct {
    name    : string;
    binding : Any;
}

Script :: struct {
    ast_root:    *Node_Block;
    flags:       Flags;
    
    variables:   [..] Script_Variable;
    procedures:  [..] Script_Procedure;
    
    pool:        Flat_Pool;
    allocator:   Allocator;
    
    stack_base:  *void;
    stack_size:  int;
    stack_ptr:   *void;
    
    dyncall_vm:  *DCCallVM;
    
    Flags :: enum_flags {
        INITIALIZED;
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
    dbg_print("wrote % to stack at %\n", << cast(**void)script.stack_ptr, script.stack_ptr);
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
    
    current_parent_node: *Node;
}

init_script :: (script: *Script) {
    script.allocator.proc = flat_pool_allocator_proc;
    script.allocator.data = *script.pool;
    
    script.flags |= .INITIALIZED;
}

parse_source_file :: (script: *Script, source: string) -> bool {
    if !(script.flags & .INITIALIZED)  return false;
    
    success: bool;
    defer if !success  free_script(script);
    
    constructor: Script_Constructor;
    constructor.script = script;
    constructor.lexer.file = source;
    init_lexer(*constructor.lexer);
    
    script.ast_root = alloc_node(*constructor, Node_Block);
    
    constructor.current_parent_node = script.ast_root;
    
    script.ast_root.statements = parse_statement(*constructor);
    if !script.ast_root.statements  return false;
    
    previous := script.ast_root.statements;
    while !expect_token_type(*constructor.lexer, .EOF) {
        node := parse_statement(*constructor);
        if !node  return false;
        
        previous.next = node;
        previous      = node;
    }
    
    assert(constructor.current_parent_node == script.ast_root);
    
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
    next_token := peek_token(*lexer); // TODO: probably just get_toke_or_return(), then switch on type.
    
    if next_token.type == {
      case .IDENTIFIER; 
        if is_reserved_word(next_token.text) {
            // looking for keywords here
            // no default fail case since identifier could just be part of some expression handled below
            // TODO: maybe check if token text is a reserved word before switch? Then we can error early if a reserved word is used inappropriately
            if next_token.text == {
              case "if";
                dbg_print("parsing if\n"); dbg_print_push_indent();
                get_token(*lexer);
                if_statement := alloc_node(ctxt, Node_If_Statement);
                current_parent_node = if_statement; 
                
                if_statement.condition = parse_expression(ctxt, 0);
                if if_statement.condition == null  return null;
                
                if_statement.statement = parse_statement(ctxt);
                if if_statement.statement == null  return null;
                
                current_parent_node = if_statement.parent; 
                return if_statement;
                
              case "while";
                dbg_print("parsing while\n"); dbg_print_push_indent();
                get_token(*lexer);
                while_loop := alloc_node(ctxt, Node_While_Loop);
                current_parent_node = while_loop; 
                
                while_loop.condition = parse_expression(ctxt, 0);
                if while_loop.condition == null  return null;
                
                while_loop.statement = parse_statement(ctxt);
                if while_loop.statement == null  return null;
                
                current_parent_node = while_loop.parent; 
                return while_loop;
                
              case "for";
                dbg_print("parsing for\n"); dbg_print_push_indent();
                get_token(*lexer);
                for_loop := alloc_node(ctxt, Node_For_Loop);
                current_parent_node = for_loop; 
                
                expr := parse_expression(ctxt, 0);
                if !expr  return null;
                
                if expect_token_type(*lexer, .SPREAD) {
                    for_loop.control_type = .RANGE;
                    for_loop.range.lower  = expr;
                    for_loop.range.upper  = parse_expression(ctxt, 0);
                    if !for_loop.range.upper  return null;
                } else {
                    for_loop.control_type = .ARRAY;
                    for_loop.array_expression = expr;
                    if for_loop.array_expression == null  return null;
                }
                
                for_loop.statement = parse_statement(ctxt);
                if for_loop.statement == null  return null;
                
                current_parent_node = for_loop.parent; 
                return for_loop;
                
              case;
                log("Error: unexpected token '%' at line %, char %", next_token.text, next_token.src_loc.line, next_token.src_loc.char);
            }
        }
        
        
      case .OPEN_BRACE;
        get_token(*lexer);
        block := alloc_node(ctxt, Node_Block);
        current_parent_node = block; 
        
        statement := parse_statement(ctxt);
        if !statement  return null;
        block.statements = statement;
        
        previous := statement;
        while !expect_token_type(*lexer, .CLOSE_BRACE) {
            statement = parse_statement(ctxt);
            if !statement  return null;
            
            statement.prev = previous;
            previous.next  = statement;
            previous       = statement;
        }
        
        current_parent_node = block.parent; 
        return block;
    }
    
    left := parse_expression(ctxt, 0);
    if left == null  return null;
    
    if expect_token_type(*lexer, .COLON) {
        if !expect_token_type(*lexer, .ASSIGN_EQUAL) {
            log("Error: colon must be followed by equals in declaration.");
            return null;
        }
        
        // maybe a bit of a hack, but for now I will just throw away the 
        // identifier node here and transfer the name/src_loc to a declaration node 
        if left.node_type != .IDENTIFIER {
            log("Error: left hand side of a declaration must be an identifier.");
            return null;
        }
        
        right := parse_expression(ctxt, 0);
        if right == null  return null;
        
        if !expect_token_type(*lexer, .SEMICOLON)  return null;
        
        declaration := alloc_node(ctxt, Node_Declaration);
        declaration.name            = (cast(*Node_Identifier)left).name;
        declaration.init_expression = right;
        return declaration;
    }
    
    if expect_token_type(*lexer, .ASSIGN_EQUAL) {
        right := parse_expression(ctxt, 0);
        if right == null  return null;
        
        if !expect_token_type(*lexer, .SEMICOLON)  return null;

        assignment := alloc_node(ctxt, Node_Assignment);
        assignment.left  = left;
        assignment.right = right;
        return assignment;
    }
    
    // check if expression can be used as a statement
    if expect_token_type(*lexer, .SEMICOLON) {
        if left.node_type == .PROCEDURE_CALL {
            left.flags |= .DISCARD_VALUE; // do not allocate space for result if its an aggregate type and do not push anythign to stack when executing
            return left;
        }
        
        return null;
    }
    
    return null;
}

is_reserved_word :: (str: string) -> bool {
    if str == {
        case "if";        return true;
        case "for";       return true;
        case "while";     return true;
    }
    return false;
}

// we probably don't need this here since this was just for it and it_index
// this will be generalized in resolve_identifier
get_enclosing_for_loop :: (parent: *Node) -> *Node {
    while parent {
        if parent.type == .FOR_LOOP  return parent;
        parent = parent.parent;
    }
    return null;
}

get_operator_precedence :: (op: string) -> int {
    for operator_precedence_table  {
        if it.op == op {
            return it.prec;
        }
    }
    return -1;
}

// TODO: probably remove this, I don't think there's any good reason to do the whole bit set thing here just to check if a node is n operator
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
    if token.type == {
      case .OPEN_PAREN;
        open_paren_location := token.src_loc;
        
        node := parse_expression(ctxt, 0);
        if node == null  return null;
        
        token = get_token_or_return(*lexer, null);
        if token.type != .CLOSE_PAREN {
            log("Error: expected a closing paren for open paren at (%:%)", open_paren_location.line, open_paren_location.char);
            return null;
        }
        return node;
        
      case .NUMBER;
        node := alloc_node(ctxt, Node_Number);
        node.float_value = token.number;
        return node;
        
        
      case .IDENTIFIER;
        if is_reserved_word(token.text) {
            log("Error: Invalid use of reserved word '%' at line %, char %.", token.text, token.src_loc.line, token.src_loc.char);
            return null;
        }
        node := alloc_node(ctxt, Node_Identifier);
        node.name = token.text;
        return node;
        
        
      case;
        assert(false, "Error: Unexpected token type '%' at (%:%). Expected a number or identifier.\n", token.type, token.src_loc.line, token.src_loc.char);
        return null;
    }
    
    return null;
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
    
    // TODO: refactor unary minus to use an actual unary operation
    //       this is a really bad way to handle unary minus... 
    do_unary_minus := expect_token_type(*lexer, .SUB);
    
    left := parse_leaf(ctxt);
    if left == null  return null;
    
    // makes a binary op of -1 * node
    if do_unary_minus {
        neg := alloc_node(ctxt, Node_Number);
        neg.float_value = -1;
        left = create_binary_operation(ctxt, "*", neg, left);
    }
    
    // dbg_print("parsed a leaf: %\n", sprint_ast_node(ctxt, left));
    
    // going iteratively, precedence can only decrease
    while true {
        // but going only recursively, precedence will only increase
        node := parse_increasing_precedence(ctxt, left, min_prec);
        if node == null  return null;
        if node == left  break;
        left = node;
    }
    
    return left;
}

// If we hit a binary operator but its precedence is too low, we return left back to caller.
parse_increasing_precedence :: (using ctxt: *Script_Constructor, left: *Node, min_prec: int) -> *Node {
    dbg_print("parse_increasing_precedence(left_i = %, min_prec = %)\n", left, min_prec); 
    dbg_print_push_indent();
    
    next_token := peek_token(*lexer);
    // dbg_print("next_token: %\n", sprint_token(next_token));
    
    // should this go here or up in main loop of parse_expression?
    // will require some refactoring to hoist up there
    if next_token.type == {
      case .OPEN_PAREN;
        get_token(*lexer);
        dbg_print("parsing argument list\n");
        dbg_print_push_indent();
        
        first_arg_node : *Node;
        arg_count      :=  0;
        
        node := alloc_node(ctxt, Node_Procedure_Call);
        
        if peek_token(*lexer).type != .CLOSE_PAREN {
            first_arg_node = parse_expression(ctxt, 0);
            if first_arg_node == null  return null;
            arg_count = 1;
            
            dbg_print("parsed first arg\n");
            
            prev_arg_node := first_arg_node;
            while expect_token_type(*lexer, .COMMA) {
                dbg_print("parsing another arg\n");
                arg_node := parse_expression(ctxt, 0);
                if arg_node == null  return null;
                
                arg_node.prev = prev_arg_node;
                prev_arg_node.next = arg_node;
                prev_arg_node = arg_node;
                arg_count += 1;
            }
        }
        dbg_print("done parsing arg list, expecting close paren.\n");
        
        if !expect_token_type(*lexer, .CLOSE_PAREN) {
            dbg_print("Error, expected a close paren at the end of parameter list!\n");
            return null;
        }
        
        node.procedure_expression = left;
        node.arguments            = first_arg_node;
        node.arguments_count      = arg_count;
        return node;
        
        
      case .DOT;
        get_token(*lexer);
        dbg_print("parsing struct member access\n");
        
        identifier := get_token(*lexer);
        if identifier.type != .IDENTIFIER {
            dbg_print("ERROR: expected identifier, got %\n", identifier);
            return null;
        }
        
        node := alloc_node(ctxt, Node_Member);
        node.base_expression = left;
        node.identifier      = identifier.text;
        return node;
        
        
      case .OPEN_BRACKET;
        get_token(*lexer);
        dbg_print("parsing array subscript\n");
        
        indexing_expr := parse_expression(ctxt, 0);
        if indexing_expr == null  return null;
        
        if !expect_token_type(*lexer, .CLOSE_BRACKET)  return null;
        
        node := alloc_node(ctxt, Node_Subscript);
        node.base_expression     = left;
        node.indexing_expression = indexing_expr;
        return node;
        
      // case .SPREAD;
      //   get_token(*lexer);
      //   dbg_print("parsing range\n");
        
      //   lower := left;
      //   upper := parse_expression(ctxt, 0);
      //   if upper == null  return null;
        
      //   node := alloc_node(ctxt, Node_Range);
      //   node.lower = lower;
      //   node.upper = upper;
      //   return node;
    }
    
    if token_is_binary_operator(next_token) {
        next_token_prec := get_operator_precedence(next_token.text);
        if next_token_prec <= min_prec {
            // dbg_print("peeked token %, but it was of lower precedence % than min_prec %. Returning node % to caller.\n",
            //     sprint_token(next_token), 
            //     next_token_prec,
            //     min_prec,
            //     sprint_ast_node(ctxt, left), 
            // );
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
    // dbg_print(
    //     "creating binary operation % % %\n", 
    //     sprint_ast_node(ctxt, left), 
    //     operator_ident, 
    //     sprint_ast_node(ctxt, right)
    // );
    node := alloc_node(ctxt, Node_Operation);
    node.name  = operator_ident;
    node.left  = left;
    node.right = right;
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




DEBUG :: false;

#if DEBUG {#add_context dbg_print_indent: int;}

dbg_print :: (format_string: string, args: .. Any) {
    #if DEBUG {
        for 0..context.dbg_print_indent  print("  ");
        print(format_string, ..args);
    }
}
dbg_print_push_indent :: () #expand {
    #if DEBUG {
        context.dbg_print_indent += 1;
        `defer context.dbg_print_indent -= 1;
    }
}
