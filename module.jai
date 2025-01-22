
#import "Basic";
#import "Math";
#import "String";
#import "Hash_Table";
#import "Flat_Pool";
#import "Reflection";
#import "Compiler"; // for 'is_subclass_of'

#import "Data_Packer";
#import "dyncall";

#load "lexer.jai";
#load "node.jai";
#load "typecheck.jai";
#load "execute.jai";
#load "procedure.jai";
#load "binary_op.jai";
#load "serialize.jai";


#module_parameters (DEBUG := false);


External_Variable :: struct {
    name:       string;
    binding:    Any;
}

Script :: struct {
    ast_root:               *Node_Block;
    flags:                  Flags;
    
    variables:              [..] External_Variable;
    procedures:             [..] External_Procedure;
    directives:             [..] Directive;
    type_table:             [..] struct { name: string; type: *Type_Info; };
    
    pool:                   Flat_Pool;
    allocator:              Allocator;
    
    stack_base:             *void;
    stack_size:             int;
    stack_ptr:              *void;
    
    dyncall_vm:             *DCCallVM;
    
    // stuff I brought over from Script Constructor
    // this is parser state, basically
    lexer:                  Lexer;
    current_parent_node:    *Node;    // perhaps this should be passed on stack
    
    
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
    array_free(directives);
    array_free(type_table);
    fini(*pool);
    if script.dyncall_vm  dcFree(script.dyncall_vm);
    
    script.* = .{};
}

PTR_SIZE :: size_of(*void);

stack_pop :: inline (script: *Script, $$is_lvalue := false) -> *void {
    script.stack_ptr -= PTR_SIZE;
    assert(script.stack_ptr >= script.stack_base);
    if is_lvalue {
        return script.stack_ptr.(**void).*;
    } else {
        return script.stack_ptr;
    }
}

stack_peek :: inline (script: *Script, $$is_lvalue := false) -> *void {
    stack_ptr := script.stack_ptr - PTR_SIZE;
    assert(stack_ptr >= script.stack_base);
    if is_lvalue {
        return stack_ptr.(**void).*;
    } else {
        return stack_ptr;
    }
}

stack_push_any :: (script: *Script, any: Any, is_lvalue := false) -> bool {
    if script.stack_ptr >= script.stack_base + script.stack_size {
        log("Runtime Error: ran out of space on stack!\n");
        return false;
    }
    defer script.stack_ptr += PTR_SIZE;
    
    if is_aggr(any.type) || is_lvalue {
        script.stack_ptr.(**void).* = any.value_pointer; // store the pointer itself instead of the value
    } else {
        assert(any.type.runtime_size <= PTR_SIZE);
        memcpy(script.stack_ptr, any.value_pointer, PTR_SIZE); // copy the value being pointed at for register-sized values (INT, FLOAT, ENUM)
    }
    // dbg_print("wrote % to stack at %\n", << cast(**void)script.stack_ptr, script.stack_ptr);
    return true;
}

stack_push_zeroes :: (script: *Script, size: int) -> bool {
    if script.stack_ptr + size >= script.stack_base + script.stack_size {
        log("Runtime Error: ran out of space on stack!\n");
        return false;
    }
    
    dbg_print("stack ptr: %\n", script.stack_ptr);
    memset(script.stack_ptr, 0, size);
    script.stack_ptr += size;
    return true;
}

stack_pop_any :: inline (script: *Script, ti: *Type_Info, is_lvalue := false) -> Any {
    assert(script.stack_ptr >= script.stack_base);
    
    script.stack_ptr -= PTR_SIZE;
    if is_aggr(ti) || is_lvalue {
        return Any.{ ti, script.stack_ptr.(**void).* }; // additional dereference to get pointer to value
    } else {
        assert(ti.runtime_size <= PTR_SIZE);
        return Any.{ ti, script.stack_ptr };
    }
}

stack_peek_any :: inline (script: *Script, ti: *Type_Info, is_lvalue := false) -> Any {
    assert(script.stack_ptr >= script.stack_base);
    ptr := script.stack_ptr - PTR_SIZE;
    
    if is_aggr(ti) || is_lvalue {
        return Any.{ ti, ptr.(**void).* }; // additional dereference to get pointer to value
    } else {
        assert(ti.runtime_size <= PTR_SIZE);
        return Any.{ ti, ptr };
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

// ensures no name conflicts
register_type :: (script: *Script, name: string, type: *Type_Info) {
    for *script.type_table {
        if it.name == name {
            log("Warning: attempt register a type with same name as previously registered type.");
            return;
        }
    }
    array_add(*script.type_table, .{ name, type });
}

init_script :: (script: *Script, stack_size := 1024) {
    script.allocator.proc = flat_pool_allocator_proc;
    script.allocator.data = *script.pool;
    
    script.stack_size = stack_size;
    script.stack_base = get(*script.pool, script.stack_size + size_of(type_of(STACK_SENTINEL_VALUE)));
    script.stack_ptr  = script.stack_base;
    sentinel := (script.stack_base + script.stack_size).(*type_of(STACK_SENTINEL_VALUE));
    sentinel.* = STACK_SENTINEL_VALUE;
    
    // allocate dyncall vm that will be reused
    script.dyncall_vm = dcNewCallVM(2048);
    dcMode(script.dyncall_vm, DC_CALL_C_DEFAULT);
    dcReset(script.dyncall_vm);
    
    script.flags |= .INITIALIZED;
}

parse_source_file :: (script: *Script, source: string) -> bool {
    if !(script.flags & .INITIALIZED)  return false;
    
    success: bool;
    defer if !success  free_script(script);
    
    script.lexer.file = source;
    init_lexer(*script.lexer);
    
    script.ast_root = alloc_node(script, Node_Block);
    script.current_parent_node = script.ast_root;
    
    script.ast_root.statements = parse_statement(script);
    if !script.ast_root.statements  return false;
    
    previous := script.ast_root.statements;
    while !expect_token_type(*script.lexer, .EOF) {
        node := parse_statement(script);
        if !node  return false;
        
        previous.next = node;
        previous      = node;
    }
    
    assert(script.current_parent_node == script.ast_root);
    
    script.flags |= .PARSED;
    
    if !typecheck_script(script) {
        log("Error: failed to typecheck script.\n");
        return false;
    }
    
    success = true;
    return true;
}

parse_statement :: (using script: *Script) -> *Node {
    token := peek_token(*lexer); // TODO: probably just get_toke_or_return(), then switch on type.
    
    if token.type == {
      case .IF; 
        dbg_print("IF STATEMENT\n"); dbg_print_push_indent();
        get_token(*lexer);
        if_statement := alloc_node(script, Node_If_Statement);
        current_parent_node = if_statement; 
        
        dbg_print("CONDITION\n"); dbg_print_push_indent();
        if_statement.condition = parse_expression(script, 0);
        if if_statement.condition == null  return null;
        
        dbg_print("STATEMENT\n"); dbg_print_push_indent();
        if_statement.statement = parse_statement(script);
        if if_statement.statement == null  return null;
        
        current_parent_node = if_statement.parent; 
        return if_statement;
        
      case .WHILE;
        dbg_print("WHILE LOOP\n"); dbg_print_push_indent();
        get_token(*lexer);
        while_loop := alloc_node(script, Node_While_Loop);
        current_parent_node = while_loop; 
        
        dbg_print("CONDITION\n");
        while_loop.condition = parse_expression(script, 0);
        if while_loop.condition == null  return null;
        
        dbg_print("STATEMENT\n");
        while_loop.statement = parse_statement(script);
        if while_loop.statement == null  return null;
        
        current_parent_node = while_loop.parent; 
        return while_loop;
        
      case .FOR;
        dbg_print("FOR LOOP\n"); dbg_print_push_indent();
        get_token(*lexer);
        for_loop := alloc_node(script, Node_For_Loop);
        current_parent_node = for_loop; 
        
        dbg_print("CONTROL EXPRESSION\n");
        expr := parse_expression(script, 0);
        if !expr  return null;
        
        // TODO: maybe allow comma separated list of elements to iterate over instead of array literals? 
        //       may jive better with lack of pointers, since lvalue-ness can be implicit
        if expect_token_type(*lexer, .SPREAD) {
            dbg_print("CONTROL TYPE IS RANGE\n");
            for_loop.control_type = .RANGE;
            for_loop.range.lower  = expr;
            for_loop.range.upper  = parse_expression(script, 0);
            if !for_loop.range.upper  return null;
        } else {
            dbg_print("CONTROL TYPE IS ARRAY\n");
            for_loop.control_type = .ARRAY;
            for_loop.array_expression = expr;
            if for_loop.array_expression == null  return null;
        }
        
        dbg_print("STATEMENT\n");
        for_loop.statement = parse_statement(script);
        if for_loop.statement == null  return null;
        
        current_parent_node = for_loop.parent; 
        return for_loop;
        
      case .OPEN_BRACE;
        dbg_print("BLOCK\n"); dbg_print_push_indent();
        get_token(*lexer);
        block := alloc_node(script, Node_Block);
        current_parent_node = block; 
        
        statement := parse_statement(script);
        if !statement  return null;
        block.statements = statement;
        
        previous := statement;
        while !expect_token_type(*lexer, .CLOSE_BRACE) {
            statement = parse_statement(script);
            if !statement  return null;
            
            previous.next  = statement;
            previous       = statement;
        }
        
        current_parent_node = block.parent; 
        return block;
    }
    
    dbg_print("EXPRESSION...\n");
    
    left := parse_expression(script, 0);
    if left == null  return null;
    
    if expect_token_type(*lexer, .COLON) {
        dbg_print("DECLARATION\n"); dbg_print_push_indent();
    
        // maybe a bit of a hack, but for now I will just throw away the 
        // identifier node here and transfer the name/src_loc to a declaration node 
        if left.node_type != Node_Identifier {
            log("Error: left hand side of a declaration must be an identifier.");
            return null;
        }
        
        declaration := alloc_node(script, Node_Declaration);
        declaration.name = left.(*Node_Identifier).name;
        // declaration.src_loc = left.src_loc;
        
        next_token_type := peek_token(*lexer).type;
        if next_token_type != .ASSIGN_EQUAL && next_token_type != .COLON {
            dbg_print("WITH EXPLICIT TYPE\n");
            declaration.type_expression = xx parse_expression(script, 0);
            if !declaration.type_expression  return null;
            if declaration.type_expression.node_type != Node_Identifier {
                log("Error: type expression of a declaration must be an identifier.");
                return null;
            }
        }
        
        do_assignment := false;
        is_constant_declaration := false;
        
        if expect_token_type(*lexer, .ASSIGN_EQUAL) {
            do_assignment = true;
        }
        else if expect_token_type(*lexer, .COLON) {
            do_assignment = true;
            is_constant_declaration = true;
        }
        
        if do_assignment {
            dbg_print("WITH ASSIGNMENT\n");
            declaration.init_expression = parse_expression(script, 0);
            if declaration.init_expression == null  return null;
            if is_constant_declaration then declaration.flags |= .IS_CONSTANT;
        }
        
        if !expect_token_type(*lexer, .SEMICOLON)  return null;
        
        return declaration;
    }
    
    // TODO: probably better here to just peek the token and then switch on type? why are we using expect_token_type here anymore?
    
    if expect_token_type(*lexer, .ASSIGN_EQUAL)  {
        dbg_print("ASSIGNMENT\n"); dbg_print_push_indent();
        right := parse_expression(script, 0);
        if right == null  return null;
        
        if !expect_token_type(*lexer, .SEMICOLON)  return null;

        assignment := alloc_node(script, Node_Assignment);
        assignment.left  = left;
        assignment.right = right;
        return assignment;
    }
    
    
    // check if expression can be used as a statement
    if expect_token_type(*lexer, .SEMICOLON) {
        if left.node_type == {
          case Node_Procedure_Call;
            left.flags |= .DISCARD_VALUE; // do not allocate space for result if its an aggregate type and do not push anythign to stack when executing
            return left;
            
          case Node_Directive;
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

// This procedure is kinda incomplete and doesn;t do everything yet, but here's the plan...
// will return float64, u64, or s64 as a sort of 'preferred type' to represent the number
// we can then recast from this preferred type to 
// obviously any number with a fractional component will use float64
// any base 10 number that can fit in an s64 will use an s64
// if the number is larger than s64 max, then we'll try to use u64 instead
// any number encoded as hexadecimal, octal, binary, etc. will use u64
// and then maybe also we could support the hexadecimal float thing, but I don't really care about that unless it's trivial to do
parse_number :: (s: string) -> Small_Any, bool {
    // TODO: if number begins with 0b, 0h, 0x, etc..
    //       this will require changes in the lexer as well

    // if we've got a decimal point in there, we will prefer float64
    if contains(s, #char ".") {
        val, ok := string_to_float64(s);
        if !ok  log("Failed to parse float from '%'.", s);
        return make_small_any(val), ok;
    }
    
    val, ok := string_to_int(s, 10, s64);
    if !ok  log("Failed to parse integer from '%'.", s);
    return make_small_any(val), ok;
}

parse_leaf :: (using script: *Script) -> *Node {
    // dbg_print("parse_leaf()\n");
    // dbg_print_push_indent();
    
    token := get_token_or_return(*lexer, null);
    if token.type == {
      case .DOT;
        dbg_print("DOT (UNARY)\n");
        if expect_token_type(*lexer, .OPEN_BRACE) {
            node := alloc_node(script, Node_Literal);
            node.literal_type = .STRUCT;
            
            ok: bool;
            node.aggr.expressions, _, ok = parse_comma_separated_expressions(script, .CLOSE_BRACE);
            if !ok  return null;
            
            return node;
        }
        
        // TODO: identifer -> unary dot enum thing
        log("Error: unexpected DOT token at (%:%)", token.src_loc.line, token.src_loc.char);
        return null;
        
      case .OPEN_PAREN;
        open_paren_location := token.src_loc;
        
        node := parse_expression(script, 0);
        if node == null  return null;
        
        token = get_token_or_return(*lexer, null);
        if token.type != .CLOSE_PAREN {
            log("Error: expected a closing paren for open paren at (%:%)", open_paren_location.line, open_paren_location.char);
            return null;
        }
        return node;
        
        
      case .NUMBER;
        dbg_print("NUMBER (%)\n", token.text);
        value, ok := parse_number(token.text);
        if !ok  return null;
        
        node := alloc_node(script, Node_Literal);
        node.literal_type = .NUMBER;
        node.number = value;
        return node;
        
        
      case .STRING;
        dbg_print("STRING (\"%\")\n", token.text);
        node := alloc_node(script, Node_Literal);
        node.literal_type = .STRING;
        node.text = token.text;
        return node;
        
    
      case .DIRECTIVE;
        dbg_print("DIRECTIVE\n"); dbg_print_push_indent();
        node := alloc_node(script, Node_Directive);
        node.name = token.text;
        
        node.directive_index = -1;
        for script.directives {
            if it.name == node.name {
                node.directive_index = it_index;
            }
        }
        if node.directive_index == -1  return null;
        
        if !expect_token_type(*lexer, .OPEN_PAREN) {
            dbg_print("Error, expected an open paren after directive name!\n");
            return null;
        }
        
        ok: bool;
        node.arguments, node.arguments_count, ok = parse_comma_separated_expressions(script, .CLOSE_PAREN);
        if !ok  return null;
        
        if !execute_directive(script, node) {
            log("Error: failed while trying to execute a directive.");
            return null;
        }
        
        return node;
        
        
      case .IDENTIFIER;
        dbg_print("IDENTIFIER (%)\n", token.text);
        if is_reserved_word(token.text) {
            log("Error: Invalid use of reserved word '%' at line %, char %.", token.text, token.src_loc.line, token.src_loc.char);
            return null;
        }
        return make_identifier(script, token.text);
        
        
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

parse_expression :: (using script: *Script, min_prec: int) -> *Node {
    // dbg_print("parse_expression(min_prec = %)\n", min_prec); 
    // dbg_print_push_indent();
    
    // TODO: refactor unary minus to use an actual unary operation
    //       this is a really bad way to handle unary minus... 
    do_unary_minus := expect_token_type(*lexer, .SUB);
    
    left := parse_leaf(script);
    if left == null  return null;
    
    // makes a binary op of -1 * node
    if do_unary_minus {
        if left.node_type == Node_Literal && left.(*Node_Literal).literal_type == .NUMBER {
            negate_number(*(left.(*Node_Literal).number));
        } else {
            neg := alloc_node(script, Node_Literal);
            neg.literal_type = .NUMBER;
            neg.number = make_small_any(-1);
            left = make_binary_operation(script, "*", neg, left);
        }
    }
    
    // dbg_print("parsed a leaf: %\n", sprint_ast_node(script, left));
    
    // going iteratively, precedence can only decrease
    while true {
        // but going only recursively, precedence will only increase
        node := parse_increasing_precedence(script, left, min_prec);
        if node == null  return null;
        if node == left  break;
        left = node;
    }
    
    return left;
}

// needs to also return bool since node can be null validly if there were no arguments
parse_comma_separated_expressions :: (using script: *Script, expected_end_token: Token_Type) -> (*Node, int, bool) {
    first: *Node;
    count: int;
    
    if !expect_token_type(*lexer, expected_end_token) {
        first = parse_expression(script, 0);
        if first == null  return null, 0, false;
        count = 1;
        
        prev := first;
        while expect_token_type(*lexer, .COMMA) {
            next := parse_expression(script, 0);
            if next == null  return null, 0, false;
            
            prev.next = next;
            prev = next;
            count += 1;
        }
        
        if !expect_token_type(*lexer, expected_end_token) {
            log("Error, expected a % token at the end of comma separated expressions!\n", expected_end_token);
            return null, 0, false;
        }
    }
    
    return first, count, true;
}

// If we hit a binary operator but its precedence is too low, we return left back to caller.
parse_increasing_precedence :: (using script: *Script, left: *Node, min_prec: int) -> *Node {
    // dbg_print("parse_increasing_precedence(left_i = %, min_prec = %)\n", left, min_prec); 
    // dbg_print_push_indent();
    
    token := peek_token(*lexer);
    // dbg_print("token: %\n", sprint_token(token));
    
    if token.type == .QUESTION_MARK {
        get_token(*lexer);
        if left.node_type != Node_Literal {
            log("Error: a `?` can only be used directly after a literal.");
            return null;
        }
        left.(*Node_Literal).flags |= .IS_MALLEABLE;
    }
    
    // should this go here or up in main loop of parse_expression?
    // will require some refactoring to hoist up there
    if token.type == {
      case .AS;
        // need to create node for cast
        // just put LHS and RHS on node, do the rest in typecheck
        log("Error: not yet implemented.");
        return null;
        
        
      case .OPEN_PAREN;
        get_token(*lexer);
        
        node := alloc_node(script, Node_Procedure_Call);
        node.procedure_expression = left;
        
        ok: bool;
        node.arguments, node.arguments_count, ok = parse_comma_separated_expressions(script, .CLOSE_PAREN);
        if !ok  return null;
        
        return node;
        
        
      case .DOT;
        get_token(*lexer);
        dbg_print("DOT (BINARY)\n");
        
        // next token must either be an identifier or an open brace, no other options
        node := alloc_node(script, Node_Dot);
        
        token := get_token(*lexer);
        if token.type == {
          case .IDENTIFIER;
            node.dot_type = .STRUCT_MEMBER;
            node.left     = left;
            node.right    = make_identifier(script, token.text);
            return node;
            
          case .OPEN_BRACE;
            node.dot_type = .LITERAL;
            node.left     = left;
            
            struct_literal: *Node_Literal;
            {
                struct_literal = alloc_node(script, Node_Literal);
                struct_literal.literal_type = .STRUCT;
                ok: bool;
                struct_literal.aggr.expressions, _, ok = parse_comma_separated_expressions(script, .CLOSE_BRACE);
                if !ok  return null;
            }
            node.right = struct_literal;
            return node;
        }
        
        log("Error: unexpected token '%' after dot at (%:%)", token.type, token.src_loc.line, token.src_loc.char);
        return null;
        
      case .OPEN_BRACKET;
        get_token(*lexer);
        dbg_print("ARRAY SUBSCRIPT\n");
        
        indexing_expr := parse_expression(script, 0);
        if indexing_expr == null  return null;
        
        if !expect_token_type(*lexer, .CLOSE_BRACKET)  return null;
        
        node := alloc_node(script, Node_Subscript);
        node.base_expression     = left;
        node.indexing_expression = indexing_expr;
        return node;
    }
    
    if token_is_binary_operator(token) {
        token_prec := get_operator_precedence(token.text);
        if token_prec <= min_prec  return left;
        dbg_print("BINARY OPERATION (%)\n", token.text);
        
        get_token(*lexer);
        
        right := parse_expression(script, token_prec);
        if right == null  return null;
        
        return make_binary_operation(script, token.text, left, right);
    }
    
    return left;
}

// TODO: add source location parameter. we should actually do this for all nodes
make_identifier :: (script: *Script, name: string) -> *Node_Identifier {
    node := alloc_node(script, Node_Identifier);
    node.name = name;
    return node;
}

make_binary_operation :: (script: *Script, operator_ident: string, left: *Node, right: *Node) -> *Node {
    // dbg_print(
    //     "creating binary operation % % %\n", 
    //     sprint_ast_node(script, left), 
    //     operator_ident, 
    //     sprint_ast_node(script, right)
    // );
    node := alloc_node(script, Node_Operation);
    node.name  = operator_ident;
    node.left  = left;
    node.right = right;
    return node;
}


#scope_module

memzero :: (t: *$T) { memset(t, 0, size_of(T)); };

is_numeric_type :: (t: Type) -> bool {
    ti := t.(*Type_Info);
    return ti.type == .INTEGER || ti.type == .FLOAT;
}

is_numeric_type :: (ti: *Type_Info) -> bool {
    return ti.type == .INTEGER || ti.type == .FLOAT;
}

// Small_Any: useful for when you need to store a small value with a type tag
//            used in this module for number literals
// NOTE: value is not actually stored as a u64, this is just the backig space for the value
//       you can get the correct value out by casting pointer to 'value' to the signified type
Small_Any :: struct {
    type:   *Type_Info;
    value:  u64; 
}

to_any :: inline (any: Small_Any) -> Any { 
    return Any.{ any.type, *any.value }; 
};

get_as_type :: inline (any: Small_Any, $T: Type) -> T, bool #modify {
    return T.(*Type_Info).runtime_size <= 8;
} { 
    return (*any.value).(*T).*, any.type == type_info(T);
}

make_small_any :: inline (value: $T) -> Small_Any #modify {
    return T.(*Type_Info).runtime_size <= 8;
} { 
    // init with zerp and then copy value just so we don't get some weird random bytes copied over somehow
    ret := Small_Any.{ type_info(T), 0 }; 
    (*ret.value).(*T).* = value;
    return ret;
}

// NOTE: this would not work with dynamic type infos, 
//       as that would require a more complex switch case
//       on type of type, runtime size, and signedness
negate_number :: inline (any: *Small_Any) {
    if as_type(any.type) == {
      // case u\8;     val := (*any.value).(*u\8);     val.* = -val.*;
      // case u16;     val := (*any.value).(*u16);     val.* = -val.*;
      // case u32;     val := (*any.value).(*u32);     val.* = -val.*;
      // case u64;     val := (*any.value).(*u64);     val.* = -val.*;
      case s\8;     val := (*any.value).(*s\8);     val.* = -val.*;
      case s16;     val := (*any.value).(*s16);     val.* = -val.*;
      case s32;     val := (*any.value).(*s32);     val.* = -val.*;
      case s64;     val := (*any.value).(*s64);     val.* = -val.*;
      case float32; val := (*any.value).(*float32); val.* = -val.*;
      case float64; val := (*any.value).(*float64); val.* = -val.*;
      
      case; assert(false, "Small_Any passed to negate_number was not a valid numeric type! (was %)", as_type(any.type));
    }
}



// copied from how_to/170_modify.jai
Dynamic_New :: (ti: *Type_Info, initialized := true) -> *void {
    size := ti.runtime_size;
    if size < 0 return null;   // Maybe they gave us a weird polymorphic type for some reason.

    memory := alloc(size);

    if initialized {
        if ti.type == .STRUCT {
            // Structs can have initializers. If the initializer is null, the struct is to be zeroed.
            tis := ti.(*Type_Info_Struct);
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
    return (*ti).(*Type).*;
}





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
