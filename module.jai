
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

free_script :: (using script: *Script) {
    array_free(variables);
    array_free(procedures);
    array_free(directives);
    array_free(type_table);
    fini(*pool);
    if script.dyncall_vm  dcFree(script.dyncall_vm);
    
    script.* = .{};
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


// STACK STUFF

STACK_SENTINEL_VALUE: u64 : 0x0123_4567_89AB_CDEF;

PTR_SIZE :: size_of(*void);

stack_pop :: inline (script: *Script, $$is_lvalue := false) -> *void {
    script.stack_ptr -= PTR_SIZE;
    assert(script.stack_ptr >= script.stack_base, "stack_ptr offset: %", script.stack_ptr - script.stack_base);
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


// PARSING STUFF

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
    
    // check if expression can be used as a statement
    if expect_token_type(*lexer, .SEMICOLON) {
        if left.node_type == {
          case Node_Procedure_Call;
            left.flags |= .DISCARD_VALUE; // do not allocate space for result if its an aggregate type and do not push anythign to stack when executing
            return left;
            
          case Node_Operation;
            operation := left.(*Node_Operation);
            if operation.operator_type == .ASSIGNMENT {
                return left;
            }
            log("Error: operator of type % cannot be used as a statement.", operation.operator_type);
            return null;
            
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
    
    node:     *Node;
    unary_op: *Node_Operation;
    
    token := get_token_or_return(*lexer, null);
    if token_is_unary_operator(token) {
        dbg_print("UNARY OPERATOR (%)\n", token.text);
        unary_op = alloc_node(script, Node_Operation);
        unary_op.name = token.text;
        unary_op.operator_type = .UNARY;
        // unary_op.left gets set at the bottom, when 'node' actually points to something.
        token = get_token_or_return(*lexer, null);
    }
    
    
    if token.type == {
      case .DOT;
        dbg_print("DOT (UNARY)\n");
        if expect_token_type(*lexer, .OPEN_BRACE) {
            literal := alloc_node(script, Node_Literal);
            literal.literal_type = .STRUCT;
            ok: bool;
            literal.aggr.expressions, _, ok = parse_comma_separated_expressions(script, .CLOSE_BRACE);
            if !ok  return null;
            node = literal;
        }
        else log("Error: unexpected DOT token at (%:%)", token.src_loc.line, token.src_loc.char);
        // TODO: identifer -> unary dot enum thing
        
        
      case .OPEN_PAREN;
        open_paren_location := token.src_loc;
        
        node = parse_expression(script, 0);
        if node == null  return null;
        
        token = get_token_or_return(*lexer, null);
        if token.type != .CLOSE_PAREN {
            log("Error: expected a closing paren for open paren at (%:%)", open_paren_location.line, open_paren_location.char);
            return null;
        }
        
        
      case .NUMBER;
        dbg_print("NUMBER (%)\n", token.text);
        value, ok := parse_number(token.text);
        if !ok  return null;
        
        literal := alloc_node(script, Node_Literal);
        literal.literal_type = .NUMBER;
        literal.number = value;
        node = literal;
        
        
      case .STRING;
        dbg_print("STRING (\"%\")\n", token.text);
        literal := alloc_node(script, Node_Literal);
        literal.literal_type = .STRING;
        literal.text = token.text;
        node = literal;
        
    
      case .DIRECTIVE;
        dbg_print("DIRECTIVE\n"); dbg_print_push_indent();
        directive := alloc_node(script, Node_Directive);
        directive.name = token.text;
        
        directive.directive_index = -1;
        for script.directives {
            if it.name == directive.name {
                directive.directive_index = it_index;
            }
        }
        if directive.directive_index == -1  return null;
        
        if !expect_token_type(*lexer, .OPEN_PAREN) {
            dbg_print("Error, expected an open paren after directive name!\n");
            return null;
        }
        
        ok: bool;
        directive.arguments, directive.arguments_count, ok = parse_comma_separated_expressions(script, .CLOSE_PAREN);
        if !ok  return null;
        
        if !execute_directive(script, directive) {
            log("Error: failed while trying to execute a directive.");
            return null;
        }
        node = directive;
        
        
      case .IDENTIFIER;
        dbg_print("IDENTIFIER (%)\n", token.text);
        if is_reserved_word(token.text) {
            log("Error: Invalid use of reserved word '%' at line %, char %.", token.text, token.src_loc.line, token.src_loc.char);
            return null;
        }
        node = make_identifier(script, token.text);
        
        
      case;
        log("Error: Unexpected token type '%' at (%:%). Expected a number or identifier.\n", token.type, token.src_loc.line, token.src_loc.char);
    }
    
    if unary_op != null {
        unary_op.left = node;
        node = xx unary_op;
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

parse_expression :: (using script: *Script, min_prec: int) -> *Node {
    // dbg_print("parse_expression(min_prec = %)\n", min_prec); 
    // dbg_print_push_indent();
    
    left := parse_leaf(script);
    if left == null  return null;
    
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
        
        token_after_dot := get_token(*lexer);
        if token_after_dot.type == {
          case .OPEN_PAREN;
            dbg_print("CAST\n");
            
            // TODO: in the future maybe we can have more complex type expressions, but that's not really a priority.
            token_identifier := get_token(*lexer);
            if token_identifier.type == .IDENTIFIER {
                // verify that identifier is followed by closing paren
                token_close_paren := get_token(*lexer);
                if token_close_paren.type != .CLOSE_PAREN {
                    log("Error: unexpected token '%' in cast type expression at (%:%). Expected a closing paren.", token_close_paren.type, token_close_paren.src_loc.line, token_close_paren.src_loc.char);
                    return null;
                }
                
                node_cast := alloc_node(script, Node_Cast);
                node_cast.left  = left;
                node_cast.right = make_identifier(script, token_identifier.text);
                return node_cast;
            }
            
            log("Error: unexpected token '%' in cast type expression at (%:%). Expected a type identifier.", token_identifier.type, token_identifier.src_loc.line, token_identifier.src_loc.char);
            return null;
            
            
          case .IDENTIFIER;
            node := alloc_node(script, Node_Dot);
            node.left   = left;
            node.right  = make_identifier(script, token_after_dot.text);
            return node;
            
            
          case .OPEN_BRACE;
            if left.node_type != Node_Identifier {
                log("Error: unexpected % node in type slot of struct literal at (%:%)", left.node_type, token.src_loc.line, token.src_loc.char);
            }
            node := alloc_node(script, Node_Literal);
            node.literal_type = .STRUCT;
            node.aggr.type_identifier = xx left;
            init_expressions, _, ok := parse_comma_separated_expressions(script, .CLOSE_BRACE);
            if !ok  return null;
            node.aggr.expressions = init_expressions;
            return node;
        }
        
        log("Error: unexpected % token '%' after dot at (%:%)", token.type, token.text, token.src_loc.line, token.src_loc.char);
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
    
    is_operator, operator_type, precedence := token_is_operator(token);
    if is_operator && operator_type != .UNARY {
        if precedence <= min_prec  return left;
        dbg_print("% OPERATION (%)\n", operator_type, token.text);
        get_token(*lexer);
        
        right := parse_expression(script, precedence);
        if right == null  return null;
        
        node := alloc_node(script, Node_Operation);
        node.name          = token.text;
        node.operator_type = operator_type;
        node.left          = left;
        node.right         = right;
        return node;
    }
    
    return left;
}

// TODO: add source location parameter. we should actually do this for all nodes
make_identifier :: (script: *Script, name: string) -> *Node_Identifier {
    node := alloc_node(script, Node_Identifier);
    node.name = name;
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


/* 
    Small_Any is an Any, but we only use it for small values (<= 8 bytes), 
        which get packed into the space of the value_pointer.
    Maybe in the future I will also use this to double as a standard any, when we need to point to larger values.
*/
Small_Any :: #type,distinct Any; 

to_any :: inline (any: Small_Any) -> Any { 
    return Any.{ any.type, *any.value_pointer }; 
};

get_as_type :: inline (any: Small_Any, $T: Type) -> T, bool #modify {
    return T.(*Type_Info).runtime_size <= 8;
} { 
    return (*any.value_pointer).(*T).*, any.type == type_info(T);
}

make_small_any :: inline (value: $T) -> Small_Any #modify {
    return T.(*Type_Info).runtime_size <= 8;
} { 
    // init with zerp and then copy value just so we don't get some weird random bytes copied over somehow
    ret := Small_Any.{ type_info(T), null }; 
    (*ret.value_pointer).(*T).* = value;
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
