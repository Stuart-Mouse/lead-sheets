
parse_source_file :: (script: *Script, source: string) -> bool {
    if !(script.flags & .INITIALIZED)  return false;
    
    success: bool;
    defer if !success  free_script(script);
    
    init_lexer(*script.lexer, source);
    
    if script.ast_root == null {
        script.ast_root = alloc_node(script, Node_Block);
        script.current_parent_node = script.ast_root;
    }
    
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

parse_statement :: (using script: *Script, expect_semicolon := true) -> *Node {
    token := peek_token(*lexer); // TODO: probably just get_toke_or_return(), then switch on type.
    
    if token.type == {
      case .IF; 
        dprint("IF STATEMENT\n"); dprint_push_indent();
        get_token(*lexer);
        if_statement := alloc_node(script, Node_If_Statement);
        current_parent_node = if_statement; 
        
        dprint("CONDITION\n"); dprint_push_indent();
        if_statement.condition = parse_expression(script, 0);
        if if_statement.condition == null  return null;
        
        dprint("STATEMENT\n"); dprint_push_indent();
        if_statement.statement = parse_statement(script);
        if if_statement.statement == null  return null;
        
        current_parent_node = if_statement.parent; 
        return if_statement;
        
      case .WHILE;
        dprint("WHILE LOOP\n"); dprint_push_indent();
        get_token(*lexer);
        while_loop := alloc_node(script, Node_While_Loop);
        current_parent_node = while_loop; 
        
        dprint("CONDITION\n");
        while_loop.condition = parse_expression(script, 0);
        if while_loop.condition == null  return null;
        
        dprint("STATEMENT\n");
        while_loop.statement = parse_statement(script);
        if while_loop.statement == null  return null;
        
        current_parent_node = while_loop.parent; 
        return while_loop;
        
      case .FOR;
        dprint("FOR LOOP\n"); dprint_push_indent();
        get_token(*lexer);
        for_loop := alloc_node(script, Node_For_Loop);
        current_parent_node = for_loop; 
        
        dprint("CONTROL EXPRESSION\n");
        expr := parse_expression(script, 0);
        if !expr  return null;
        
        // TODO: maybe allow comma separated list of elements to iterate over instead of array literals? 
        //       may jive better with lack of pointers, since lvalue-ness can be implicit
        if expect_token_type(*lexer, .SPREAD) {
            dprint("CONTROL TYPE IS RANGE\n");
            for_loop.control_type = .RANGE;
            for_loop.range.lower  = expr;
            for_loop.range.upper  = parse_expression(script, 0);
            if !for_loop.range.upper  return null;
        } else {
            dprint("CONTROL TYPE IS ARRAY\n");
            for_loop.control_type = .ARRAY;
            for_loop.array_expression = expr;
            if for_loop.array_expression == null  return null;
        }
        
        dprint("STATEMENT\n");
        for_loop.statement = parse_statement(script);
        if for_loop.statement == null  return null;
        
        current_parent_node = for_loop.parent; 
        return for_loop;
        
      case .FOREACH;
        dprint("FOREACH LOOP\n"); dprint_push_indent();
        get_token(*lexer);
        for_loop := alloc_node(script, Node_For_Loop);
        current_parent_node = for_loop; 
        
        dprint("ITERATOR LIST\n");
        if !expect_token_type(*lexer, .OPEN_PAREN) {
            log("Error: an open paren is required to begin a foreach iterator list.");
            return null;
        }
        expressions, _, ok := parse_comma_separated_expressions(script, .CLOSE_PAREN);
        if !ok  return null;
        
        for_loop.control_type = .LIST;
        for_loop.list = expressions;
        
        dprint("STATEMENT\n");
        for_loop.statement = parse_statement(script);
        if for_loop.statement == null  return null;
        
        current_parent_node = for_loop.parent; 
        return for_loop;
        
      case .OPEN_BRACE;
        dprint("BLOCK\n"); dprint_push_indent();
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
    
    dprint("EXPRESSION...\n");
    
    left := parse_expression(script, 0);
    if left == null  return null;
    
    if expect_token_type(*lexer, .COLON) {
        // maybe a bit of a hack, but for now I will just throw away the 
        // identifier node here and transfer the name/source_location to a declaration node 
        if left.node_type != Node_Identifier {
            log("Error: left hand side of a declaration must be an identifier.");
            return null;
        }
        
        // colon followed by open brace is a named block, not a standard declaration
        // maybe we should make a named block a type of declaration so that we can resolve it by identifier in the standard way? idk yet...
        // also, for now we will require that all named blocks are top-level, since we will actually just push these blocks into a special array
        if peek_token(*lexer).type == .OPEN_BRACE {
            dprint("NAMED BLOCK (%)\n", left.(*Node_Identifier).name); dprint_push_indent();
            if script.current_parent_node != script.ast_root {
                log("Error: named blocks can only be declared at the top-level scope.");
                return null;
            }
            
            block := parse_statement(script).(*Node_Block);
            if block == null  return null;
            if block.node_type != Node_Block {
                log("Error: expected a Node_Block, got a %.", block.node_type);
                return null;
            }
            block.name = left.(*Node_Identifier).name;
            
            array_add(*script.named_blocks, block);
            return block;
        }
        
        
        dprint("DECLARATION\n"); dprint_push_indent();
        
        declaration := alloc_node(script, Node_Declaration);
        declaration.name = left.(*Node_Identifier).name;
        // declaration.source_location = left.source_location;
        
        next_token_type := peek_token(*lexer).type;
        if next_token_type != .ASSIGN_EQUAL && next_token_type != .COLON {
            dprint("WITH EXPLICIT TYPE\n");
            declaration.type_expression = xx parse_expression(script, 999);
            if !declaration.type_expression  return null;
            if declaration.type_expression.node_type != Node_Identifier {
                log("Error: type expression of a declaration must be an identifier. (was %)", declaration.type_expression.node_type);
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
            dprint("WITH ASSIGNMENT\n");
            declaration.init_expression = parse_expression(script, 0);
            if declaration.init_expression == null  return null;
            if is_constant_declaration then declaration.flags |= .IS_CONSTANT;
        }
        
        if expect_semicolon && !expect_token_type(*lexer, .SEMICOLON)  return null;
        
        return declaration;
    }
    
    if expect_token_type(*lexer, .SEMICOLON) || !expect_semicolon {
        // do not allocate space for result if its an aggregate type and do not push anything to stack when executing
        left.flags |= .DISCARD_VALUE; 
        return left;
    }
    
    return null;
}



// This procedure is kinda incomplete and doesn't do everything yet, but here's the plan...
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
    // dprint("parse_leaf()\n");
    // dprint_push_indent();
    
    node:     *Node;
    unary_op: *Node_Operation;
    
    token := get_token_or_return(*lexer, null);
    is_operator, operator_type := token_is_operator(token, expect_unary = true);
    if is_operator && operator_type == .UNARY {
        dprint("UNARY OPERATOR (%)\n", token.text);
        unary_op = alloc_node(script, Node_Operation);
        unary_op.name = token.text;
        unary_op.operator_type = .UNARY;
        // unary_op.left gets set at the bottom, when 'node' actually points to something.
        token = get_token_or_return(*lexer, null);
    }
    
    if token.type == {
      case .DOT;
        dprint("DOT (UNARY)\n");
        token_after_dot := get_token(*lexer);
        if token_after_dot.type == .OPEN_BRACE {
            literal := alloc_node(script, Node_Literal);
            literal.literal_type = .STRUCT;
            ok: bool;
            literal.aggr.expressions, _, ok = parse_comma_separated_expressions(script, .CLOSE_BRACE);
            if !ok  return null;
            node = literal;
        }
        else if token_after_dot.type == .IDENTIFIER {
            literal := alloc_node(script, Node_Literal);
            literal.literal_type     = .ENUM;
            literal._enum.identifier = token_after_dot.text;
            node = literal;
        }
        else log("Error: unexpected DOT token at (%:%)", token_after_dot.source_location.line, token_after_dot.source_location.char);

        
      case .OPEN_PAREN;
        open_paren_location := token.source_location;
        
        node = parse_expression(script, 0);
        if node == null  return null;
        node.flags |= .PARENTHESIZED;
        
        token = get_token_or_return(*lexer, null);
        if token.type != .CLOSE_PAREN {
            log("Error: expected a closing paren for open paren at (%:%)", open_paren_location.line, open_paren_location.char);
            return null;
        }
        
        
      case .NUMBER;
        dprint("NUMBER (%)\n", token.text);
        node = make_number_literal(script, token);
        
        
      case .STRING;
        dprint("STRING (\"%\")\n", token.text);
        literal := alloc_node(script, Node_Literal);
        literal.literal_type = .STRING;
        literal.text = token.text;
        node = literal;
        
    
      case .DIRECTIVE;
        dprint("DIRECTIVE\n"); dprint_push_indent();
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
            dprint("Error, expected an open paren after directive name!\n");
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
        dprint("IDENTIFIER (%)\n", token.text);
        node = make_identifier(script, token.text);
        
        
      case;
        log("Error: Unexpected token type '%' at (%:%). Expected a number or identifier.\n", token.type, token.source_location.line, token.source_location.char);
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


// will parse a single string as a statement, checking that we end on EOF token
// this is useful for use cases where we are just using the script as a context for evaluating 
parse_statement :: (script: *Script, statement: string, allocator: *Allocator = null, expect_eof := true) -> *Node, *u8 {
    // just in case, we will backup current lexer and just attach a temporary one to the script
    prev_lexer := script.lexer;
    defer script.lexer = prev_lexer;
    
    prev_allocator := script.allocator;
    defer script.allocator = prev_allocator;
    if allocator  script.allocator = allocator;
    
    // we have to reinit lexer for every statement
    init_lexer(*script.lexer, statement);
    node := parse_statement(script, expect_semicolon = false);
    
    if expect_eof && !expect_token_type(*script.lexer, .EOF)  return null, null;
    return node, script.lexer.ptr_after_last_token;
}

// will parse a single string as an expression, checking that we end on EOF token
// this is useful for use cases where we are just using the script as a context for evaluating 
parse_expression :: (script: *Script, expr: string, allocator: *Allocator = null, expect_eof := true) -> *Node, *u8 {
    // just in case, we will backup current lexer and just attach a temporary one to the script
    prev_lexer := script.lexer;
    defer script.lexer = prev_lexer;
    
    prev_allocator := script.allocator;
    defer script.allocator = prev_allocator;
    if allocator  script.allocator = allocator;
    
    // we have to reinit lexer for every expression
    // TODO: set the lexer source location based on dom node?
    init_lexer(*script.lexer, expr);
    expression := parse_expression(script, 0);
    
    if expect_eof && !expect_token_type(*script.lexer, .EOF)  return null, null;
    return expression, script.lexer.ptr_after_last_token;
}

parse_expression :: (using script: *Script, min_prec: int) -> *Node {
    // dprint("parse_expression(min_prec = %)\n", min_prec); 
    // dprint_push_indent();
    
    left := parse_leaf(script);
    if left == null  return null;
    
    // dprint("parsed a leaf: %\n", sprint_ast_node(script, left));
    
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
    // dprint("parse_increasing_precedence(left_i = %, min_prec = %)\n", left, min_prec); 
    // dprint_push_indent();
    
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
            dprint("CAST\n");
            
            // TODO: in the future maybe we can have more complex type expressions, but that's not really a priority.
            token_identifier := get_token(*lexer);
            if token_identifier.type == .IDENTIFIER {
                // verify that identifier is followed by closing paren
                token_close_paren := get_token(*lexer);
                if token_close_paren.type != .CLOSE_PAREN {
                    log("Error: unexpected token '%' in cast type expression at (%:%). Expected a closing paren.", token_close_paren.type, token_close_paren.source_location.line, token_close_paren.source_location.char);
                    return null;
                }
                
                node_cast := alloc_node(script, Node_Cast);
                node_cast.value = left;
                node_cast.type_identifier = make_identifier(script, token_identifier.text);
                return node_cast;
            }
            
            log("Error: unexpected token '%' in cast type expression at (%:%). Expected a type identifier.", token_identifier.type, token_identifier.source_location.line, token_identifier.source_location.char);
            return null;
            
            
          case .IDENTIFIER;
            node := alloc_node(script, Node_Dot);
            node.left   = left;
            node.right  = make_identifier(script, token_after_dot.text);
            return node;
            
            
          case .OPEN_BRACE;
            if left.node_type != Node_Identifier {
                log("Error: unexpected % node in type slot of struct literal at (%:%)", left.node_type, token.source_location.line, token.source_location.char);
            }
            node := alloc_node(script, Node_Literal);
            node.literal_type = .STRUCT;
            node.aggr.type_identifier = xx left;
            init_expressions, _, ok := parse_comma_separated_expressions(script, .CLOSE_BRACE);
            if !ok  return null;
            node.aggr.expressions = init_expressions;
            return node;
        }
        
        log("Error: unexpected % token '%' after dot at (%:%)", token.type, token.text, token.source_location.line, token.source_location.char);
        return null;
        
      case .OPEN_BRACKET;
        get_token(*lexer);
        dprint("ARRAY SUBSCRIPT\n");
        
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
        dprint("% OPERATION (%)\n", operator_type, token.text);
        get_token(*lexer);
        
        right := parse_expression(script, precedence);
        if right == null  return null;
        
        node := alloc_node(script, Node_Operation);
        node.name          = token.text;
        node.operator_type = operator_type;
        node.left          = left;
        node.right         = right;
        if operator_type == .ASSIGNMENT {
            node.flags |= .MUST_BE_STATEMENT_ROOT;
        }
        return node;
    }
    
    return left;
}
