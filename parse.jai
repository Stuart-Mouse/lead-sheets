// NOTE: file path only used for error reporting
parse_source_file :: (script: *Script, source: string, file_path := "") -> bool {
    if !(script.flags & .INITIALIZED)  return false;
    
    init_lexer(script, source, location = .{ file_path, 1, 1 });
    
    // TODO: this really should not be a check 
    if script.ast_root == null {
        script.ast_root = alloc_node(script, Node_Block);
    }
    script.current_scope = script.ast_root;
    
    statements, ok := parse_statements(script,, temp);
    if !ok  return false;
    script.ast_root.statements = array_copy(statements,, get_pool_allocator(*script.pool));
    
    assert(script.current_scope == script.ast_root);
    
    script.flags |= .PARSED;
    
    if !typecheck_script(script)  return false;
    
    return true;
}

parse_statements :: (using script: *Script, break_token_type: Token_Type = .EOF) -> ([] *Node, bool) {
    statements: [..] *Node;
    while true {
        if expect_token_type(script, break_token_type) break;
        if expect_token_type(script, .EOF) break;
        stmt := parse_statement(script);
        if stmt == null {
            array_reset(*statements);
            return .[], false;
        }
        array_add(*statements, stmt);
    }
    return statements, true;
}

parse_statement :: (using script: *Script, expect_semicolon := true) -> *Node {
    token := peek_token(script);
    if token.type == {
      case .IF; 
        dprint("IF STATEMENT\n"); dprint_push_indent();
        if_token := get_token(script);
        if_statement := alloc_node(script, Node_If_Statement, loc = if_token.location, trivia = if_token.trivia);
        current_scope = if_statement; 
        
        dprint("CONDITION\n"); dprint_push_indent();
        if_statement.condition = my_parse_expression(script, 0);
        if if_statement.condition == null  return null;
        
        dprint("STATEMENT\n"); dprint_push_indent();
        if_statement.statement = parse_statement(script, expect_semicolon);
        if if_statement.statement == null  return null;
        
        current_scope = if_statement.scope; 
        return if_statement;
        
      case .WHILE;
        dprint("WHILE LOOP\n"); dprint_push_indent();
        while_token := get_token(script);
        while_loop := alloc_node(script, Node_While_Loop, loc = while_token.location, trivia = while_token.trivia);
        current_scope = while_loop; 
        
        dprint("CONDITION\n");
        while_loop.condition = my_parse_expression(script, 0);
        if while_loop.condition == null  return null;
        
        dprint("STATEMENT\n");
        while_loop.statement = parse_statement(script, expect_semicolon);
        if while_loop.statement == null  return null;
        
        current_scope = while_loop.scope; 
        return while_loop;
        
      case .FOR;
        dprint("FOR LOOP\n"); dprint_push_indent();
        for_token := get_token(script);
        for_loop := alloc_node(script, Node_For_Loop, loc = for_token.location, trivia = for_token.trivia);
        current_scope = for_loop; 
        
        dprint("CONTROL EXPRESSION\n");
        expr := my_parse_expression(script, 0);
        if !expr  return null;
        
        if expect_token_type(script, .SPREAD) {
            dprint("CONTROL TYPE IS RANGE\n");
            for_loop.control_type = .RANGE;
            for_loop.range.lower  = expr;
            for_loop.range.upper  = my_parse_expression(script, 0);
            if !for_loop.range.upper  return null;
        } else {
            dprint("CONTROL TYPE IS ARRAY\n");
            for_loop.control_type = .ARRAY;
            for_loop.array_expression = expr;
            if for_loop.array_expression == null  return null;
        }
        
        dprint("STATEMENT\n");
        for_loop.statement = parse_statement(script, expect_semicolon);
        if for_loop.statement == null  return null;
        
        current_scope = for_loop.scope; 
        return for_loop;
        
      case .FOREACH;
        dprint("FOREACH LOOP\n"); dprint_push_indent();
        for_token := get_token(script);
        for_loop := alloc_node(script, Node_For_Loop, loc = for_token.location, trivia = for_token.trivia);
        current_scope = for_loop; 
        
        dprint("ITERATOR LIST\n");
        open_paren := get_token(script);
        if open_paren.type != .OPEN_PAREN {
            set_parse_error(script, "%: expected an open paren to begin a foreach iterator list.", open_paren.location);
            return null;
        }
        if peek_token(script).type == .CLOSE_PAREN {
            close_paren := peek_token(script);
            set_parse_error(script, "%: foreach loop cannot have an empty iterator list.", close_paren.location);
            return null;
        }
        expressions, ok := parse_comma_separated_expressions(script,, temp);
        if !ok  return null;
        
        for_loop.list = array_copy(expressions,, get_pool_allocator(*script.pool));
        for_loop.control_type = .LIST;
        
        close_paren := get_token(script);
        if close_paren.type != .CLOSE_PAREN {
            set_parse_error(script, "%: expected closing paren after foreach loop iterator list. Instead we saw %\n", close_paren.location, close_paren.text);
            return null;
        }
        
        dprint("STATEMENT\n");
        for_loop.statement = parse_statement(script, expect_semicolon);
        if for_loop.statement == null  return null;
        
        current_scope = for_loop.scope; 
        return for_loop;
        
      case .OPEN_BRACE;
        dprint("BLOCK\n"); dprint_push_indent();
        open_brace := get_token(script);
        block := alloc_node(script, Node_Block, loc = open_brace.location, trivia = open_brace.trivia);
        current_scope = block; 
        
        statements, ok := parse_statements(script, .CLOSE_BRACE);
        if !ok  return null;
        block.statements = array_copy(statements,, get_pool_allocator(*script.pool));
        
        current_scope = block.scope; 
        return block;
    }
    
    /*
        TODO:
            we can improve this section if we beef up the lexer so that we scan a few more tokens ahead, or pre-tokenize the entire file.
            If we could peek 2 tokens ahead we could capture the identifer + colon case before the general expression case.
            which would prevent us from making the identifier node and throwing it away just to stick the name on the declaration or assignment expression
        NOTE:
            after later consideration, I've decided not to do the above, even though I've increased the lexer lookahead.
            There are two reasons for this:
                1. We can provide a better error message below, since we can recognize that some invalid expression was used on the left-hand side of a declaration, where only an identifier should go.
                2. I am now thining that perhaps we will want to be able to do the same identifier renaming for declarations that we support (or are working on supporting) for other cases of identifiers.
                    If we go through with this, we will need to stop throwing away the identifier on the declaration and store a *Node instead of the name as a string.
    */
    
    dprint("EXPRESSION...\n");
    
    left := my_parse_expression(script, 0);
    if left == null  return null;
    
    if expect_token_type(script, .COLON) {
        // maybe a bit of a hack, but for now I will just throw away the 
        // identifier node here and transfer the name/source_location to a declaration node 
        // if left.node_type != Node_Identifier {
        //     set_parse_error(script, "%: left hand side of a declaration must be an identifier.", get_location(left));
        //     return null;
        // }
        
        // colon followed by open brace is a named block, not a standard declaration
        // maybe we should make a named block a type of declaration so that we can resolve it by identifier in the standard way? idk yet...
        // also, for now we will require that all named blocks are top-level, since we will actually just push these blocks into a special array
        if peek_token(script).type == .OPEN_BRACE {
            dprint("NAMED BLOCK (%)\n", left.(*Node_Identifier).name); dprint_push_indent();
            if script.current_scope != script.ast_root {
                set_parse_error(script, "%: named blocks can only be declared at the top-level scope.", get_location(left));
                return null;
            }
            
            block := parse_statement(script).(*Node_Block);
            if block == null  return null;
            assert(block.node_type == Node_Block);
            block.name = left.(*Node_Identifier).name;
            block.location = left.location;
            block.trivia   = left.trivia;
            
            array_add(*script.named_blocks, block);
            return block;
        }
        
        
        dprint("DECLARATION\n"); dprint_push_indent();
        
        declaration := alloc_node(script, Node_Declaration, loc = left.location, trivia = left.trivia);
        declaration.left = left.(*Node_Identifier);
        
        next_token := peek_token(script);
        if !is_operator(next_token, "=") && next_token.type != .COLON {
            dprint("WITH EXPLICIT TYPE\n");
            declaration.type_expression = my_parse_expression(script, 999); // @Hack 
            if !declaration.type_expression  return null;
            if declaration.type_expression.node_type != Node_Identifier {
                set_parse_error(script, "%: type expression of a declaration must be an identifier. (was %)", get_location(left), declaration.type_expression.node_type);
                return null;
            }
        }
        
        do_assignment := false;
        is_macro_declaration := false;
        
        next_token = peek_token(script);
        if is_operator(next_token, "=") {
            get_token(script);
            do_assignment = true;
        }
        else if next_token.type == .COLON {
            get_token(script);
            do_assignment = true;
            is_macro_declaration = true;
        }
        
        if do_assignment {
            dprint("WITH ASSIGNMENT\n");
            declaration.init_expression = my_parse_expression(script, 0);
            if declaration.init_expression == null  return null;
            if is_macro_declaration then declaration.flags |= .MACRO;
        }
        
        if !expect_token_type(script, .SEMICOLON) && expect_semicolon {
            return null;
        }
        
        return declaration;
    }
    
    if expect_token_type(script, .SEMICOLON) || !expect_semicolon {
        // do not allocate space for result if its an aggregate type and do not push anything to stack when executing
        left.flags |= .DISCARD_VALUE; 
        return left;
    }
    
    log("returning null");
    return null;
}


parse_leaf :: (using script: *Script) -> *Node {
    dprint("parse_leaf()\n");
    dprint_push_indent();
    token := get_token(script);
    
    is_operator, operator_index := get_prefix_operator(script, token);
    if is_operator {
        dprint("PREFIX OPERATOR (%)\n", token.text);
        precedence := get_operator(script, operator_index).precedence;
        left := my_parse_expression(script, precedence);
        if left == null  return null;
        return make_operation(script, token, operator_index, left, null);
    }
    
    if token.type == {
      case .DOT;
        dprint("UNARY DOT\n");
        token_after_dot := get_token(script);
        if token_after_dot.type == .OPEN_BRACE {
            struct_contents, ok := parse_contents_of_struct_literal(script);
            if !ok  return null;
            literal := make_struct_literal(script, token_after_dot, struct_contents);
            return make_dot(script, token, null, literal);
        }
        else if token_after_dot.type == .IDENTIFIER {
            // For now we will make a dot node since there's already logic there for everything we gotta do to resolve the identifier
            // an alternative method here would be to flag the identifier as implicitly namespaced or something
            identifier := make_identifier(script, token_after_dot);
            return make_dot(script, token, null, identifier);
        }
        else set_parse_error(script, "%: unexpected % token '%' after unary dot.", token_after_dot.location, token_after_dot.type, token_after_dot.text);

        
      case .OPEN_PAREN;
        open_paren_location := token.location;
        
        expression := my_parse_expression(script, 0);
        if expression == null  return null;
        expression.flags |= .PARENTHESIZED;
        
        close_paren_token := get_token(script);
        if close_paren_token.type != .CLOSE_PAREN {
            set_parse_error(script, "%: expected a closing paren for open paren at %.", close_paren_token.location, open_paren_location);
            return null;
        }
        return expression;
        
      case .TRUE;
        dprint("TRUE\n");
        literal := alloc_node(script, Node_Literal, loc = token.location, trivia = token.trivia);
        literal.literal_type = .BOOLEAN;
        literal.number       = to_any_number(true);
        return literal;
        
      case .FALSE;
        dprint("FALSE\n");
        literal := alloc_node(script, Node_Literal, loc = token.location, trivia = token.trivia);
        literal.literal_type = .BOOLEAN;
        literal.number       = to_any_number(false);
        return literal;
        
      case .NUMBER;
        dprint("NUMBER (%)\n", token.text);
        return make_number_literal(script, token);
        
        
      case .STRING;
        dprint("STRING (\"%\")\n", token.text);
        return make_string_literal(script, token);
        
    
      case .DIRECTIVE;
        dprint("DIRECTIVE\n"); dprint_push_indent();
        directive := alloc_node(script, Node_Directive, loc = token.location, trivia = token.trivia);
        directive.name = token.text;
        
        directive.directive_index = -1;
        for script.directives {
            if it.name == directive.name {
                directive.directive_index = it_index;
            }
        }
        if directive.directive_index == -1 {
            set_parse_error(script, "%: Unable to resolve directive '%'\n", directive.location, directive.name);
            return null;
        }
        
        if expect_token_type(script, .OPEN_PAREN) {
            if !expect_token_type(script, .CLOSE_PAREN) {
                expressions, ok := parse_comma_separated_expressions(script,, temp);
                if !ok  return null;
                directive.arguments = array_copy(expressions,, get_pool_allocator(*script.pool));
                
                close_paren_token := get_token(script);
                if close_paren_token.type != .CLOSE_PAREN {
                    set_parse_error(script, "%: expected a closing paren after arguments of directive. Instead we saw %\n", close_paren_token.location, close_paren_token.text);
                    return null;
                }
            }
        }
        
        if !evaluate_directive(script, directive, .PARSE) {
            set_parse_error(script, "%: failed while trying to execute a directive during parsing.", token.location);
            return null;
        }
        return directive;
        
        
      case .IDENTIFIER;
        dprint("IDENTIFIER (%)\n", token.text);
        return make_identifier(script, token);
    }
    
    return null;
}

// will parse a single string as a statement, checking that we end on EOF token
// this is useful for use cases where we are just using the script as a context for evaluating 
parse_statement_string :: (script: *Script, statement: string, expect_eof := true) -> *Node {
    // just in case, we will backup current lexer and just attach a temporary one to the script
    prev_lexer := script.lexer;
    defer script.lexer = prev_lexer;
    
    // we have to reinit lexer for every statement
    init_lexer(script, statement);
    node := parse_statement(script, expect_semicolon = false);
    
    if expect_eof && !expect_token_type(script, .EOF) {
        log("%", get_token(script));
        return null;
    }
    return node;
}

// will parse a single string as an expression, checking that we end on EOF token
// this is useful for use cases where we are just using the script as a context for evaluating 
parse_expression_string :: (script: *Script, expr: string, expect_eof := true) -> *Node {
    // just in case, we will backup current lexer and just attach a temporary one to the script
    prev_lexer := script.lexer;
    defer script.lexer = prev_lexer;
    
    // we have to reinit lexer for every expression
    // TODO: set the lexer source location based on dom node?
    init_lexer(*script.lexer, expr);
    expression := my_parse_expression(script, 0);
    
    if expect_eof && !expect_token_type(*script.lexer, .EOF)  return null;
    return expression;
}

parse_expression :: (using script: *Script, min_prec: int) -> *Node {
    left := my_parse_leaf(script);
    if left == null  return null;
    
    // going iteratively, precedence can only decrease
    while true {
        // but going only recursively, precedence will only increase
        node := my_parse_binary(script, left, min_prec);
        if node == null  return null;
        if node == left  break;
        left = node;
    }
    
    return left;
}

// needs to also return bool since node can be null validly if there were no arguments
parse_comma_separated_expressions :: (using script: *Script) -> ([] *Node, bool) {
    expressions: [..] *Node;
    while true {
        expr := my_parse_expression(script, 0);
        if expr == null {
            array_reset(*expressions);
            return .[], false;
        }
        array_add(*expressions, expr);
        if !expect_token_type(script, .COMMA)  break;
    }
    return expressions, true;
}

// If we hit a binary operator but its precedence is too low, we return left back to caller.
parse_binary :: (using script: *Script, left: *Node, min_prec: int) -> *Node {
    token := peek_token(script);
    
    if token.type == .QUESTION_MARK {
        question_mark := get_token(script);
        
        literal := get_terminal_literal(left);
        if !literal || !can_be_malleable(literal) {
            set_parse_error(script, "%: a `?` can only be used directly after a constant literal.", format_location(question_mark.location));
            return null;
        }
        literal.flags |= .IS_MALLEABLE;
        
        token = peek_token(script);
    }
    
    // In addition to normal binary operators, we have a few binary-operator-like constructs that have essentially infinite operator precedence.
    // These are open parenthesis for procedure calls, dot for struct member access and cast, and open bracket for array indexing.
    if token.type == {
      case .OPEN_PAREN;
        get_token(script);
        dprint("PROCEDURE ARGUMENTS LIST\n");
        arg_exprs, ok := parse_procedure_arguments(script);
        if !ok  return null;
        return make_procedure_call(script, token, left, arg_exprs);
        
        
      case .DOT;
        dot_token := get_token(script);
        dprint("BINARY DOT\n");
        
        token_after_dot := get_token(script);
        if token_after_dot.type == {
          case .OPEN_PAREN;
            dprint("CAST\n");
            type_expr := my_parse_expression(script, 0);
            close_paren := get_token(script);
            if close_paren.type != .CLOSE_PAREN {
                set_parse_error(script, "%: unexpected token '%' in cast type expression. Expected a closing paren.", close_paren.location, close_paren.type);
                return null;
            }
            return make_cast(script, dot_token, left, type_expr);
            
          case .IDENTIFIER;
            identifier := make_identifier(script, token_after_dot);
            return make_dot(script, dot_token, left, identifier);
            
          case .OPEN_BRACE;
            struct_contents, ok := parse_contents_of_struct_literal(script);
            if !ok {
                set_parse_error(script, "%: failed to parse contents of struct literal.", token.location);
                return null;
            }
            literal := make_struct_literal(script, token_after_dot, struct_contents);
            return make_dot(script, dot_token, left, literal);
        }
        
        set_parse_error(script, "%: unexpected % token '%' after dot.", token.location, token.type, token.text);
        return null;
        
        
      case .ARROW;
        arrow_token := get_token(script);
        
        token_after_arrow := get_token(script);
        if token_after_arrow.type == {
          case .IDENTIFIER;
            identifier := make_identifier(script, token_after_arrow);
            return make_arrow(script, arrow_token, left, identifier);
        }
        
        set_parse_error(script, "%: unexpected % token '%' after arrow.", token.location, token.type, token.text);
        return null;
        
        
      case .OPEN_BRACKET;
        open_bracket_token := get_token(script);
        dprint("ARRAY SUBSCRIPT\n");
        
        indexing_expr := my_parse_expression(script, 0);
        if indexing_expr == null  return null;
        if !expect_token_type(script, .CLOSE_BRACKET)  return null;
        
        return make_subscript(script, open_bracket_token, left, indexing_expr);
    }
    
    // We only consume the binary operator here if it is of a higher precedence than the previous binary operator.
    // If we hit a binary operator but its precedence is too low, we return left back to caller.
    is_operator, operator_index := get_binary_operator(script, token);
    if is_operator {
        _operator := get_operator(script, operator_index);
        if _operator.precedence <= min_prec  return left;
        
        operator_token := get_token(script);
        right := my_parse_expression(script, _operator.precedence);
        if right == null  return null;
        
        return make_operation(script, operator_token, operator_index, left, right);
    }
    
    return left;
}


parse_contents_of_struct_literal :: (using script: *Script) -> ([] *Node, bool) {
    if expect_token_type(script, .CLOSE_BRACE)  return .[], true;
    
    expressions, ok := parse_comma_separated_expressions(script,, temp);
    if !ok  return .[], false;
    
    // TODO: if first expression is an assignment operation, then we should parse all struct literal inner expressions as such
    //       we will then also need to flag the struct literal as being by-name intialized for evaluation and serialization purposes
    
    close_brace := get_token(script);
    if close_brace.type != .CLOSE_BRACE {
        set_parse_error(script, "%: expected a closing brace after initialization expressions of struct literal. Instead got % '%'\n", close_brace.location, close_brace.type, close_brace.text);
        return .[], false;
    }
    
    return array_copy(expressions,, get_pool_allocator(*script.pool)), true;
}

parse_procedure_arguments :: (using script: *Script) -> ([] *Node, bool) {
    if expect_token_type(script, .CLOSE_PAREN)  return .[], true;
    
    expressions, ok := parse_comma_separated_expressions(script,, temp);
    if !ok  return .[], false;
    
    close_paren_token := get_token(script);
    if close_paren_token.type != .CLOSE_PAREN {
        set_parse_error(script, "%: expected a closing paren after arguments of procedure call. Instead we saw %\n", close_paren_token.location, close_paren_token.text);
        return .[], false;
    }
    
    return array_copy(expressions,, get_pool_allocator(*script.pool)), true;
}
