// NOTE: file path only used for error reporting
parse_source_file :: (script: *Script, source: string, file_path := "") -> bool {
    if !(script.flags & .INITIALIZED)  return false;
    
    lexer: Lexer;
    init_lexer(*lexer, script, source, location = .{ file_path, 1, 1 });
    
    root_block := alloc_node(script, Node_Block);
    script.ast_root      = root_block;
    script.current_scope = root_block;
    
    statements, ok := parse_statements(*lexer,, temp);
    if !ok  return false;
    root_block.statements = array_copy(statements,, get_allocator(script));
    
    assert(script.current_scope == script.ast_root);
    
    script.flags |= .PARSED;
    
    if !typecheck_script(script)  return false;
    
    return true;
}

parse_statements :: (lexer: *Lexer, break_token_type: Token_Type = .EOF) -> ([] *Node, bool) {
    statements: [..] *Node;
    while true {
        if expect_token_type(lexer, break_token_type) break;
        if expect_token_type(lexer, .EOF) break;
        stmt := parse_statement(lexer);
        if stmt == null {
            array_reset(*statements);
            return .[], false;
        }
        array_add(*statements, stmt);
    }
    return statements, true;
}

// TODO: probably remove this as part of doing blocks as expressions
// used in if/for/while, where we need to wrap single statements in a block implicitly.
parse_block_or_statement :: (lexer: *Lexer, block: *Node_Block, expect_semicolon := true) -> bool {
    lexer.script.current_scope = block; 
    defer lexer.script.current_scope = block.scope; 
    
    open_brace := peek_token(lexer);
    if open_brace.type == .OPEN_BRACE {
        get_token(lexer);
        statements, ok := parse_statements(lexer, .CLOSE_BRACE);
        if !ok  return false;
        block.statements = array_copy(statements,, get_allocator(lexer.script));
        block.location = open_brace.location;
        block.trivia   = open_brace.trivia;
    } else {
        statement := parse_statement(lexer, expect_semicolon);
        if statement == null {
            if !has_error(lexer.script) {
                set_parse_error(lexer.script, "Expected a statement.", loc = lexer.location);
            }
            return false;
        }
        block.statements = NewArray(1, *Node,, get_allocator(lexer.script));
        block.statements[0] = statement;
        block.location = statement.location;
        block.flags |= .IMPLICIT;
    }
    
    return true;
}

parse_statement :: (lexer: *Lexer, expect_semicolon := true) -> *Node {
    token := peek_token(lexer);
    if token.type == {
      case .BREAK;
        debug_print("BREAK"); debug_print_push_indent();
        break_token := get_token(lexer);
        _break := alloc_node(lexer.script, Node_Break, loc = break_token.location, trivia = break_token.trivia);
        
        open_bracket_token := peek_token(lexer);
        if open_bracket_token.type == .OPEN_BRACKET {
            get_token(lexer);
            debug_print("TARGET"); debug_print_push_indent();
            
            _break.target_identifier = xx lexer.script.my_parse_expression(lexer, 0);
            if _break.target_identifier == null  return null;
            if _break.target_identifier.node_type != Node_Identifier {
                set_parse_error(lexer.script, "Expected an identifier for break target.", loc = _break.target_identifier.location);
                return null;
            }
            
            close_bracket_token := get_token(lexer);
            if close_bracket_token.type != .CLOSE_BRACKET {
                set_parse_error(lexer.script, "Expected a closing bracket after break target identifier, but saw % token '%'.", close_bracket_token.type, close_bracket_token.text, loc = close_bracket_token.location);
                return null;
            }
        }
        
        debug_print("EXPRESSION"); debug_print_push_indent();
        
        if !expect_token_type(lexer, .SEMICOLON) {
            _break.value_expression = lexer.script.my_parse_expression(lexer, 0);
            if _break.value_expression == null  return null;
            
            if !check_for_semicolon(lexer, _break.value_expression, expect_semicolon) {
                token = peek_token(lexer);
                set_parse_error(lexer.script, "Expected a semicolon after statement (%), but saw % token: '%'", _break.value_expression.node_type, token.type, token.text, loc = lexer.location);
                return null;
            }
        }
        
        return _break;
        
      case .IF; 
        debug_print("IF (STATEMENT)"); debug_print_push_indent();
        if_token := get_token(lexer);
        if_statement := alloc_node(lexer.script, Node_If_Statement, loc = if_token.location, trivia = if_token.trivia);
        
        lexer.script.current_scope = if_statement;  
        defer lexer.script.current_scope = if_statement.scope;
        
        if_statement.then_block = alloc_node(lexer.script, Node_Block);
        if_statement.then_block.scope = if_statement;
        
        open_bracket_token := peek_token(lexer);
        if open_bracket_token.type == .OPEN_BRACKET {
            get_token(lexer);
            debug_print("SUBCONDITION"); debug_print_push_indent();
            
            // NOTE: the subcondition is considered to be inside the block's scope for the purposes of identifier resolution
            //       this will give the subcondition access to identifiers that are declared in the main condition
            lexer.script.current_scope = if_statement.then_block;  
            defer lexer.script.current_scope = if_statement;
            
            if_statement.subcondition = lexer.script.my_parse_expression(lexer, 0);
            if if_statement.subcondition == null  return null;
            
            close_bracket_token := get_token(lexer);
            if close_bracket_token.type != .CLOSE_BRACKET {
                set_parse_error(lexer.script, "Expected a closing bracket for if statement subcondition.", loc = close_bracket_token.location);
                return null;
            }
        }
        
        debug_print("CONDITION"); debug_print_push_indent();
        if_statement.condition = parse_statement(lexer, false);
        if if_statement.condition == null  return null;
        
        // For now, we have to restrict use of these statement-level constructs in the condition explicitly.
        if if_statement.condition.node_type == {
          case Node_If_Statement;
            set_parse_error(lexer.script, "Another if statement cannot be used in an if statement condition.", loc = if_statement.condition.location);
            return null;
          case Node_While_Loop;
            set_parse_error(lexer.script, "A while loop cannot be used in an if statement condition.", loc = if_statement.condition.location);
            return null;
          case Node_For_Loop;
            set_parse_error(lexer.script, "A for loop cannot be used in an if statement condition.", loc = if_statement.condition.location);
            return null;
        }
        
        token = peek_token(lexer);
        if token.type == {
          case .THEN;
            get_token(lexer);
            #through;
            
          case;
            debug_print("THEN BLOCK"); debug_print_push_indent();
            if !parse_block_or_statement(lexer, if_statement.then_block, false)  return null; 
        }
        
        token = peek_token(lexer);
        if token.type == .ELSE {
            get_token(lexer);
            debug_print("ELSE BLOCK"); debug_print_push_indent();
            
            if if_statement.else_block == null {
                if_statement.else_block = alloc_node(lexer.script, Node_Block);
                if_statement.else_block.scope = if_statement;
            }
            
            if !parse_block_or_statement(lexer, if_statement.else_block, false)  return null; 
        }
        
        return if_statement;
        
      case .WHILE;
        debug_print("WHILE LOOP"); debug_print_push_indent();
        while_token := get_token(lexer);
        while_loop := alloc_node(lexer.script, Node_While_Loop, loc = while_token.location, trivia = while_token.trivia);
        
        lexer.script.current_scope = while_loop;  
        defer lexer.script.current_scope = while_loop.scope;
        
        while_loop.block = alloc_node(lexer.script, Node_Block);
        while_loop.block.scope = while_loop;
        
        debug_print("CONDITION");
        while_loop.condition = lexer.script.my_parse_expression(lexer, 0);
        if while_loop.condition == null  return null;
        
        debug_print("STATEMENT");
        if !parse_block_or_statement(lexer, while_loop.block, expect_semicolon)  return null;
        while_loop.block.scope = while_loop;
            
        return while_loop;
        
      case .FOR;
        debug_print("FOR LOOP"); debug_print_push_indent();
        for_token := get_token(lexer);
        for_loop := alloc_node(lexer.script, Node_For_Loop, loc = for_token.location, trivia = for_token.trivia);
        
        lexer.script.current_scope = for_loop;  
        defer lexer.script.current_scope = for_loop.scope;
        
        debug_print("CONTROL EXPRESSION");
        
        // NOTE: A preceding comma indicates that we are defining list of elements to iterate, but we may only have one element. 
        //       This is sort of a hack, but it's useful for now since I don't have array literals in the language yet.
        preceding_comma := expect_token_type(lexer, .COMMA);
        exprs := parse_comma_separated_expressions(lexer,, temp);
        if !exprs  return null;
        
        if !preceding_comma {
            if expect_token_type(lexer, .COLON) {
                if exprs.count == {
                  case 2;
                    identifier := exprs[1].(*Node_Identifier);
                    if identifier.node_type != Node_Identifier {
                        set_parse_error(lexer.script, "Expected an identifier for declaration of `it`.", node = identifier);
                        return null;
                    }
                    for_loop.it_index_decl = alloc_node(lexer.script, Node_Declaration, loc = identifier.location);
                    for_loop.it_index_decl.left = identifier;
                    #through;
                    
                  case 1;
                    identifier := exprs[0].(*Node_Identifier);
                    if identifier.node_type != Node_Identifier {
                        set_parse_error(lexer.script, "Expected an identifier for declaration of `it_index`.", node = identifier);
                        return null;
                    }
                    for_loop.it_decl = alloc_node(lexer.script, Node_Declaration, loc = identifier.location);
                    for_loop.it_decl.left = identifier;
                    
                  case;
                    set_parse_error(lexer.script, "Unexpected expressions in iterator declaration slot.", node = exprs[0]);
                    return null;
                }
                
                // parse expressions again for right side of colon
                preceding_comma = expect_token_type(lexer, .COMMA);
                exprs = parse_comma_separated_expressions(lexer,, temp);
                if !exprs  return null;
            }
        }
        
        if !preceding_comma && exprs.count == 1 {
            if expect_token_type(lexer, .SPREAD) {
                debug_print("CONTROL TYPE IS RANGE");
                for_loop.control_type = .RANGE;
                for_loop.range.lower  = exprs[0];
                for_loop.range.upper  = lexer.script.my_parse_expression(lexer, 0);
                if !for_loop.range.upper  return null;
            } else {
                debug_print("CONTROL TYPE IS ARRAY");
                for_loop.control_type = .ARRAY;
                for_loop.array_expression = exprs[0];
            }
        } else {
            debug_print("CONTROL TYPE IS LIST");
            for_loop.list = array_copy(exprs,, get_allocator(lexer.script));
            for_loop.control_type = .LIST;
        }
        
        for_loop.block = alloc_node(lexer.script, Node_Block);
        for_loop.block.scope = for_loop;
        
        debug_print("STATEMENT");
        if !parse_block_or_statement(lexer, for_loop.block, expect_semicolon)  return null;
        
        return for_loop;
    }
    
    debug_print("EXPRESSION");
    
    expression := lexer.script.my_parse_expression(lexer, 0);
    if expression == null  return null;
    
    if expect_token_type(lexer, .COLON) {
        debug_print("DECLARATION"); debug_print_push_indent();
        
        declaration := alloc_node(lexer.script, Node_Declaration, loc = expression.location);
        declaration.left = expression.(*Node_Identifier);
        
        next_token := peek_token(lexer);
        if !is_operator(next_token, "=") && next_token.type != .COLON {
            debug_print("WITH EXPLICIT TYPE");
            declaration.type_expression = lexer.script.my_parse_expression(lexer, 999); // @Hack: This prevents us from parsing the `=` expected after the type expression.
            if !declaration.type_expression  return null;
            if declaration.type_expression.node_type != Node_Identifier {
                set_parse_error(lexer.script, "Type expression of a declaration must be an identifier. (was %)", declaration.type_expression.node_type, loc = expression.location);
                return null;
            }
        }
        
        do_assignment := false;
        is_macro_declaration := false;
        
        next_token = peek_token(lexer);
        if is_operator(next_token, "=") {
            get_token(lexer);
            do_assignment = true;
        }
        else if next_token.type == .COLON {
            get_token(lexer);
            do_assignment = true;
            is_macro_declaration = true;
        }
        
        if do_assignment {
            debug_print("WITH ASSIGNMENT");
            declaration.init_expression = lexer.script.my_parse_expression(lexer, 0);
            if declaration.init_expression == null  return null;
            if is_macro_declaration then declaration.flags |= .STATIC;
        }
        
        expression = declaration;
    }
    
    if !check_for_semicolon(lexer, expression, expect_semicolon) {
        token = peek_token(lexer);
        set_parse_error(lexer.script, "Expected a semicolon after statement (%), but saw % token: '%'", expression.node_type, token.type, token.text, loc = lexer.location);
        return null;
    }
    
    return expression;
}

check_for_semicolon :: (lexer: *Lexer, expression: *Node, expect_semicolon := true) -> bool {
    // Check for semicolon
    token := peek_token(lexer);
    
    if token.type == .SEMICOLON { get_token(lexer); return true; }
    
    // We generally require semicolons to delimit statements, 
    // but since blocks are now treated as expressions, 
    // we make a special exception for blocks.
    if expression.node_type == {
      // case Node_If_Statement;
      //   _if := expression.(*Node_If_Statement);
      //   expect_semicolon &&= (ifx _if.else_block else _if.then_block).flags & .IMPLICIT != 0;
        
      case Node_Block;
        expect_semicolon = false;
    }
    
    // If we don't expect to see one then we are ok whether we saw one or not
    return !expect_semicolon;
}


parse_leaf :: (lexer: *Lexer) -> *Node {
    debug_print_push_indent();
    token := get_token(lexer);
    
    is_operator, operator_index := get_prefix_operator(lexer.script, token);
    if is_operator {
        debug_print("PREFIX OPERATOR (%)", token.text);
        precedence := get_operator_info(lexer.script, operator_index).precedence;
        left := lexer.script.my_parse_expression(lexer, precedence);
        if left == null  return null;
        return make_operator(lexer.script, token, operator_index, left, null);
    }
    
    if token.type == {
      case .IFX; 
        debug_print("IF (EXPRESSION)"); debug_print_push_indent();
        if_token := token;
        if_expression := alloc_node(lexer.script, Node_If_Expression, loc = if_token.location, trivia = if_token.trivia);
        
        debug_print("CONDITION"); debug_print_push_indent();
        if_expression.condition = parse_expression(lexer, 0);
        if if_expression.condition == null  return null;
        
        token = peek_token(lexer);
        if token.type == {
          case .ELSE; // no op
            
          case .THEN;
            get_token(lexer);
            #through;
            
          case;
            debug_print("THEN EXPRESSION"); debug_print_push_indent();
            if_expression.then_expression = lexer.script.my_parse_expression(lexer, 0);
            if if_expression.then_expression == null  return null;
        }
        
        token = peek_token(lexer);
        if token.type == .ELSE {
            get_token(lexer);
            debug_print("ELSE EXPRESSION"); debug_print_push_indent();
            if_expression.else_expression = lexer.script.my_parse_expression(lexer, 0);
            if if_expression.else_expression == null  return null;
        }
        
        return if_expression;
        
      case .DOT;
        debug_print("UNARY DOT");
        token_after_dot := get_token(lexer);
        if token_after_dot.type == .OPEN_BRACE {
            struct_contents, ok := parse_contents_of_struct_literal(lexer);
            if !ok  return null;
            literal := make_struct_literal(lexer.script, token_after_dot, struct_contents);
            return make_dot(lexer.script, token, null, literal);
        }
        else if token_after_dot.type == .IDENTIFIER {
            // For now we will make a dot node since there's already logic there for everything we gotta do to resolve the identifier
            // an alternative method here would be to flag the identifier as implicitly namespaced or something
            identifier := make_identifier(lexer.script, token_after_dot);
            return make_dot(lexer.script, token, null, identifier);
        }
        else set_parse_error(lexer.script, "Unexpected % token '%' after unary dot.", token_after_dot.type, token_after_dot.text, loc = token_after_dot.location);
        
      case .OPEN_BRACE;
        debug_print("BLOCK"); debug_print_push_indent();
        open_brace := token;
        block := alloc_node(lexer.script, Node_Block, loc = open_brace.location, trivia = open_brace.trivia);
        lexer.script.current_scope = block; 
        
        statements, ok := parse_statements(lexer, .CLOSE_BRACE);
        if !ok  return null;
        block.statements = array_copy(statements,, get_allocator(lexer.script));
        
        lexer.script.current_scope = block.scope; 
        return block;
        
      case .OPEN_PAREN;
        open_paren_location := token.location;
        
        expression := lexer.script.my_parse_expression(lexer, 0);
        if expression == null  return null;
        expression.flags |= .PARENTHESIZED;
        
        close_paren_token := get_token(lexer);
        if close_paren_token.type != .CLOSE_PAREN {
            set_parse_error(lexer.script, "Expected a closing paren for open paren at %.", open_paren_location, loc = close_paren_token.location);
            return null;
        }
        return expression;
        
      case .TRUE;
        debug_print("TRUE");
        literal := alloc_node(lexer.script, Node_Literal, loc = token.location, trivia = token.trivia);
        literal.literal_type = .BOOLEAN;
        literal.number       = to_any_number(true);
        return literal;
        
      case .FALSE;
        debug_print("FALSE");
        literal := alloc_node(lexer.script, Node_Literal, loc = token.location, trivia = token.trivia);
        literal.literal_type = .BOOLEAN;
        literal.number       = to_any_number(false);
        return literal;
        
      case .NUMBER;
        debug_print("NUMBER (%)", token.text);
        return make_number_literal(lexer.script, token);
        
      case .STRING;
        debug_print("STRING (\"%\")", token.text);
        return make_string_literal(lexer.script, token);
        
      case .DIRECTIVE;
        debug_print("DIRECTIVE"); debug_print_push_indent();
        directive := alloc_node(lexer.script, Node_Directive, loc = token.location, trivia = token.trivia);
        directive.name = token.text;
        
        directive.directive_index = -1;
        for lexer.script.directives {
            if it.name == directive.name {
                directive.directive_index = it_index;
            }
        }
        if directive.directive_index == -1 {
            set_parse_error(lexer.script, "Unable to resolve directive '%'\n", directive.name, loc = directive.location);
            return null;
        }
        
        if expect_token_type(lexer, .OPEN_PAREN) {
            if !expect_token_type(lexer, .CLOSE_PAREN) {
                expressions := parse_comma_separated_expressions(lexer,, temp);
                if !expressions  return null;
                directive.arguments = array_copy(expressions,, get_allocator(lexer.script));
                
                close_paren_token := get_token(lexer);
                if close_paren_token.type != .CLOSE_PAREN {
                    set_parse_error(lexer.script, "Expected a closing paren after arguments of directive. Instead we saw %\n", close_paren_token.text, loc = close_paren_token.location);
                    return null;
                }
            }
        }
        
        if !evaluate_directive(lexer.script, directive, .PARSE) {
            set_parse_error(lexer.script, "Failed while trying to execute a directive during parsing.", loc = token.location);
            return null;
        }
        return directive;
        
      case .IDENTIFIER;
        if peek_token(lexer, 0).type == .COLON 
        && peek_token(lexer, 1).type == .OPEN_BRACE {
            name_token  := token;
            colon_token := get_token(lexer);
            
            debug_print("NAMED BLOCK (%)", name_token.text); debug_print_push_indent();
            
            block := lexer.script.my_parse_leaf(lexer).(*Node_Block);
            if block == null  return null;
            assert(block.node_type == Node_Block);
            
            block.name     = name_token.text;
            block.location = name_token.location;
            block.trivia   = name_token.trivia;
            
            return block;
        }
        
        debug_print("IDENTIFIER (%)", token.text);
        return make_identifier(lexer.script, token);
    }
    
    set_parse_error(lexer.script, "Unexpected token encountered in parse_leaf: '%'", token.text, loc = token.location);
    return null;
}

// will parse a single string as a statement, checking that we end on EOF token
// this is useful for use cases where we are just using the script as a context for evaluating 
parse_statement_string :: (script: *Script, statement: string, expect_eof := true) -> *Node {
    lexer: Lexer;
    init_lexer(*lexer, script, statement);
    node := parse_statement(*lexer, expect_semicolon = false);
    
    if expect_eof && !expect_token_type(*lexer, .EOF) {
        token := get_token(*lexer);
        set_parse_error(script, "Unexpected % token '%'.", token.type, token.text, loc = token.location);
        return null;
    }
    return node;
}

// will parse a single string as an expression, checking that we end on EOF token
// this is useful for use cases where we are just using the script as a context for evaluating 
parse_expression_string :: (script: *Script, expr: string, expect_eof := true) -> *Node {
    lexer: Lexer;
    init_lexer(*lexer, script, expr);
    expression := script.my_parse_expression(*lexer, 0);
    
    if expect_eof && !expect_token_type(*lexer, .EOF)  return null;
    return expression;
}

parse_expression :: (lexer: *Lexer, min_prec: int) -> *Node {
    left := lexer.script.my_parse_leaf(lexer);
    if left == null  return null;
    
    // going iteratively, precedence can only decrease
    while true {
        // but going only recursively, precedence will only increase
        node := lexer.script.my_parse_binary(lexer, left, min_prec);
        if node == null  return null;
        if node == left  break;
        left = node;
    }
    
    return left;
}

parse_comma_separated_expressions :: (lexer: *Lexer) -> [] *Node {
    expressions: [..] *Node;
    while true {
        expr := lexer.script.my_parse_expression(lexer, 0);
        if expr == null {
            array_reset(*expressions);
            return .[];
        }
        array_add(*expressions, expr);
        if !expect_token_type(lexer, .COMMA)  break;
    }
    return expressions;
}

// If we hit a binary operator but its precedence is too low, we return left back to caller.
parse_binary :: (lexer: *Lexer, left: *Node, min_prec: int) -> *Node {
    token := peek_token(lexer);
    
    // In addition to normal binary operators, we have a few binary-operator-like constructs that have essentially infinite operator precedence.
    // These are open parenthesis for procedure calls, dot for struct member access and cast, and open bracket for array indexing.
    if token.type == {
      case .OPEN_PAREN;
        get_token(lexer);
        debug_print("PROCEDURE ARGUMENTS LIST");
        arg_exprs, ok := parse_procedure_arguments(lexer);
        if !ok  return null;
        return make_procedure_call(lexer.script, token, left, arg_exprs);
        
        
      case .DOT;
        dot_token := get_token(lexer);
        debug_print("BINARY DOT");
        
        token_after_dot := get_token(lexer);
        if token_after_dot.type == {
          case .OPEN_PAREN;
            debug_print("CAST");
            type_expr := lexer.script.my_parse_expression(lexer, 0);
            close_paren := get_token(lexer);
            if close_paren.type != .CLOSE_PAREN {
                set_parse_error(lexer.script, "Unexpected token '%' in cast type expression. Expected a closing paren.", close_paren.type, loc = close_paren.location);
                return null;
            }
            return make_cast(lexer.script, dot_token, left, type_expr);
            
          case .IDENTIFIER;
            identifier := make_identifier(lexer.script, token_after_dot);
            return make_dot(lexer.script, dot_token, left, identifier);
            
          case .OPEN_BRACE;
            struct_contents, ok := parse_contents_of_struct_literal(lexer);
            if !ok {
                set_parse_error(lexer.script, "Failed to parse contents of struct literal.", loc = token.location);
                return null;
            }
            literal := make_struct_literal(lexer.script, token_after_dot, struct_contents);
            return make_dot(lexer.script, dot_token, left, literal);
        }
        
        set_parse_error(lexer.script, "Unexpected % token '%' after dot.", token.type, token.text, loc = token.location);
        return null;
        
        
      case .ARROW;
        arrow_token := get_token(lexer);
        
        token_after_arrow := get_token(lexer);
        if token_after_arrow.type == {
          case .IDENTIFIER;
            identifier := make_identifier(lexer.script, token_after_arrow);
            return make_arrow(lexer.script, arrow_token, left, identifier);
        }
        
        set_parse_error(lexer.script, "Unexpected % token '%' after arrow.", token.type, token.text, loc = token.location);
        return null;
        
        
      case .OPEN_BRACKET;
        open_bracket_token := get_token(lexer);
        debug_print("ARRAY SUBSCRIPT");
        
        indexing_expr := lexer.script.my_parse_expression(lexer, 0);
        if indexing_expr == null  return null;
        if !expect_token_type(lexer, .CLOSE_BRACKET)  return null;
        
        return make_subscript(lexer.script, open_bracket_token, left, indexing_expr);
    }
    
    // We only consume the binary operator here if it is of a higher precedence than the previous binary operator.
    // If we hit a binary operator but its precedence is too low, we return left back to caller.
    is_operator, operator_index := get_binary_operator(lexer.script, token);
    if is_operator {
        _operator := get_operator_info(lexer.script, operator_index);
        if _operator.precedence <= min_prec  return left;
        
        debug_print("BINARY OPERATOR: %", _operator.token);
        
        operator_token := get_token(lexer);
        right := lexer.script.my_parse_expression(lexer, _operator.precedence);
        if right == null  return null;
        
        return make_operator(lexer.script, operator_token, operator_index, left, right);
    }
    
    return left;
}

// TODO: eventually, we will just want to call parse_block and then we can check if the contents of the block are valid for a struct literal during typechecking
parse_contents_of_struct_literal :: (lexer: *Lexer, scope_end_token := Token_Type.CLOSE_BRACE) -> ([] *Node, bool) {
    if expect_token_type(lexer, scope_end_token)  return .[], true;
    
    expressions := parse_comma_separated_expressions(lexer,, temp);
    if !expressions  return .[], false;
    
    expression_is_binary_assignment :: (node: *Node) -> bool {
        return node.node_type == Node_Binary_Operator && node.(*Node_Binary_Operator).token == "=";
    }
    
    using_assignment_syntax := expression_is_binary_assignment(expressions[0]);
    if using_assignment_syntax {
        for expressions {
            if !expression_is_binary_assignment(it) {
                set_parse_error(lexer.script, "Expected an assignment expression. Explicit member assignments cannot be mixed with simple comma-separated expressions in a struct literal.", loc = it.location);
                return .[], false;
            }
        }
    } else {
        // TODO: More thorough checks could be done here to make sure that the expressions in the struct literal are not binary assignments with +=, -=, etc.
        for expressions {
            if expression_is_binary_assignment(it) {
                set_parse_error(lexer.script, "Unexpected assignment expression. Explicit member assignments cannot be mixed with simple comma-separated expressions in a struct literal.", loc = it.location);
                return .[], false;
            }
        }
    }
    
    scope_end := get_token(lexer);
    if scope_end.type != scope_end_token {
        set_parse_error(lexer.script, "Expected a closing brace after initialization expressions of struct literal. Instead we saw % token '%'.", scope_end.type, scope_end.text, loc = scope_end.location);
        return .[], false;
    }
    
    return array_copy(expressions,, get_allocator(lexer.script)), true;
}

parse_procedure_arguments :: (lexer: *Lexer, scope_end_token := Token_Type.CLOSE_PAREN) -> ([] *Node, bool) {
    if expect_token_type(lexer, scope_end_token)  return .[], true;
    
    expressions := parse_comma_separated_expressions(lexer,, temp);
    if !expressions  return .[], false;
    
    scope_end := get_token(lexer);
    if scope_end.type != scope_end_token {
        set_parse_error(lexer.script, "Expected a closing paren after arguments of procedure call. Instead we saw % token '%'.", scope_end.type, scope_end.text, loc = scope_end.location);
        return .[], false;
    }
    
    return array_copy(expressions,, get_allocator(lexer.script)), true;
}
