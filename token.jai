
/*
    The Lexer implementation here is extremely basic.
    On the one hand, it's proabbly good to keep it simple since the scripts themselves are very simple,
        but on the other I wonder if I shouldn't try to beef it up a little and learn from the Jai Lexer.
    Oh well, maybe I'll do that one day if there's a good reason to do so.
    
    We don't really need a lot of context here, so you can basically only look at one token at a time.
    There's no buffer of previous tokens to refer to. 
    The Lexer always looks one token ahead of the current token that is returned to the user.
    
    get_token() will return 'next_token', and will lex another token to replace 'next_token'.
    peek_token() will return 'next_token', but will not consume the token or lex another token.
    
    The general usage pattern is just to peek_token() to look ahead, and then to commit consumption of the token use get_token() after we know we want to pick up the token.
    I really only use get_token() on its own when I know for sure that the next token must be of a certain type.
    
    There is also expect_token_type(), which is useful when one doesn't care about anything other than the token type,
        for example when expecting a closing `)` or `]` token.
    
*/

Script_Token_Type :: enum {
    EOF           :: 0;
    ERROR         :: 1;
    
    OPEN_PAREN    :: #char "(";
    CLOSE_PAREN   :: #char ")";
    
    OPEN_BRACE    :: #char "{";
    CLOSE_BRACE   :: #char "}";
    
    OPEN_BRACKET  :: #char "[";
    CLOSE_BRACKET :: #char "]";
    
    DOT           :: #char ".";
    COMMA         :: #char ",";    
    SEMICOLON     :: #char ";";
    ASSIGN_EQUAL  :: #char "=";
    
    LESS_THAN     :: #char "<";
    GREATER_THAN  :: #char ">";
    
    
    ADD           :: #char "+";
    SUB           :: #char "-";
    MUL           :: #char "*";
    DIV           :: #char "/";
    
    IDENTIFIER    :: 256;
    NUMBER;
    COMPARE_EQUAL;
    LESS_THAN_OR_EQUAL_TO;
    GREATER_THAN_OR_EQUAL_TO;
    
    LOGICAL_AND;
    LOGICAL_OR;
    
    SPREAD;
}

Script_Token :: struct {
    type: Script_Token_Type;
    union {
        text:   string;
        number: float32;
    };
    
    src_loc: Source_Location;
}

Source_Location :: struct {
    line, char: int;
}

// we should just always store token text probably? would simplify printing token, and negligible memory usage extra
print_token :: (token: Script_Token) {
    print("%: ", token.type);
    
    if token.type == {
      case;
        print("%\n", token.text);
      case .NUMBER;
        print("%\n", token.number);
    }
}

sprint_token :: (token: Script_Token) -> string {
    sb: String_Builder;
    init_string_builder(*sb,, allocator = temp);

    print_to_builder(*sb, "%: ", token.type);

    if token.type == {
      case;
        print_to_builder(*sb, "%", token.text);
      case .NUMBER;
        print_to_builder(*sb, "%", token.number);
    }
    
    return builder_to_string(*sb);
}

// NOTE: need to call consume_token once to init lexer for proper usage
//       otherwise, first token will appear to be .EOF

Lexer :: struct {
    file            : string;
    next_token      : Script_Token;
    cursor_location : Source_Location;
}

init_lexer :: (using t: *Lexer) {
    cursor_location = .{ line = 1, char = 1 };
    get_token(t);
}

// convenience macro 
get_token_or_return :: (using t: *Lexer, code: Code) -> Script_Token #expand {
    token := get_token(t);
    if token.type == .ERROR  `return #insert code;
    return token;
}

// consumes a token
get_token :: inline (using t: *Lexer) -> Script_Token {
    if next_token.type == .ERROR {
        dbg_print("LEXER ERROR: % (line %, char %)\n", next_token.text, next_token.src_loc.line, next_token.src_loc.char);
        return next_token;
    }
    
    current_token := next_token;
    next_token = lex_next_token(t);
    return current_token;
}

// peeks next_token, but does not consume it
peek_token :: inline (using t: *Lexer) -> Script_Token {
    return next_token;
}

// consumes token if it was expected type, else it was just peeked
expect_token_type :: (using t: *Lexer, type: Script_Token_Type) -> bool {
    token := peek_token(t);
    if token.type == type {
        get_token(t);
        return true;
    }
    return false;
}

/*
    The logic for 1- and 2-char tokens could probably be more efficient, but at least it's relatively clear.
*/
lex_next_token :: (using lexer: *Lexer) -> Script_Token {
    if !skip_whitespace_and_comments(lexer) {
        return .{ type = .EOF };
    }
    
    src_loc := cursor_location;
    make_error_token :: (error_string: string = "") -> Script_Token #expand { 
        return .{ type = .ERROR, src_loc = src_loc, text = error_string }; 
    }
    
    // convenience macro for short tokens
    make_token :: (type: Script_Token_Type, len: int) -> Script_Token #expand {
        defer advance(lexer, len);
        return .{
            type    = type,
            text    = slice(file, 0, len),
            src_loc = src_loc
        };
    }
    
    // 2-char logical operators
    if file.count >= 2 {
        if slice(file, 0, 2) == {
          case "&&"; return make_token(.LOGICAL_AND, 2);
          case "||"; return make_token(.LOGICAL_OR, 2);
          case ".."; return make_token(.SPREAD, 2); // not a logical operator but whatever, it goes here
        }
    }
    
    // 2-char comparison operators
    if file.count >= 2 && file[1] == #char "=" {
        if file[0] == {
          case #char "="; return make_token(.COMPARE_EQUAL, 2);
          case #char "<"; return make_token(.LESS_THAN_OR_EQUAL_TO, 2);
          case #char ">"; return make_token(.GREATER_THAN_OR_EQUAL_TO, 2);
        }
    }
    
    // single-character tokens
    if is_any(file[0], "+-*/(),;=.[]{}<>") {
        return make_token(xx file[0], 1);
    }
    
    // parse an identifier
    if is_alpha(file[0]) || file[0] == #char "_" {
        str: string = .{ 1, *file[0] };
        if !advance(lexer)  return make_error_token("Unexpected EOF while parsing identifier.");
        
        while file && is_alnum(file[0]) {
            str.count += 1;
            if !advance(lexer)  return make_error_token("Unexpected EOF while parsing identifier.");
        }
        
        // TODO: Check if the identifier is a reserved word
        
        return .{
            type    = .IDENTIFIER,
            text    = str,
            src_loc = src_loc
        };
    }
    
    // parse a number
    if is_digit(file[0]) {
        str: string = .{ 1, *file[0] };
        if !advance(lexer)  return make_error_token("Unexpected EOF while parsing number.");
        
        after_dot := false;
        while file {
            // we can only allow one decimal point in a number, and it cannot be followed by a second decimal point
            if !is_digit(file[0]) {
                if file[0] != #char "." || after_dot  break;
                if file.count > 1 && file[1] == #char "."  break; // cannot have a double dot, that is a spread/range operator
                after_dot = true;
            }
            
            str.count += 1;
            if !advance(lexer)  return make_error_token("Unexpected EOF while parsing number.");
        }
        
        val, ok, rem := string_to_float(str);
        return .{
            type    = .NUMBER,
            number  = val,
            src_loc = src_loc
        };
    }
    
    return make_error_token("Unexpected character encountered.");
}

/* 
    For now, we are not pre-tokenizing the file, since it is really simple to just get the tokens lazily.
    If tokenization later becomes more complicated due to needing more lookahead or something, 
        I may consider going back to pre-tokenizing the file before constructing the AST.
*/
tokenize :: (text: string) -> ([..] Script_Token, bool) {
    lexer := Lexer.{ file = text };
    success := false;
    tokens: [..] Script_Token;
    
    defer if !success {
        array_free(tokens);
        memset(*tokens, 0, size_of(type_of(tokens)));
    }
    
    while lexer.file {
        token, ok := lex_next_token(*lexer);
        print_token(token);
        if !ok  return tokens, false;
        if token.type != .EOF {
            array_add(*tokens, token);
        } else {
            break;
        }
    }
    
    success = true;
    return tokens;
}


// cycles between skipping whitespace and comments until next character is neither
skip_whitespace_and_comments :: (using lexer: *Lexer) -> bool {
    if file.count == 0  return false;
    while true {
        while is_whitespace(file[0]) {
            if !advance(lexer)  return false;
        }
        if file[0] == #char "#" {
            while file[0] != #char "\n" {
                if !advance(lexer)  return false;
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


#scope_module

advance :: inline (using t: *Lexer, amount := 1) -> bool {
    _amount := min(amount, file.count);
    
    for 0.._amount-1 {
        if file[it] == #char "\n" {
            cursor_location.line += 1;
            cursor_location.char  = 1;
        } else {
            cursor_location.char += 1;
        }
    }
    
    file.data  += _amount;
    file.count -= _amount;
    
    return file.count > 0; // return false when we hit EOF
}
