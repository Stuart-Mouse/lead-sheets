
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


Token_Type :: enum {
    EOF             :: 0;
    ERROR           :: 1;
    
    OPEN_PAREN      :: #char "(";
    CLOSE_PAREN     :: #char ")";
    
    OPEN_BRACE      :: #char "{";
    CLOSE_BRACE     :: #char "}";
    
    OPEN_BRACKET    :: #char "[";
    CLOSE_BRACKET   :: #char "]";
    
    QUESTION_MARK   :: #char "?";
    DOT             :: #char ".";
    COMMA           :: #char ",";    
    COLON           :: #char ":";
    SEMICOLON       :: #char ";";
    ASSIGN_EQUAL    :: #char "=";
    
    LESS_THAN       :: #char "<";
    GREATER_THAN    :: #char ">";
    
    ADD             :: #char "+";
    SUB             :: #char "-";
    MUL             :: #char "*";
    DIV             :: #char "/";
    
    
    IDENTIFIER      :: 256;
    STRING;
    NUMBER;
    COMPARE_EQUAL;
    LESS_THAN_OR_EQUAL_TO;
    GREATER_THAN_OR_EQUAL_TO;
    
    LOGICAL_AND;
    LOGICAL_OR;
    
    PLUS_EQUALS;
    MINUS_EQUALS;
    
    // DOUBLE_QUESTION_MARK;
    
    SPREAD;
    DIRECTIVE;
    
    IF;
    WHILE;
    FOR;
}


Token :: struct {
    type:               Token_Type;
    text:               string;
    source_location:    Source_Location;
    whitespace_before:  string;
    
    // using source_location;
}

Source_Location :: struct {
    line, char: int;
}

Lexer :: struct {
    file:               string;
    next_token:         Token;
    cursor_location:    Source_Location;
}

init_lexer :: (lexer: *Lexer, file := "", location := Source_Location.{ 1, 1 }) {
    // NOTE: need to call consume_token once to init lexer for proper usage
    //       otherwise, first token will appear to be .EOF
    lexer.file = file;
    lexer.cursor_location = location;
    get_token(lexer);
}

// convenience macro 
get_token_or_return :: (lexer: *Lexer, code: Code) -> Token #expand {
    token := get_token(lexer);
    if token.type == .ERROR  `return #insert code;
    return token;
}

// consumes a token
get_token :: inline (using lexer: *Lexer) -> Token {
    if next_token.type == .ERROR {
        dprint("LEXER ERROR: % (line %, char %)\n", 
            next_token.text, 
            next_token.source_location.line, 
            next_token.source_location.char
        );
        return next_token;
    }
    
    current_token := next_token;
    next_token = lex_next_token(lexer);
    return current_token;
}

// peeks next_token, but does not consume it
peek_token :: inline (using t: *Lexer) -> Token {
    return next_token;
}

// consumes token if it was expected type, else it was just peeked
expect_token_type :: (lexer: *Lexer, type: Token_Type) -> bool {
    token := peek_token(lexer);
    if token.type == type {
        get_token(lexer);
        return true;
    }
    return false;
}

// TODO: consider removing some of the 'unexpected EOF' cases, only leaving those that are strictly necessary
//       main candidates are in number and identifier cases
lex_next_token :: (using lexer: *Lexer) -> Token {
    whitespace, is_eof, ok := skip_whitespace_and_comments(lexer);
    if !ok {
        log("Error: unexpected EOF in the middle of a comment!");
        return .{ type = .ERROR };
    }
    if is_eof {
        return .{ type = .EOF };
    }
    
    // we grab cursor location at top so that it points to start of token
    source_location := cursor_location;
    
    // convenience macro for short tokens
    make_token :: (type: Token_Type, len: int) -> Token #expand {
        defer advance(lexer, len);
        return .{
            type              = type,
            text              = slice(file, 0, len),
            source_location   = source_location,
            whitespace_before = whitespace
        };
    }
    
    if file.count >= 2 {
        // 2-char comparison operators
        if file[1] == #char "=" {
            if file[0] == {
              case #char "="; return make_token(.COMPARE_EQUAL, 2);
              case #char "<"; return make_token(.LESS_THAN_OR_EQUAL_TO, 2);
              case #char ">"; return make_token(.GREATER_THAN_OR_EQUAL_TO, 2);
              
              case #char "+"; return make_token(.PLUS_EQUALS, 2);
              case #char "-"; return make_token(.MINUS_EQUALS, 2);
            }
        }
        
        // 2-char logical operators
        if slice(file, 0, 2) == {
          case "&&"; return make_token(.LOGICAL_AND, 2);
          case "||"; return make_token(.LOGICAL_OR, 2);
          case ".."; return make_token(.SPREAD, 2); // not a logical operator but whatever, it goes here
        }
    }
    
    
    // single-character tokens
    if is_any(file[0], "+-*/(),:;?=.[]{}<>") {
        return make_token(xx file[0], 1);
    }
    
    // parse an identifier
    {
        ident, was_parsed := try_parse_identifier(lexer);
        if was_parsed {
            // replace type if identifier is a reserved word
            if ident.text == {
              case "if";    ident.type = .IF;
              case "for";   ident.type = .FOR;
              case "while"; ident.type = .WHILE;
            }
            ident.whitespace_before = whitespace;
            return ident;
        }
    }
    
    // parse a directive
    {
        if file[0] == #char "#" {
            advance(lexer);
            ident, was_parsed := try_parse_identifier(lexer);
            if was_parsed {
                ident.type = .DIRECTIVE;
                ident.whitespace_before = whitespace;
                return ident;
            }
            return make_error_token(source_location, "Unexpected character encountered while attempting to parse a directive name.");
        }
    }
    
    // parse a number
    if is_digit(file[0]) {//|| file[0] == #char "-" {
        str: string = .{ 1, *file[0] };
        advance(lexer);
        
        after_dot := false;
        while file {
            // we can only allow one decimal point in a number, and it cannot be followed by a second decimal point
            if !is_digit(file[0]) {
                if file[0] != #char "." || after_dot  break;
                if file.count > 1 && file[1] == #char "."  break; // cannot have a double dot, that is a spread/range operator
                after_dot = true;
            }
            
            str.count += 1;
            advance(lexer);
        }
        
        val, ok, rem := string_to_float(str);
        return .{
            type              = .NUMBER,
            text              = str,
            source_location   = source_location,
            whitespace_before = whitespace
        };
    }
    
    // parse string or backticked identifier
    if file[0] == #char "\"" || file[0] == #char "`" { 
        quote_char := file[0];
        is_identifier := quote_char == #char "`";
        
        if !advance(lexer)  return make_error_token(source_location, "Unexpected EOF while parsing string.");
        string_value: string = ---;
        string_value.data  = file.data;
        string_value.count = 0;
        
        while file[0] != quote_char {
            // TODO: handle escape sequences more properly
            adv := 1 + (file[0] == #char "\\").(int); 
            string_value.count += adv;
            if !advance(lexer, adv)  
                return make_error_token(source_location, "Unexpected EOF while parsing string.");
        }
        advance(lexer);
        
        return .{ 
            type              = ifx is_identifier then .IDENTIFIER else .STRING, 
            text              = string_value, 
            source_location   = source_location,
            whitespace_before = whitespace
        };
    }
    
    return make_error_token(source_location, "Unexpected character encountered.");
}

make_error_token :: (source_location: Source_Location, error_string: string = "") -> Token { 
    return .{ type = .ERROR, source_location = source_location, text = error_string }; 
}

// returns true if we even start to parse an identifier, not only on success
try_parse_identifier :: (using lexer: *Lexer) -> (Token, bool) {
    source_location := cursor_location;
    if is_alpha(file[0]) || file[0] == #char "_" {
        str: string = .{ 1, *file[0] };
        advance(lexer);
        
        while file && is_alnum(file[0]) {
            str.count += 1;
            advance(lexer);
        }
        
        return .{
            type    = .IDENTIFIER,
            text    = str,
            source_location = source_location
        }, true;
    }
    return .{}, false;
}


// NOTE: this must be kept in accord with try_parse_identifier
is_legal_identifier :: (str: string) -> bool {
    if !str return false;
    if !is_alpha(str[0]) && str[0] != #char "_"  return false;
    for str  if !is_alnum(it)  return false;
    return true;
}


/* 
    For now, we are not pre-tokenizing the file, since it is really simple to just get the tokens lazily.
    If tokenization later becomes more complicated due to needing more lookahead or something, 
        I may consider going back to pre-tokenizing the file before constructing the AST.
*/
tokenize :: (text: string) -> ([..] Token, bool) {
    lexer := Lexer.{ file = text };
    success := false;
    tokens: [..] Token;
    
    defer if !success {
        array_free(tokens);
        memset(*tokens, 0, size_of(type_of(tokens)));
    }
    
    while lexer.file {
        token, ok := lex_next_token(*lexer);
        print(token.text);
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
skip_whitespace_and_comments :: (using lexer: *Lexer) -> (whitespace: string, is_eof: bool, ok: bool) {
    if file.count == 0  return "", true, true;
    
    whitespace := string.{ data = file.data };
    is_eof := false;
    while true {
        while is_whitespace(file[0]) {
            if !advance(lexer) {
                is_eof = true;
                break;
            }
        }
        if begins_with(file, "//") {
            while file[0] != #char "\n" {
                if !advance(lexer)  return "", true, false;
            }
        }
        else if begins_with(file, "/*") {
            while !begins_with(file, "*/") {
                if !advance(lexer)  return "", true, false;
            }
        }
        else break;
    }
    
    whitespace.count = file.data - whitespace.data;
    return whitespace, is_eof, true;
}

is_whitespace :: inline (char: u8) -> bool {
  return char == #char " "
      || char == #char "\t"
      || char == #char "\r"
      || char == #char "\n";
}


#scope_module

advance :: inline (using lexer: *Lexer, amount := 1) -> bool {
    _amount := min(amount, file.count);
    
    for 0.._amount-1 {
        if file[it] == #char "\n" {
            cursor_location.line += 1;
            cursor_location.char  = 0;
        }
        cursor_location.char += 1;
    }
    
    file.data  += _amount;
    file.count -= _amount;
    
    return file.count > 0; // return false when we hit EOF
}
