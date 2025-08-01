
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
    
    BINARY_OR       :: #char "|";
    BINARY_AND      :: #char "&";
    BINARY_NOT      :: #char "~";
    BINARY_XOR      :: #char "^";
    
    LESS_THAN       :: #char "<";
    GREATER_THAN    :: #char ">";
    
    ADD             :: #char "+";
    SUB             :: #char "-";
    MUL             :: #char "*";
    DIV             :: #char "/";
    
    DOLLAR          :: #char "$";
    
    
    
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
    FOREACH;
}


Token :: struct {
    type:       Token_Type;
    text:       string;
    location:   Source_Code_Location;
    trivia:     string;
    error_type: Error_Type; // TODO: maybe unionize with normal result?  Seems easier than making the lexer return some Result object...
}

/*
    Lexer can be used either in a sort of active mode where tokens are only lexed as requested, 
        or in a pre-lexed mode where all tokens are lexed up front.
    If input was pre-lexed, then Lexer will return sequential tokens from `tokens`,
        otherwise, it will use `token_buffer` as a ring buffer for the next few tokens.
    The interface is exactly the same either way, but we support both so that there's options 
        about how things get allocated, since you will probably only want to pre-lex an entire 
        input string if those tokens will be stored in the script's arena.
    It's sort of like how we have an option to either allocate nodes in the script's arena or 
        using the temporary allocator, depending on the use case.
*/
Lexer :: struct {
    using #as scanner: Scanner;
    
    lexer_proc := lex_next_token;
    
    TOKEN_BUFFER_COUNT :: 4;
    token_buffer: [TOKEN_BUFFER_COUNT] Token; // ring buffer, indexed modulo TOKEN_BUFFER_COUNT
    next_token_index: int;
}


init_lexer :: (lexer: *Lexer, file := "", location := Source_Code_Location.{ "", 1, 1 }) {
    lexer.* = .{};
    lexer.file = file;
    lexer.location = location;
    
    // lex first few tokens into token buffer
    for 0..lexer.TOKEN_BUFFER_COUNT-1 {
        lexer.token_buffer[it] = lex_next_token(lexer);
        if lexer.token_buffer[it].type == .ERROR
        || lexer.token_buffer[it].type == .EOF {
            break;
        }
    }
}

// consumes a token
get_token :: (using lexer: *Lexer) -> Token {
    return_token := token_buffer[next_token_index];
    if return_token.type == .ERROR 
    || return_token.type == .EOF {
        return return_token;
    }
    
    token_buffer[next_token_index] = lexer_proc(lexer);
    next_token_index = (next_token_index + 1) % TOKEN_BUFFER_COUNT;
    
    return return_token;
}

// peeks next_token, but does not consume it
peek_token :: (using t: *Lexer, peek_index := 0) -> Token {
    assert(peek_index >= 0 && peek_index < TOKEN_BUFFER_COUNT-1);
    index := (next_token_index + peek_index) % TOKEN_BUFFER_COUNT;
    return token_buffer[index];
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

lex_next_token :: (using lexer: *Lexer) -> Token {
    trivia, ok := skip_whitespace_and_comments(lexer);
    if !ok    return make_error_token(location, .UNEXPECTED_EOF, message = "in the middle of a comment");
    if !file  return .{ type = .EOF, location = location };
    
    // we grab cursor location at top so that it points to start of token
    token_location := lexer.location;
    
    // convenience macro for short tokens
    make_token :: (type: Token_Type, len: int) -> Token #expand {
        defer advance(lexer, len);
        return .{
            type       = type,
            text       = slice(file, 0, len),
            location   = token_location,
            trivia     = trivia, 
        };
    }
    
    token_text := try_lex_number(lexer);
    if token_text  return .{ .NUMBER, token_text, token_location, trivia, .NO_ERROR };
    
    if file.count >= 2 {
        if slice(file, 0, 2) == {
          case "=="; return make_token(.COMPARE_EQUAL, 2);
          case "<="; return make_token(.LESS_THAN_OR_EQUAL_TO, 2);
          case ">="; return make_token(.GREATER_THAN_OR_EQUAL_TO, 2);
          case "+="; return make_token(.PLUS_EQUALS, 2);
          case "-="; return make_token(.MINUS_EQUALS, 2);
          case "&&"; return make_token(.LOGICAL_AND, 2);
          case "||"; return make_token(.LOGICAL_OR, 2);
          case ".."; return make_token(.SPREAD, 2); // not a logical operator but whatever, it goes here
        }
    }
    
    // TODO: have some comptime function to generate this single char tokens string based on set values and list of operators?
    // single-character tokens
    if is_any(file[0], "+-*/(),:;?=.[]{}|&~^<>$") {
        return make_token(xx file[0], 1);
    }
    
    token_text = try_lex_identifier(lexer);
    if token_text {
        type := Token_Type.IDENTIFIER;
        // TODO: can abstract this to a lookup table if we want
        if token_text == {
          case "if";        type = .IF;
          case "for";       type = .FOR;
          case "foreach";   type = .FOREACH;
          case "while";     type = .WHILE;
        }
        return .{ type, token_text, token_location, trivia, .NO_ERROR };
    }
    
    // TODO: perhaps this should not be a token in itself, and should be handled on a higher level by the parser
    // parse a directive
    if file[0] == #char "#" {
        advance(lexer);
        token_text = try_lex_identifier(lexer);
        if token_text  return .{ .DIRECTIVE, token_text, token_location, trivia, .NO_ERROR };
        return make_error_token(token_location, .UNEXPECTED_CHARACTER, string.{1,file.data}, "while attempting to parse a directive name.");
    }
    
    // parse string or backticked identifier
    if file[0] == #char "\"" || file[0] == #char "`" { 
        quote_char := file[0];
        
        if !advance(lexer)  return make_error_token(token_location, .UNEXPECTED_EOF, message = "while parsing string.");
        token_text := string.{ 0, file.data };
        
        while file[0] != quote_char {
            if file[0] == #char "\\" {
                _, ok := parse_escape_sequence(lexer);
                if !ok  return make_error_token(token_location, .UNEXPECTED_CHARACTER, "invalid escape sequence encountered while parsing string");
            } else {
                if !advance(lexer)  return make_error_token(token_location, .UNEXPECTED_EOF, message = "while parsing string.");
            }
        }
        
        token_text.count = lexer.file.data - token_text.data;
        advance(lexer);
        is_identifier := quote_char == #char "`";
        return .{ ifx is_identifier then .IDENTIFIER else .STRING, token_text, token_location, trivia, .NO_ERROR };
    }
    
    return make_error_token(token_location, .UNEXPECTED_CHARACTER, string.{1,file.data});
}

// NOTE: we use the trivia string to attach some additional context to error tokens
make_error_token :: (location: Source_Code_Location, error_type: Error_Type, text := "", message := "") -> Token { 
    assert(error_type & .CLASS_MASK == .LEXER_ERROR);
    return .{ type = .ERROR, location = location, error_type = error_type, text = text, trivia = message }; 
}

begins_identifier :: (char: u8) -> bool { return is_alpha(char) || char == #char "_"; }
continues_identifier :: is_alnum;

// returns true if we even start to parse an identifier, not only on success
try_lex_identifier :: (using lexer: *Lexer) -> string {
    if begins_identifier(file[0]) {
        str: string = .{ 1, *file[0] };
        advance(lexer);
        
        while file && continues_identifier(file[0]) {
            str.count += 1;
            advance(lexer);
        }
        return str;
    }
    return "";
}

is_legal_identifier :: (str: string) -> bool {
    if !str return false;
    if !begins_identifier(str[0])  return false;
    for str  if !continues_identifier(it)  return false;
    return true;
}

tokenize :: (file: string) -> ([..] Token, bool) {
    lexer: Lexer;
    init_lexer(*lexer, file);
    
    success := true;
    tokens: [..] Token;
    
    defer if !success {
        array_free(tokens);
        memset(*tokens, 0, size_of(type_of(tokens)));
    }
    
    while lexer.file {
        token := get_token(lexer);
        array_add(*tokens, token);
        if token.type == {
            case .EOF;    break;
            case .ERROR;  success = false; break;
        }
    }
    
    return tokens, success;
}

// cycles between skipping whitespace and comments until next character is neither
skip_whitespace_and_comments :: (using lexer: *Lexer) -> (trivia: string, ok: bool) {
    if file.count == 0  return "", true;
    
    trivia := string.{ 0, file.data };
    ok := true;
    
    while loop := true {
        while is_whitespace(file[0]) {
            if !advance(lexer)  break;
        }
        if begins_with(file, "//") {
            advance(lexer, 2);
            while file[0] != #char "\n" {
                if !advance(lexer)  break loop;
            }
            advance(lexer);
        }
        else if begins_with(file, "/*") {
            advance(lexer, 2);
            while !begins_with(file, "*/") {
                if !advance(lexer) {
                    ok = false;
                    break loop; 
                }
            }
            advance(lexer, 2);
        }
        else break;
    }
    
    trivia.count = file.data - trivia.data;
    return trivia, ok;
}

is_whitespace :: inline (char: u8) -> bool {
  return char == #char " "
      || char == #char "\t"
      || char == #char "\r"
      || char == #char "\n";
}
