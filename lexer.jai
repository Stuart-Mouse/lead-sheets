
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
    
    
    TODO: consider adding user-level lexing overrides, maybe using negative values for Token_Type to signify user tokens
          then user could add new operators by creating user-level tokens and adding those to the operator table
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
    
    IDENTIFIER      :: 256;
    DIRECTIVE;
    
    // TODO: Add mechanism to add keywords in a similar way to directives.
    //       We could also use keywords to replace special literals like true and false.
    // KEYWORD;
    
    TRUE;
    FALSE;
    
    STRING;
    NUMBER;
    
    OPERATOR;
    
    ARROW;      // ->
    SPREAD;     // .. 
    
    IF;
    IFX;
    WHILE;
    FOR;
    THEN;
    ELSE;
    BREAK;
}

Token :: struct {
    type:       Token_Type;
    text:       string;
    trivia:     string;
    location:   Source_Location_Info;
}

Lexer :: struct {
    using #as scanner: Scanner;
    
    // using script.interface_procedures; // causes crash: compiler bug?
    using,only(
        parse_expression,
        parse_binary,
        parse_leaf,
    ) script: *Script;
    
    source_file_id: Source_File_ID;
    
    lex_next_token := default_lex_next_token;
    
    TOKEN_BUFFER_COUNT :: 4;
    token_buffer: [TOKEN_BUFFER_COUNT] Token; // ring buffer, indexed modulo TOKEN_BUFFER_COUNT
    next_token_index: int;
}

init_lexer :: (lexer: *Lexer, script: *Script, file := "", location := Source_Code_Location.{ "", 1, 1 }, source_file_id: Source_File_ID = 0) {
    lexer.* = .{};
    lexer.script = script;
    lexer.source_file_id = source_file_id;
    
    init_scanner(*lexer.scanner, file);
    lexer.location = location;
    
    // lex first few tokens into token buffer
    for 0..lexer.TOKEN_BUFFER_COUNT-1 {
        lexer.token_buffer[it] = lexer.lex_next_token(lexer);
        if lexer.token_buffer[it].type == .ERROR
        || lexer.token_buffer[it].type == .EOF {
            break;
        }
    }
}

// consumes a token
get_token :: (lexer: *Lexer) -> Token {
    return_token := lexer.token_buffer[lexer.next_token_index];
    if return_token.type == .ERROR 
    || return_token.type == .EOF {
        return return_token;
    }
    
    lexer.token_buffer[lexer.next_token_index] = lexer.lex_next_token(lexer);
    lexer.next_token_index = (lexer.next_token_index + 1) % lexer.TOKEN_BUFFER_COUNT;
    
    return return_token;
}

// peeks next_token, but does not consume it
peek_token :: (lexer: *Lexer, peek_index := 0) -> Token {
    assert(peek_index >= 0 && peek_index < lexer.TOKEN_BUFFER_COUNT-1);
    index := (lexer.next_token_index + peek_index) % lexer.TOKEN_BUFFER_COUNT;
    return lexer.token_buffer[index];
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

default_lex_next_token :: (lexer: *Lexer) -> Token {
    trivia := skip_whitespace_and_comments(lexer);
    if has_error(lexer.script)  return .{ type = .ERROR };
    if is_eof(lexer)            return .{ type = .EOF, location = get_source_location_info(lexer), trivia = trivia };
    
    token_location := Source_Location_Info.{
        file_id = lexer.source_file_id,
        l0      = xx lexer.line_number,
        l1      = xx lexer.line_number,
        c0      = xx lexer.character_number,
        c1      = xx lexer.character_number,
    };
    
    make_token :: (type: Token_Type, count: int) -> Token #expand {
        token_location.c1 = xx (token_location.c0 + count);
        token := Token.{
            type       = type,
            text       = slice(remaining_as_string(lexer), 0, count),
            location   = token_location,
            trivia     = trivia, 
        };
        advance(lexer, count);
        return token;
    }
    
    // 2-character tokens (excluding operators)
    if remaining_as_string(lexer).count >= 2 {
        if slice(remaining_as_string(lexer), 0, 2) == {
          case ".."; return make_token(.SPREAD, 2);
          case "->"; return make_token(.ARROW, 2);
        }
    }
    
    // single-character tokens (excluding operators)
    if is_any(lexer.cursor.*, "()[]{},.:;?") {
        return make_token(xx lexer.cursor.*, 1);
    }
    
    // NOTE: this is being done before operators, presumably because a number may start with a minus sign
    //       this is probably fine, but be aware of this while doing operator refactoring
    token_text := default_try_lex_number(lexer);
    if token_text {
        token_location.c1 = xx (token_location.c0 + token_text.count - 1);
        return .{ .NUMBER, token_text, trivia, token_location };
    }
    
    // operator tokens
    {
        best_match, best_match_len := -1;
        for lexer.script.operator_table {
            if it.token.count > best_match_len 
            && begins_with(lexer, it.token) {
                best_match = it_index;
                best_match_len = it.token.count;
            }
        }
        if best_match >= 0 {
            _operator := *lexer.script.operator_table[best_match];
            return make_token(.OPERATOR, _operator.token.count);
        }
    }
    
    token_text = try_lex_identifier(lexer);
    if token_text {
        type := Token_Type.IDENTIFIER;
        if token_text == {
          case "true";      type = .TRUE;
          case "false";     type = .FALSE;
          case "if";        type = .IF;
          case "ifx";       type = .IFX;
          case "for";       type = .FOR;
          case "while";     type = .WHILE;
          case "then";      type = .THEN;
          case "else";      type = .ELSE;
          case "break";     type = .BREAK;
        }
        token_location.c1 = xx (token_location.c0 + token_text.count - 1);
        return .{ type, token_text, trivia, token_location };
    }
    
    // parse a directive
    if get_next_byte(lexer) == #char "#" {
        advance(lexer);
        token_text = try_lex_identifier(lexer);
        if token_text { 
            token_location.c1 = xx (token_location.c0 + token_text.count - 1);
            return .{ .DIRECTIVE, token_text, trivia, token_location };
        }
        set_lexer_error(lexer.script, "Unexpected character encountered while attempting to parse a directive name.", type = .UNEXPECTED_CHARACTER, loc = get_source_location_info(lexer));
        return .{ type = .ERROR };
    }
    
    // parse string or backticked identifier
    if get_next_byte(lexer) == #char "\"" || get_next_byte(lexer) == #char "`" { 
        quote_char := get_next_byte(lexer);
        
        if !advance(lexer) {
            set_lexer_error(lexer.script, "Unexpected EOF encountered while parsing string literal.", type = .UNEXPECTED_EOF, loc = get_source_location_info(lexer));
            return .{ type = .ERROR };
        }
        token_text := string.{ 0, lexer.cursor };
        
        while get_next_byte(lexer) != quote_char {
            if get_next_byte(lexer) == #char "\\" {
                _, ok := parse_escape_sequence(lexer);
                if !ok {
                    set_lexer_error(lexer.script, "Invalid escape sequence encountered while parsing string literal.", type = .UNEXPECTED_CHARACTER, loc = get_source_location_info(lexer));
                    return .{ type = .ERROR };
                }
            } else {
                if !advance(lexer) {
                    set_lexer_error(lexer.script, "Unexpected EOF encountered while parsing string literal.", type = .UNEXPECTED_EOF, loc = get_source_location_info(lexer));
                    return .{ type = .ERROR };
                }
            }
        }
        
        token_text.count = lexer.cursor - token_text.data;
        advance(lexer);
        is_identifier := quote_char == #char "`";
        
        token_location.c1 = xx (token_location.c0 + token_text.count - 1);
        return .{ ifx is_identifier then .IDENTIFIER else .STRING, token_text, trivia, token_location };
    }
    
    set_lexer_error(lexer.script, "Unexpected character encountered: '%'", string.{ 1, lexer.cursor }, type = .UNEXPECTED_CHARACTER, loc = get_source_location_info(lexer));
    return .{ type = .ERROR };
}

begins_identifier :: (char: u8) -> bool { return is_alpha(char) || char == #char "_"; }
continues_identifier :: is_alnum;

// returns true if we even start to parse an identifier, not only on success
try_lex_identifier :: (using lexer: *Lexer) -> string {
    if begins_identifier(get_next_byte(lexer)) {
        str := string.{ 1, lexer.cursor };
        advance(lexer);
        
        while continues_identifier(get_next_byte(lexer)) {
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

// cycles between skipping whitespace and comments until next character is neither
skip_whitespace_and_comments :: (using lexer: *Lexer) -> trivia: string {
    if is_eof(lexer)  return "";
    
    trivia := string.{ 0, lexer.cursor };
    
    while loop := !is_eof(lexer) {
        while is_whitespace(get_next_byte(lexer)) {
            if !advance(lexer)  break;
        }
        if begins_with(lexer, "//") {
            advance(lexer, 2);
            while get_next_byte(lexer) != #char "\n" {
                if !advance(lexer)  break loop;
            }
            advance(lexer);
        }
        else if begins_with(lexer, "/*") {
            advance(lexer, 2);
            while !begins_with(lexer, "*/") {
                if !advance(lexer) {
                    set_lexer_error(lexer.script, "Unexpected EOF in multiline comment.", type = .UNEXPECTED_EOF, loc = get_source_location_info(lexer));
                    return "";
                }
            }
            advance(lexer, 2);
        }
        else break;
    }
    
    trivia.count = lexer.cursor - trivia.data;
    return trivia;
}

is_whitespace :: inline (char: u8) -> bool {
  return char == #char " "
      || char == #char "\t"
      || char == #char "\r"
      || char == #char "\n";
}





Source_Location_Info :: struct {
    file_id:            Source_File_ID;
    using text_range:   Source_Text_Range;
}

// Indicates index + 1, so that 0 acts as a null value.
Source_File_ID :: #type,isa u32;

// NOTE: Source_Text_Range is inclusive of both bounds.
Source_Text_Range :: struct { l0, l1, c0, c1: u32; };

// Assuming the source range is all on one line, which is often the case.
make_source_text_range :: (line_number: int, character_number: int, count: int) -> Source_Text_Range {
    return .{ xx line_number, xx line_number, xx character_number, xx (character_number + min(count - 1, 0)) };
}

get_source_location_info :: (lexer: Lexer) -> Source_Location_Info {
    return .{
        file_id     = lexer.source_file_id, 
        text_range  = .{ xx lexer.line_number, xx lexer.line_number, xx lexer.character_number, xx lexer.character_number },
    };
}

print_source_range_with_underline :: (builder: *String_Builder, source_text: string, source_range: Source_Text_Range) {
    text := source_text;
    
    if source_range.l0 != source_range.l1 then return;
    
    line_number := 1;
    while text && line_number < source_range.l0 {
        if text[0] == "\n" {
            line_number += 1;
            if line_number == source_range.l0 {
                advance(*text);
                break;
            }
        }
        advance(*text);
    }
    
    line_begin := text.data;
    line_end   := line_begin;
    while text {
        if text[0] == "\n" {
            line_end = text.data;
            break;
        }
        advance(*text);
    }
    if line_end == line_begin then line_end = text.data;
    
    line_string := string.{ line_end - line_begin, line_begin };
    append(builder, line_string);
    append(builder, "\n");
    
    for 1..source_range.c0-1 {
        append(builder, " ");
    }
    for source_range.c0..source_range.c1-1 {
        append(builder, "^");
    }
    append(builder, "\n");
}
