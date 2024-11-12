
Script_Token_Type :: enum {
    EOF         :: 0;
    ERROR       :: 1;
    
    OPEN_PAREN  :: #char "(";
    CLOSE_PAREN :: #char ")";
    
    COMMA       :: #char ",";
    SEMICOLON   :: #char ";";
    EQUALS      :: #char "=";
    
    ADD         :: #char "+";
    SUB         :: #char "-";
    MUL         :: #char "*";
    DIV         :: #char "/";
    
    IDENTIFIER  :: 256;
    NUMBER;
}

Script_Token :: struct {
    type : Script_Token_Type;
    union {
        text   : string;
        number : float32;
    };
    
    src_loc: Source_Location;
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

// NOTE: need to call consume_token once to init tokenizer for proper usage
// otherwise, first token will appear to be .EOF

Tokenizer :: struct {
    file       : string;
    next_token : Script_Token;
}

get_token_or_return :: (using t: *Tokenizer, code: Code) -> Script_Token #expand {
    token := get_token(t);
    if token.type == .ERROR  `return #insert code;
    return token;
}

get_token :: inline (using t: *Tokenizer) -> Script_Token {
    // do not try to lex any more after hitting error token
    if next_token.type == .ERROR  return next_token; 
    
    current_token := next_token;
    next_token = lex_next_token(t);
    return current_token;
}

peek_token :: inline (using t: *Tokenizer) -> Script_Token {
    return next_token;
}

// consumes token if it was expected type, else it was just peeked
expect_token_type :: (using t: *Tokenizer, type: Script_Token_Type) -> bool {
    token := peek_token(t);
    if token.type == type {
        get_token(t);
        return true;
    }
    return false;
}

lex_next_token :: (using t: *Tokenizer) -> Script_Token {
    if !skip_whitespace_and_comments(*file) || !file
        return .{ type = .EOF };
    
    // single-character tokens
    if is_any(file[0], "+-*/(),;=") {
        ret := Script_Token.{
            type = cast(Script_Token_Type) file[0], // character value for single-char tokens can be cast directly to token type
            text = slice(file, 0, 1),
        };
        advance(*file);
        return ret;
    }
    
    // parse an identifier
    if is_alpha(file[0]) || file[0] == #char "_" {
        str: string = .{ 1, *file[0] };
        if !advance(*file)  return .{ type = .ERROR };
        
        while file && is_alnum(file[0]) {
            str.count += 1;
            if !advance(*file)  return .{ type = .ERROR };
        }
        
        // TODO: Check if the identifier is a reserved word
        
        return .{
            type = .IDENTIFIER,
            text = str,
        };
    }
    
    // parse a number
    if is_digit(file[0]) {
        str: string = .{ 1, *file[0] };
        if !advance(*file)  return .{ type = .ERROR };
        
        while file && (is_digit(file[0]) || file[0] == #char ".") {
            str.count += 1;
            if !advance(*file)  return .{ type = .ERROR };
        }
        
        val, ok, rem := string_to_float(str);
        return .{
            type   = .NUMBER,
            number = val,
        };
    }
    
    return .{ type = .ERROR };
}

/* 
    For now, we are not pre-tokenizing the file, since it is really simple to just get the tokens lazily.
    If tokenization later becomes more complicated due to needing more lookahead or something, 
        I may consider going back to pre-tokenizing the file before constructing the AST.
*/
tokenize :: (text: string) -> ([..] Script_Token, bool) {
    tokenizer := Tokenizer.{ file = text };
    success := false;
    tokens: [..] Script_Token;
    
    defer if !success {
        array_free(tokens);
        memset(*tokens, 0, size_of(type_of(tokens)));
    }
    
    while tokenizer.file {
        token, ok := lex_next_token(*tokenizer);
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
// TODO: add *int param to increment line count 
skip_whitespace_and_comments :: (file: *string) -> bool {
    if file.count == 0 return false;
    while true {
        while is_whitespace(file.*[0]) {
            if !advance(file) return false;
        }
        if file.*[0] == #char "#" {
            while file.*[0] != #char "\n" {
                if !advance(file) return false;
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
