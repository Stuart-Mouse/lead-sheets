
Script_Token_Type :: enum {
    INVALID;
    EOF;
    
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

// get_next_token :: (using ctxt: *Script_Constructor) -> (Script_Token, bool) {
//     if token_index >= tokens.count 
//         return .{}, false;
    
//     ret := tokens[token_index];
//     token_index += 1;
//     return ret, true;
// }

// peek_next_token :: (using ctxt: *Script_Constructor) -> (Script_Token, bool) {
//     if token_index >= tokens.count 
//         return .{}, false;
    
//     ret := tokens[token_index];
//     return ret, true;
// }


Tokenizer :: struct {
    file       : string;
    next_token : Script_Token;
}

consume_token :: (using t: *Tokenizer) -> bool {
    if next_token.type == .EOF  return true;
    ok: bool;
    next_token, ok = lex_next_token(t);
    return ok;
}

get_token :: (using t: *Tokenizer) -> (Script_Token, bool) {
    current_token := next_token;
    return current_token, consume_token(t);
}

peek_token :: (using t: *Tokenizer) -> Script_Token {
    return next_token;
}

lex_next_token :: (using t: *Tokenizer) -> (Script_Token, bool) {
    if !skip_whitespace_and_comments(*file) || !file
        return .{ type = .EOF }, true;
    
    // single-character tokens
    if is_any(file[0], "+-*/(),;=") {
        ret := Script_Token.{
            type = cast(Script_Token_Type) file[0], // character value for single-char tokens can be cast directly to token type
            text = slice(file, 0, 1),
        };
        advance(*file);
        return ret, true;
    }
    
    // parse an identifier
    if is_alpha(file[0]) || file[0] == #char "_" {
        str: string = .{ 1, *file[0] };
        if !advance(*file)  return .{ type = .INVALID }, false;
        
        while file && is_alnum(file[0]) {
            str.count += 1;
            if !advance(*file)  return .{ type = .INVALID }, false;
        }
        
        // TODO: Check if the identifier is a reserved word
        
        return .{
            type = .IDENTIFIER,
            text = str,
        }, true;
    }
    
    // parse a number
    if is_digit(file[0]) {
        str: string = .{ 1, *file[0] };
        if !advance(*file)  return .{ type = .INVALID }, false;
        
        while file && (is_digit(file[0]) || file[0] == #char ".") {
            str.count += 1;
            if !advance(*file)  return .{ type = .INVALID }, false;
        }
        
        val, ok, rem := string_to_float(str);
        return .{
            type   = .NUMBER,
            number = val,
        }, true;
    }
    
    print("Unhandled token: '%'\n", file);
    assert(false);
    return .{ type = .INVALID }, false;
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
    return tokens, true;
}

