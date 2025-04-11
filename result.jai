

// Result :: struct(T: Type) {
//     error: Error;
//     // error: bool;
//     value: T;
// }

// operator ! :: inline (result: Result) -> bool { return result.error.type != .NO_ERROR; }
// // operator ! :: inline (result: Result) -> bool { return !result.error; }

// make_result :: inline (value: $T) -> Result(T) {
//     return .{ value = value };
// }

// // only for errors, since we have binary compatibility in that case
// recast_error :: (result: Result($T), $to_type: Type) -> Result(to_type) {
//     assert(result.error != .NO_ERROR);
//     return .{ error = result.error };
// }

// or_return :: (result: Result($T), $R: Type) -> T #expand {
//     if !result  `return recast_error(R, result); 
//     return result.value;
// }

// or_return :: (result: Result($T)) -> T #expand {
//     if !result  `return result;
//     return result.value;
// }

// or_default :: inline (result: Result($T), default: T) -> T {
//     if !result  return default;
//     return result.value;
// }

/*
    f76a8 - d66c8 = 20FE0
    c76e8 - afb98 = 17b50
    
*/


// ===== Error Struct =====

// instead of just returning all errors as bool + string, we are using an error enum and union
// one reason for this is that we want to be able to recover from certain types of errors that may occur, particularly during typechecking
// the other reason is that we can defer formatting an error into a string until we have completely unwound the stack to the point where the error is handled or reported
// this means we can attach more information as we unwind, which should give better context to some error messages
// another benefit is that in the future we could aggregate multiple errors before printing them all out

// s64 is definitely overkill but whatever
Error_Type :: enum s64 {
    NO_ERROR        :: 0;
    
    // broad classes of errors, stored in upper 32 bits
    // this is done so that we can | the subtype with broader type to determine if a subtype belongs to a certain class of error
    // some of these classes of errors correspond to Error union subtypes with additional information which is specific to that error
    GENERAL_ERROR   :: 1 << 32;
    LEXER_ERROR     :: 2 << 32;
    PARSE_ERROR     :: 3 << 32;
    TYPECHECK_ERROR :: 4 << 32;
    EVAL_ERROR      :: 5 << 32;
    EXEC_ERROR      :: 6 << 32;
    USER_ERROR      :: 7 << 32;
    
    CLASS_MASK :: 0xffffffff_00000000;
    
    // error subtypes stored in lower 32 bits
    MISSING_TYPE_HINT       :: TYPECHECK_ERROR + 1;
    UNRESOLVED_IDENTIFIER   :: TYPECHECK_ERROR + 2;
    
    UNEXPECTED_TOKEN        :: LEXER_ERROR + 1;
    UNEXPECTED_CHARACTER    :: LEXER_ERROR + 2;
    UNEXPECTED_EOF          :: LEXER_ERROR + 3;
}

Error :: union {
    using
    base:       Error_Base;
    parse:      Parse_Error;
    lexer:      Lexer_Error;
    typecheck:  Typecheck_Error;
}

Error_Base :: struct {
    type:       Error_Type;
    message:    string;
    location:   Source_Code_Location; // NOTE: This is the Jai location where the error struct itself was created, not the location in the script source.
}

Lexer_Error :: struct {
    using 
    base:   Error_Base;
    token:  Token;
}

Parse_Error :: struct {
    using 
    base:   Error_Base;
    node:   *Node;
}

Typecheck_Error :: struct {
    using 
    base:   Error_Base;
    node:   *Node;
}

operator ! :: inline (error: Error) -> bool { return error.type == .NO_ERROR; }

get_class :: inline (error: Error) -> Error_Type { return error.type & .CLASS_MASK; }

format_error :: (builder: *String_Builder, error: Error) {
    if !error  return;
    
    print_to_builder(builder, "%:%,%: ", error.location.fully_pathed_filename, error.location.line_number, error.location.character_number);
    
    if get_class(error) == {
      case .LEXER_ERROR;
        token := error.lexer.token;
        print_to_builder(builder, "\n%:%,%: ", token.location.fully_pathed_filename, token.location.line_number, token.location.character_number);
        
        if error.type == {
          case .UNEXPECTED_TOKEN;
            print_to_builder(builder, "Unexpected token: % %", token.text, token.trivia);
            
          case .UNEXPECTED_CHARACTER;
            print_to_builder(builder, "Unexpected character: % %", token.text, token.trivia);
        }
        
      case .TYPECHECK_ERROR;
        node := error.typecheck.node;
        print_to_builder(builder, "\n%:%,%: ", node.location.fully_pathed_filename, node.location.line_number, node.location.character_number);
        append(builder, error.message);
        
      case; #through; // temporary, until other cases are implemented
      case .GENERAL_ERROR;
        append(builder, error.message);
    }
}

format_error :: (error: Error) -> string {
    builder: String_Builder; 
    format_error(builder, error);
    return builder_to_string(*builder); 
}

