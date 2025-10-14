
/*
    Notes about error handling:
    
        currently, we aren't using result objects because of the stack frame bloat issue...
        we may reevaluate this in the future if it provides us with considerably better or_return semantics, but it seems like we can do just as well with a single error object that gets set in the script itself.
        
        only typechecking really makes much use of checking the script.error for flow control, 
            and TBH we could probably go back to simply checking if the returned value type is null for all internal calls to typecheck_node, only checking script.error when we really need to. 
                this would probably be just a bit faster too.
            but in either case, we will want to check the script.error for typechecking callbacks, so that they may return null when they simply don't want to override typechecking for a given node
        
        there aren't really many error cases during execution, and we simply use a bool return value on execute_node for flow control. it is then just assumed that the error should always be set before returning false
            again, we may make typechecking behave the same later on
        
        evaluation incomplete
        parsing incomplete
        lexing incomplete, really need to figure out better solution for propogating up lexer errors
        
        for all cases, we still need to further improve the actual error recording and reporting


*/

// Result :: struct(T: Type) {
//     error: Error;
//     value: T;
// }

// operator ! :: inline (result: Result) -> bool { return result.error.type != .NO_ERROR; }

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
    EXECUTION_ERROR :: 6 << 32;
    USER_ERROR      :: 7 << 32;
    
    CLASS_MASK :: 0xffffffff_00000000;
    
    // error subtypes stored in lower 32 bits
    MISSING_TYPE_HINT           :: TYPECHECK_ERROR + 1;
    UNRESOLVED_IDENTIFIER       :: TYPECHECK_ERROR + 2;
    INVALID_CAST                :: TYPECHECK_ERROR + 3;
    FAILED_IMPLICIT_CAST        :: TYPECHECK_ERROR + 4;
    FAILED_OVERLOAD_RESOLUTION  :: TYPECHECK_ERROR + 5;
    
    UNEXPECTED_TOKEN            :: LEXER_ERROR + 1;
    UNEXPECTED_CHARACTER        :: LEXER_ERROR + 2;
    UNEXPECTED_EOF              :: LEXER_ERROR + 3;
    
    STACK_OVERFLOW              :: EXECUTION_ERROR + 1;
}

Error :: union {
    using
    base:       Error_Base;
    parse:      Parse_Error;
    lexer:      Lexer_Error;
    typecheck:  Typecheck_Error;
    execution:  Execution_Error;
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
    procedure_overload_report:    Procedure_Overload_Report;
}

Execution_Error :: struct {
    using 
    base:   Error_Base;
    node:   *Node;
    
    // TODO: maybe take a stack trace?
}

get_class :: inline (error: Error) -> Error_Type { return error.type & .CLASS_MASK; }

format_error :: (builder: *String_Builder, script: *Script, append_jai_location := true, append_script_location := true) {
    if !has_error(script)  return;
    error := *script.error;
    
    if append_jai_location {
        print(builder, "%:%,%: ", error.location.fully_pathed_filename, error.location.line_number, error.location.character_number);
    }
    
    if get_class(error) == {
      case .LEXER_ERROR;
        token := error.lexer.token;
        if append_script_location {
            print(builder, "\n%:%,%: ", token.location.fully_pathed_filename, token.location.line_number, token.location.character_number);
        }
        
        if error.type == {
          case .UNEXPECTED_TOKEN;
            print(builder, "Unexpected token: % %", token.text, token.trivia);
            
          case .UNEXPECTED_CHARACTER;
            print(builder, "Unexpected character: % %", token.text, token.trivia);
        }
        
      case .PARSE_ERROR;
        node := error.parse.node;
        if node {
            if append_script_location {
                print(builder, "\n%:%,%: ", node.location.fully_pathed_filename, node.location.line_number, node.location.character_number);
            }
        } else {
            print(builder, "[node is null! error type is %] ", error.type);
        }
        
      case .TYPECHECK_ERROR;
        node := error.typecheck.node;
        if node {
            if append_script_location {
                print(builder, "\n%:%,%: ", node.location.fully_pathed_filename, node.location.line_number, node.location.character_number);
            }
            if error.type == {
              case .UNRESOLVED_IDENTIFIER;
                assert(node.node_type == Node_Identifier);
                print(builder, "Unresolved identifier: '%'\n", node.(*Node_Identifier).name);
                
              case .MISSING_TYPE_HINT;
                print(builder, "Unable to resolve type due to missing type hint.\n");
                
              case .FAILED_IMPLICIT_CAST;
                // NOTE: currently the full error is just in error.message
                
              case .FAILED_OVERLOAD_RESOLUTION;
                format_procedure_overload_report(builder, script, error.typecheck.procedure_overload_report);
            }
        } else {
            print(builder, "[node is null! error type is %] ", error.type);
        }
        
        
      case .EXECUTION_ERROR;
        node := error.execution.node;
        if node {
            if append_script_location {
                print(builder, "\n%:%,%: ", node.location.fully_pathed_filename, node.location.line_number, node.location.character_number);
            }
            if error.type == {
              case .STACK_OVERFLOW;
                assert(node.node_type == Node_Identifier);
                print(builder, "Ran out of stack space. Try increasing stack size when initializing script.\n", node.(*Node_Identifier).name);
            }
        } else {
            print(builder, "[node is null! error type is %] ", error.type);
        }
        
      case; #through; // temporary, until other cases are implemented
      case .GENERAL_ERROR;
    }
    
    append(builder, error.message);
}

format_error :: (script: *Script, append_jai_location := true, append_script_location := true) -> string {
    builder: String_Builder; 
    format_error(*builder, script, append_jai_location, append_script_location);
    return builder_to_string(*builder); 
}


// set_error :: (script: *Script, type: Error_Type, format: string, args: ..Any, loc := #caller_location) {
//     script.error = .{ type = type, message = tprint(format, ..args), location = loc };
// } @PrintLike

set_general_error :: (script: *Script, format: string, args: ..Any, loc := #caller_location) {
    script.error = .{ type = .GENERAL_ERROR, message = tprint(format, ..args), location = loc };
} @PrintLike

set_parse_error :: (script: *Script, format: string = "", args: ..Any, node: *Node = null, loc := #caller_location) {
    // assert(type & .CLASS_MASK == .PARSE_ERROR);
    script.error = .{ type = .PARSE_ERROR, message = tprint(format, ..args), location = loc };
    script.error.parse.node = node;
} @PrintLike

set_typecheck_error :: (script: *Script, format: string = "", args: ..Any, node: *Node, type := Error_Type.TYPECHECK_ERROR, loc := #caller_location) {
    assert(type & .CLASS_MASK == .TYPECHECK_ERROR);
    script.error = .{ type = type, message = tprint(format, ..args), location = loc };
    script.error.typecheck.node = node;
} @PrintLike

set_execution_error :: (script: *Script, format: string = "", args: ..Any, node: *Node, type := Error_Type.EXECUTION_ERROR, loc := #caller_location) {
    assert(type & .CLASS_MASK == .EXECUTION_ERROR);
    script.error = .{ type = type, message = tprint(format, ..args), location = loc };
    script.error.execution.node = node;
} @PrintLike


