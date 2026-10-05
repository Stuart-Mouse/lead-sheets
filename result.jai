
// ===== Error Struct =====

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
    location:   Source_Code_Location;   // NOTE: This is the Jai location where the error struct itself was created, not the location in the script source.
}

Lexer_Error :: struct {
    using 
    base:       Error_Base;
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
        if append_script_location {
            print(builder, "\n%:%,%: ", error.lexer.location.fully_pathed_filename, error.lexer.location.line_number, error.lexer.location.character_number);
        }
        
        // if error.type == {
        //   case .UNEXPECTED_TOKEN;
        //     print(builder, "Unexpected token: % \"%\"", token.type, token.text);
            
        //   case .UNEXPECTED_CHARACTER;
        //     print(builder, "Unexpected character: % \"%\"", token.type, token.text);
        // }
        
      case .PARSE_ERROR;
        node := error.parse.node;
        if node {
            if append_script_location {
                print(builder, "\n%:%,%: ", node.location.fully_pathed_filename, node.location.line_number, node.location.character_number);
            }
        } else {
            // print(builder, "[node is null! error type is %] ", error.type);
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
                print(builder, "Unresolved identifier: \"%\"", node.(*Node_Identifier).name);
                
              case .MISSING_TYPE_HINT;
                print(builder, "Unable to resolve type due to missing type hint.");
                
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
                print(builder, "Ran out of stack space. Try increasing stack size when initializing script.", node.(*Node_Identifier).name);
            }
        } else {
            print(builder, "[node is null! error type is %] ", error.type);
        }
        
      case; #through; // temporary, until other cases are implemented
      case .GENERAL_ERROR;
    }
    
    message := error.message;
    if message {
        append(builder, "\n");
        append(builder, message);
    }
}

format_error :: (script: *Script, append_jai_location := true, append_script_location := true) -> string {
    builder: String_Builder; 
    format_error(*builder, script, append_jai_location, append_script_location);
    return builder_to_string(*builder); 
}

log_error :: (script: *Script, append_jai_location := true, append_script_location := true) {
    error_string := format_error(script, append_jai_location, append_script_location,, temp);
    log("%", error_string);
}


// set_error :: (script: *Script, type: Error_Type, format: string, args: ..Any, loc := #caller_location) {
//     script.error = .{ type = type, location = loc };
// } @PrintLike

set_general_error :: (script: *Script, format: string, args: ..Any, loc := #caller_location) {
    script.error.base = .{ type = .GENERAL_ERROR, location = loc };
    set_error_message(script, sprint(format, ..args));
} @PrintLike

set_lexer_error :: (script: *Script, format: string = "", args: ..Any, node: *Node = null, type := Error_Type.LEXER_ERROR, loc := #caller_location) {
    assert(type & .CLASS_MASK == .LEXER_ERROR);
    script.error.base = .{ type = .LEXER_ERROR, location = loc };
    set_error_message(script, sprint(format, ..args));
} @PrintLike

set_parse_error :: (script: *Script, format: string = "", args: ..Any, node: *Node = null, type := Error_Type.PARSE_ERROR, loc := #caller_location) {
    assert(type & .CLASS_MASK == .PARSE_ERROR);
    script.error.base = .{ type = .PARSE_ERROR, location = loc };
    script.error.parse.node = node;
    set_error_message(script, sprint(format, ..args));
} @PrintLike

set_typecheck_error :: (script: *Script, format: string = "", args: ..Any, node: *Node, type := Error_Type.TYPECHECK_ERROR, loc := #caller_location) {
    assert(type & .CLASS_MASK == .TYPECHECK_ERROR);
    script.error.base = .{ type = type, location = loc };
    script.error.typecheck.node = node;
    set_error_message(script, sprint(format, ..args));
} @PrintLike

set_execution_error :: (script: *Script, format: string = "", args: ..Any, node: *Node, type := Error_Type.EXECUTION_ERROR, loc := #caller_location) {
    assert(type & .CLASS_MASK == .EXECUTION_ERROR);
    script.error.base = .{ type = type, location = loc };
    script.error.execution.node = node;
    set_error_message(script, sprint(format, ..args));
} @PrintLike

set_error_message :: (script: *Script, message: string) {
    if script.error.message then free(script.error.message);
    script.error.message = message;
}
