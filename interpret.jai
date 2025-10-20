/*
    interpret.jai
    
    `interpret_node` will function as a more dynamic way of evaluating AST nodes, interleaving typechecking and evaluation.
    This is basically what the evaluation stuff was originally supposed to do before I gave up on that idea.
    
    Part of the challenge of implementing this stuff id going to just be reducing the duplication of code from typechecking and evaluation as much as possible.
    But maybe the process will help in figuring out where those places are that we could actually de-duplicate code.
    
    
    What is the contract between caller and interpret_node?
    Caller can provide a hint type, which interpret_node should try to match and then caller can decide whether what is actually returned is acceptable or not.
        This is most applicable in the case of identifiers and literals.
        
    
    Do we actually want a generalized interpret_node procedure that is recursive?
    Ideally, we can just use a small subset of fully dynamic operations and leave the rest of our evaluation to use pre-typechecked nodes.
    The most fundamental case to handle is operators.
    Then probably procedure calls?
        This will be very tricky to do overload resolution for.
    Casts can be used as coercions/type assertions on Any's to get us back into statically-typed land.
    Otherwise anything that typechecks as an Any get us into dynamic interpreting mode.
    In general, we really just want to get from Any into a more concrete type as quickly as possible.
    
    
    Sort of an aside, but I should probably document that I do not want any interpreting procedures to actually modify the AST nodes in any way
    
    Random: I have no idea how we are going to handle lvalue stuff with this yet
*/

// Interpreter_Flags :: enum_flags {
//     NONE :: 0;
// }

// interpret_node ::  (
//     script:             *Script, 
//     node:               *Node, 
//     flags:              Interpreter_Flags,
//     hint_type:          *Type_Info,
//     // provided_storage:   *void,
// ) -> Any {
//     return_result :: (node: *Node, any: Any) -> bool #expand {
//         // if provided_storage {
//         //     dprint("Returning result using provided storage @ %: ", provided_storage, newline = false);
//         //     if !is_aggregate(any.type)  dprint("%", any, newline = false);
//         //     dprint(" (%)", as_type(any.type));
            
//         //     memcpy(provided_storage, any.value_pointer, any.type.runtime_size);
//         //     `return Any.{ node.value_type, provided_storage };
//         // }
        
//         // TODO: maybe we should try to handle any coercion to the expected type here in this macro so that all things that return will at least attempt to coerce to teh hint type
//         //       then if caller wants to do anythign special, that's their own prerogative...
        
//         ok, result := push_any(*script.stack, any, should_push_by_pointer(node));
//         if !ok  set_execution_error(script, "Ran out of space on stack!", node = null);
        
//         dprint("Returning result using stack @ %: ", result.value_pointer, newline = false);
//         if !is_aggregate(any.type)  dprint("%", any);
//         dprint(" (%)", as_type(any.type));
//         `return result;
//     }
    
//     get_result_storage :: (type: *Type_Info) -> Any #expand {
//         // if provided_storage {
//         //     dprint("Got provided storage @ % for result of type: %", provided_storage, as_type(node.value_type));
//         //     return provided_storage;
//         // }
        
//         result := push_any(*script.stack, type, false);
//         if !result {
//             set_execution_error(script, "Ran out of space on stack!", node = null);
//             `return NULL_ANY;
//         }
//         dprint("Got space on stack @ % for result of type: %", result, as_type(type));
//         return result;
//     }
    
//     if node.node_type == {
//       // case Node_Directive;
//       //   no idea what to do for directives here yet...
        
//       case Node_Literal;
//         assert(!is_malleable(literal));
        
//         if #complete literal.literal_type == {
//           case .ANY;
//             if hint_type && hint_type != literal.any.type {
//                 result := get_result_storage(hint_type);
//                 if Convert.any_to_any(result, literal.any) 
//                     then return result;
//                     else pop_any(*script.stack, result.type, should_push_by_pointer(node));
//             }
//             return_result(node, literal.any);
            
//           case .STRING;
//             return_result(literal, literal.text);
            
//           case .BOOLEAN; #through;
//           case .NUMBER;
//             result := literal.number;
//             if is_numeric_type(hint_type) {
//                 result = Any_Number.zero_of_type(hint_type);
//                 Convert.any_to_any(to_any(*new_number), to_any(*literal.number));
//             } else if literal.number.type == xx float64 {
//                 result = Any_Number.zero_of_type(float32);
//                 Convert.any_to_any(to_any(*new_number), to_any(*literal.number));
//             }
//             return_result(literal, to_any(*literal.number));
            
//           case .STRUCT;
//             struct_info := hint_type.(*Type_Info_Struct);
//             if struct_info == null {
//                 set_typecheck_error(script, 
//                     "Unable to typecheck struct literal without a type hint.", 
//                     type = .MISSING_TYPE_HINT, 
//                     node = literal
//                 );
//                 return;
//             }
//             if struct_info.type != .STRUCT {
//                 set_typecheck_error(script, "Type hint provided to struct literal is not a struct type. (was '%')", as_type(ti_struct), node = literal);
//                 return;
//             }
//             if literal.aggr.expressions.count >= struct_info.members.count {
//                 set_typecheck_error(script, "Too many values provided in struct literal.", node = literal);
//                 return;
//             }
            
//             result := get_result_storage(hint_type);
//             for literal.aggr.expressions {
//                 member := struct_info.members[it_index];
//                 member_result := interpret_node(script, it, .NONE, member.type);
//                 if member_result.type != member.type {
//                     set_execution_error(script, "interpret_node returned unexpected type for struct member value.", node = literal);
//                     return NULL_ANY;
//                 }
//                 memcpy(result.value_pointer + member.offset_in_bytes, member_result.value_pointer, member.type.runtime_size);
//             }
//             return result;
//         }
//         unreachable();
        
//       case Node_Identifier;
        
//       case Node_Operation;
        
//       case Node_Procedure_Call;
        
//       case Node_Cast;
        
//       case Node_Dot;
        
//       case Node_Subscript;
        
//       case Node_Declaration;
        
//       case Node_Block;
        
//       case Node_If_Statement;
        
//       case Node_While_Loop;
        
//       case Node_For_Loop;
        
//     }
// }

/*
    interpret_operation
    
    for numeric types, we will just find the builtin operation with the least cast distance and use that
    we will need more robust overload resolution, similar to what we do for procedure calls
    FWIW it will probably be easier to first make procedure calls work with implict conversions from Any
    then we can refactor builtin operator resolution to use the same principles
    (this would also allow us to remove all of the operators for the various integer types other than s64 and u64)
    
    
*/
interpret_operation :: (script: *Script, _operator: Operator, left: Any, right: Any) -> Any {
    if operation.directive {
        // typecheck_node_or_return(script, operation.directive, check_flags, hint_type);
        // if operation.directive.runtime_node  return;
        set_general_error(script, "Interpreting a directive operator is not currently supported. Operator is %", _operator);
        return NULL_ANY;
    }
    
    _operator := get_operator(script, operation);
    
    
    
}

