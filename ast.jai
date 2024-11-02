
AST :: struct {
    Node_Type :: enum {
        OPERATION;
        NUMBER;
        VARIABLE;
        PROCEDURE_CALL;
    }
    Node :: struct {
        value_type : Type;
        type       : Node_Type;
        src_loc    : Source_Location;
        
        union {
            variable : External_Variable;
            number   : float32;
            ident    : string; // view onto source string
            procedure_call : struct {
                using #as procedure: Script_Procedure;
                params: [..] int; // TODO: should make this not be a dynamic array later if possible. for now, its just convenient
            };
            operation: struct {
                using #as procedure: Script_Procedure;
                left, right: int;
            };
        }
    }
    
    Expression :: struct {
        flags : enum { TYPECHECKED; };
        root  : int;
    }
    
    Statement_Type :: enum {
        ASSIGNMENT;
    }
    Statement :: struct {
        type : Statement_Type;
        union {
            assignment : struct {
                left  : string; // TODO: later we will want to support some kind of expression on left side (primarily for arrays)
                right : Expression;
            };
        }
    }
    
    // for now all nodes are stored in one big array
    // later maybe we put nodes in separate arrays for each statement
    nodes      : [..] Node;
    statements : [..] Statement;
}

free_ast :: (ast: *AST) {
    array_free(ast.nodes);
    memset(*ast.nodes, 0, size_of(Resizable_Array));
}

get_expression_type :: (expr: *AST.Expression) -> Type { 
    ret: Type; // 0-valued Type
    if !expr.flags & .TYPECHECKED     return ret;
    if expr.root >= expr.nodes.count  return ret;
    return expr.nodes[expr.root].value_type;
}

print_ast :: (using ctxt: *Script, node_i: int, parent_prec: int = 0) {
    if node_i < 0 || node_i >= ast.nodes.count  return;
    
    node := *ast.nodes[node_i];
    if node.type == {
        case .NUMBER;
            dbg_print("%", node.number);
            
        case .VARIABLE;
            dbg_print("%", node.variable.name);
            
        case .OPERATION;
            prec := get_operator_precedence(node.operation.name[0]);
            do_parens := ifx prec < parent_prec then true;
            
            if do_parens  dbg_print("(");
            dbg_print_ast(ctxt, node.operation.left, prec);
            dbg_print(node.operation.name);
            dbg_print_ast(ctxt, node.operation.right, prec);
            if do_parens  dbg_print(")");
            
        case .PROCEDURE_CALL;
            dbg_print("%(", node.procedure_call.name);
            for node.procedure_call.params {
                dbg_print_ast(ctxt, it);
                if it_index != node.procedure_call.params.count-1  dbg_print(",");
            }
            dbg_print(")");
    }
}

sprint_ast_node :: (node: AST.Node) -> string {
    if node.type == {
        case .NUMBER;
            return tprint(".NUMBER: %", node.number);
            
        case .VARIABLE;
            return tprint(".VARIABLE: %", node.variable.name);
            
        case .OPERATION;
            return tprint(".OPERATION: %", node.operation.name);
        
        case .PROCEDURE_CALL;
            return tprint(".PROCEDURE_CALL: %", node.procedure_call.name);
        
        case;
            dbg_print("%: ??? ", node.type);
    }
    return "";
}

execute_script :: (script: *Script) -> bool {
    dbg_print("executing script with % statements\n", script.ast.statements.count);
    for * script.ast.statements {
        dbg_print("executing statement #%\n", it_index);
        if !execute_statement(script, it)  return false;
    }
    return true;
}

execute_statement :: (
    script : *Script, 
    stmt   : *AST.Statement
) -> bool {
    if stmt.type == {
      case .ASSIGNMENT;
        dbg_print("statement type is %\n", stmt.type);
        ret, ok := evaluate_expression(script, *stmt.assignment.right);
        if !ok {
            dbg_print("failed to evaluate statement!\n");
            return false;
        }
        
        dst: Any;
        for script.variables {
            if it.name == stmt.assignment.left { 
                dst = it.binding;
                break;
            }
        }
        
        if !dst.value_pointer {
            dbg_print("could not resolve variable '%'\n", stmt.assignment.left);
            return false;
        }
        
        if !remap_data(dst, ret) {
            dbg_print("remap_data failed in assignment statement to script variable '%'\n", stmt.assignment.left);
            return false;
        }
        
        return true;
        
      case;
        return false;
    }
}

// evaluates expression from the root node
evaluate_expression :: inline (script: *Script, expr: *AST.Expression) -> (Any, bool) {
    ret, ok := evaluate_expression_node(script, expr, expr.root);
    return ret, ok;
}

// evaluates expression from the given node index
// TODO: maybe rename to evaulate_expression_node?
evaluate_expression_node :: (
    script : *Script, 
    expr   : *AST.Expression, 
    node_i : int
) -> (
    Any, bool
) {
    if node_i < 0 || node_i >= script.ast.nodes.count {
        dbg_print("Invalid node '%'\n", node_i);
        return Any.{}, false;
    }
    
    node := *script.ast.nodes[node_i];
    if node.type == {
        case .NUMBER;
            dbg_print("returning number literal: %\n", node.number);
            return node.number, true;
            
        case .VARIABLE;
            // get the binding for a given variable if it is not already set
            // TODO: should probably make sure that all variables and procedures resolve before evaluating an expression
            if node.variable.binding.value_pointer == null {
                for * script.variables {
                    if it.name == node.variable.name {
                        node.variable.binding = it.binding;
                        break;
                    }
                }
            }
            if node.variable.binding.value_pointer == null {
                dbg_print("ERROR: No variable found named '%'!\n", node.variable.name);
                return Any.{}, false;
            }
            
            ret := node.variable.binding;
            
            if is_numeric_type(node.variable.binding.type) {
                ret = Dynamic_New_Any(type_info(float32));
                f32_value, ok := get_numeric_value_as_f32(node.variable.binding);
                if !ok  assert(false);
                (cast(*float32)ret.value_pointer).* = f32_value;
            }
            
            dbg_print("returning variable '%': %\n", node.variable.name, ret);
            return ret, true;
            
        case .OPERATION;
            left, ok := evaluate_expression_node(script, expr, node.operation.left);
            if !ok  return Any.{}, false;
            
            right:, ok = evaluate_expression_node(script, expr, node.operation.right);
            if !ok  return Any.{}, false;
            
            arguments: [2] Any = .[ left, right ];
            
            // TODO: move this to typecheck phase, but at least for now we only do it once?
            if !node.operation.ptr {
                script_proc, ok := resolve_procedure(script, node.operation.name, .[ left.type, right.type ], true);
                if !ok  return Any.{}, false;
                node.operation.procedure = script_proc;
            }
            
            if node.operation.flags & .SWAP_ARGUMENT_ORDER_FOR_SYMMETRIC_CALL {
                arguments = .[ right, left ];
            }
            
            ti_proc := cast(*Type_Info_Procedure) node.operation.type;
            assert(ti_proc.return_types.count == 1);
            ret := Dynamic_New_Any(ti_proc.return_types[0]);
            
            if !setup_and_exec_vm_jai(make_procedure_any(node.operation), arguments, Any.[ ret ]) {
                dbg_print("failed while trying to execute dyncall!\n");
                return Any.{}, false;
            }
            
            dbg_print("returning result of operation '%': %\n", node.operation.name, ret);
            
            return ret, true;
            
        case .PROCEDURE_CALL;
            arguments := NewArray(node.procedure_call.params.count, Any,, temp);
            argument_types := NewArray(node.procedure_call.params.count, *Type_Info,, temp); // TODO: this is dumb, but we have to do this until resolving procedure is moved to typechecking step
            for node.procedure_call.params {
                arguments[it_index] = evaluate_expression_node(script, expr, it);
                argument_types[it_index] = arguments[it_index].type;
            }
            dbg_print("arguments: %\n", arguments);
            
            // TODO: move this to typecheck phase, but at least for now we only do it once?
            if !node.procedure_call.ptr {
                script_proc, ok := resolve_procedure(script, node.procedure_call.name, argument_types);
                if !ok {
                    return Any.{}, false;
                }
                node.procedure_call.procedure = script_proc;
            }
            
            ti_proc := cast(*Type_Info_Procedure) node.procedure_call.type;
            assert(ti_proc.return_types.count == 1);
            ret := Dynamic_New_Any(ti_proc.return_types[0]);
            
            if !setup_and_exec_vm_jai(make_procedure_any(node.procedure_call), arguments, .[ ret ]) {
                return Any.{}, false;
            }
            
            return ret, true;
    }
    
    assert(false);
    return Any.{}, false;
}

/*
    Typechecking notes
    
    once we have typechecking, we won't need to abuse temp storage for all intermediate values and their Any's
    and even further, if we improve the interface for dyncall so that we can do a call passing *void instead of Any, then we can cut out the need to allocated space for the Any's to values
    
    Even with current situation, we could improve things by just using a custom stack allocator for the intermediate values and Any's
    
    
*/


// typecheck_expression :: (
//     script : *Script, 
//     expr   : *AST.Expression, 
//     node_i : int
// ) -> (
//     Type, bool
// ) {
//     if node_i < 0 || node_i >= expr.nodes.count {
//         dbg_print("Invalid node.");
//         assert(false);
//         return .{}, false;
//     }
    
//     node := *expr.nodes[node_i];
//     if node.type == {
//         case .NUMBER;
//             return float32, true;
            
//         case .VARIABLE; 
//             if node.variable.binding.value_pointer == null {
//                 dbg_print("ERROR: No variable found named '%'!\n", node.variable.name);
//             }
//             type := ((*Type)*node.variable.binding.type).*;
//             ok   := Internal_Variable_Value.is_valid_type(type) || is_numeric_type(type);
//             return type, ok;
            
//         case .OPERATION;
//             left     := get_expression_type(script, expr, node.operation.left );
//             right    := get_expression_type(script, expr, node.operation.right);
//             type, ok := typecheck_binary_operation(node.operation._operator, left, right);
//             return ret, ok;
            
//         case .PROCEDURE_CALL;
//             if node.procedure_call.proc == null {
//                 dbg_print("ERROR: No procedure found named '%'!\n", node.variable.name);
//             }
            
//             params := NewArray(node.procedure_call.params.count, Internal_Variable_Value,, allocator = temp);
//             for node.procedure_call.params {
//                 params[it_index] = evaluate_expression_node(script, expr, it);
//             }
            
//             ret, ok := node.procedure_call.proc(params);
//             dbg_print("returning result of proc call %: % %\n", node.procedure_call.name, ret.tag, get_as_any(*ret));
//             return ret, true;
//     }
    
//     assert(false);
//     return .{}, false;
// }



#scope_module

DEBUG :: true;

dbg_print :: (format_string: string, args: .. Any, to_standard_error := false) -> bytes_printed: s64 {
    #if DEBUG  return print(format_string, ..args, to_standard_error);
}

