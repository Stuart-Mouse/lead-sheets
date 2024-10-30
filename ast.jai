
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
                using #as procedure: Script_Procedure; // maybe we can make this be by pointer later if really worth it
                params: [..] int; // should make this not be a dynamic array later if possible. for now, its just convenient
            };
            operation: struct {
                _operator: u8; // for now, just use the character that represents the operation. Later, we will desugar all operations into "procedures", maybe
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
            print("%", node.number);
            
        case .VARIABLE;
            print("%", node.variable.name);
            
        case .OPERATION;
            prec := get_operator_precedence(node.operation._operator);
            do_parens := ifx prec < parent_prec then true;
            
            if do_parens  print("(");
            print_ast(ctxt, node.operation.left, prec);
            print("%", string.{ 1, *node.operation._operator });
            print_ast(ctxt, node.operation.right, prec);
            if do_parens  print(")");
            
        case .PROCEDURE_CALL;
            print("%(", node.procedure_call.name);
            for node.procedure_call.params {
                print_ast(ctxt, it);
                if it_index != node.procedure_call.params.count-1  print(",");
            }
            print(")");
    }
}

sprint_ast_node :: (node: AST.Node) -> string {
    if node.type == {
        case .NUMBER;
            return tprint(".NUMBER: %", node.number);
            
        case .VARIABLE;
            return tprint(".VARIABLE: %", node.variable.name);
            
        case .OPERATION;
            return tprint(".OPERATION: %", string.{ 1, *node.operation._operator });
        
        case .PROCEDURE_CALL;
            return tprint(".PROCEDURE_CALL: %", node.procedure_call.name);
        
        case;
            print("%: ??? ", node.type);
    }
    return "";
}

execute_script :: (script: *Script) -> bool {
    print("executing script with % statements\n", script.ast.statements.count);
    for * script.ast.statements {
        print("executing statement #%\n", it_index);
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
        print("statement type is %\n", stmt.type);
        ret, ok := evaluate_expression(script, *stmt.assignment.right);
        if !ok {
            print("failed to evaluate statement!\n");
            return false;
        }
        
        dst: Any;
        for script.variables 
            if it.name == stmt.assignment.left 
                dst = it.binding;
        
        if !dst.value_pointer {
            print("could not resolve variable '%'\n", stmt.assignment.left);
            return false;
        }
        
        return remap_data(dst, ret);
        
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
        print("Invalid node '%'\n", node_i);
        return Any.{}, false;
    }
    
    node := *script.ast.nodes[node_i];
    if node.type == {
        case .NUMBER;
            print("returning number literal: %\n", node.number);
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
                print("ERROR: No variable found named '%'!\n", node.variable.name);
            }
            
            ret := Dynamic_New_Any(type_info(float32));
            f32_value, ok := get_numeric_value_as_f32(node.variable.binding);
            if !ok  assert(false);
            (cast(*float32)ret.value_pointer).* = f32_value;
            print("returning variable '%': %\n", node.variable.name, ret);
            return ret, true;
            
        case .OPERATION;
            left, ok := evaluate_expression_node(script, expr, node.operation.left);
            if !ok  return Any.{}, false;
            
            right:, ok = evaluate_expression_node(script, expr, node.operation.right);
            if !ok  return Any.{}, false;
            
            arguments: [2] Any = .[ left, right ];
            
            // log("trying to find proc '%'", string.{ 1, *node.operation._operator });
            proc_call: Script_Procedure;
            for *script.operators {
                // log("comparing proc '%'", it.name);
                if it.name != string.{ 1, *node.operation._operator }  continue;
                // log("typechecking args...");
                
                ti_proc := cast(*Type_Info_Procedure) it.type;
                
                // typechecking logic is slightly messy for operators
                {
                    ok := typecheck_arguments(ti_proc, left, right);
                    if !ok && (ti_proc.procedure_flags & .IS_SYMMETRIC) {
                        ok = typecheck_arguments(ti_proc, right, left);
                        if ok {
                            // swap left and right args to match what symmetric call expects
                            arguments = .[ right, left ];
                        }
                    }
                    if !ok  continue;
                }
                
                proc_call = it;
                break;
            }
            if !proc_call.ptr {
                log("Unable to find procedure '%' with matching parameter types!", string.{ 1, *node.operation._operator });
                return Any.{}, false;
            }
            
            ti_proc := cast(*Type_Info_Procedure) proc_call.type;
            assert(ti_proc.return_types.count == 1);
            ret := Dynamic_New_Any(ti_proc.return_types[0]);
            
            if !setup_and_exec_vm_jai(make_procedure_any(proc_call), arguments, Any.[ ret ]) {
                return Any.{}, false;
            }
            
            print("returning result of operation '%': %\n", proc_call.name, ret);
            
            return ret, true;
            
        case .PROCEDURE_CALL;
            arguments := NewArray(node.procedure_call.params.count, Any,, temp);
            for node.procedure_call.params {
                arguments[it_index] = evaluate_expression_node(script, expr, it);
            }
            
            if node.procedure_call.ptr == null {
                for * script.procedures {
                    if it.name != node.procedure_call.name  continue;
                    if !typecheck_arguments(xx it.type, ..arguments)  continue;
                    
                    node.procedure_call.procedure = it;
                    break;
                }
            }
            
            print("arguments: %\n", arguments);
            
            if !node.procedure_call.ptr {
                log("Unable to find procedure '%' with matching parameter types!", node.procedure_call.name);
                return Any.{}, false;
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


// typecheck_expression :: (
//     script : *Script, 
//     expr   : *AST.Expression, 
//     node_i : int
// ) -> (
//     Type, bool
// ) {
//     if node_i < 0 || node_i >= expr.nodes.count {
//         print("Invalid node.");
//         assert(false);
//         return .{}, false;
//     }
    
//     node := *expr.nodes[node_i];
//     if node.type == {
//         case .NUMBER;
//             return float32, true;
            
//         case .VARIABLE; 
//             if node.variable.binding.value_pointer == null {
//                 print("ERROR: No variable found named '%'!\n", node.variable.name);
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
//                 print("ERROR: No procedure found named '%'!\n", node.variable.name);
//             }
            
//             params := NewArray(node.procedure_call.params.count, Internal_Variable_Value,, allocator = temp);
//             for node.procedure_call.params {
//                 params[it_index] = evaluate_expression_node(script, expr, it);
//             }
            
//             ret, ok := node.procedure_call.proc(params);
//             print("returning result of proc call %: % %\n", node.procedure_call.name, ret.tag, get_as_any(*ret));
//             return ret, true;
//     }
    
//     assert(false);
//     return .{}, false;
// }


#import "Data_Packer";