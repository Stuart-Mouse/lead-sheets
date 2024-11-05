
AST :: struct {
    Node_Type :: enum {
        OPERATION;
        NUMBER;
        VARIABLE;
        PROCEDURE_CALL;
    }
    Node :: struct {
        type       : Node_Type;
        src_loc    : Source_Location;
        
        union {
            variable_index: int; // index to variable in Script.variables array
            number: float32;
            procedure_call : struct {
                index  : int;
                params : [..] int; // TODO: should make this not be a dynamic array later if possible. for now, its just convenient
                
            };
            operation: struct {
                index       : int;
                left, right : int;
                swap_args   : bool;
                // TODO: add flags, since we have swap_args and will also need is_builtin
            };
        }
    }
    
    Expression :: struct {
        root: int;
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
    // later maybe we put nodes in separate arrays for each statement ?
    nodes      : [..] Node;
    statements : [..] Statement;
}

free_ast :: (ast: *AST) {
    array_free(ast.nodes);
    memset(*ast.nodes, 0, size_of(Resizable_Array));
}

// for now, void means invalid type ?
get_expression_type :: (expr: *AST.Expression) -> Type { 
    if expr.root >= expr.nodes.count  return void;
    return node_get_value_type(expr.nodes[expr.root]);
}

print_ast :: (script: *Script, node_i: int, parent_prec: int = 0) {
    if node_i < 0 || node_i >= ast.nodes.count  return;
    
    node := *ast.nodes[node_i];
    if node.type == {
        case .NUMBER;
            dbg_print("%", node.number);
            
        case .VARIABLE;
            dbg_print("%", get_node_variable(script, node).name);
            
        case .OPERATION;
            prec := get_operator_precedence(get_node_operation(script, node).name[0]);
            do_parens := ifx prec < parent_prec then true;
            
            if do_parens  dbg_print("(");
            dbg_print_ast(script, node.operation.left, prec);
            dbg_print(get_node_operation(script, node).name);
            dbg_print_ast(script, node.operation.right, prec);
            if do_parens  dbg_print(")");
            
        case .PROCEDURE_CALL;
            dbg_print("%(", get_node_prcedure(script, node).name);
            for node.procedure_call.params {
                dbg_print_ast(script, it);
                if it_index != node.procedure_call.params.count-1  dbg_print(",");
            }
            dbg_print(")");
    }
}

sprint_ast_node :: (script: *Script, node: AST.Node) -> string {
    if node.type == {
        case .NUMBER;
            return tprint(".NUMBER: %", node.number);
            
        case .VARIABLE;
            return tprint(".VARIABLE: %", node_get_variable(script, node).name);
            
        case .OPERATION;
            return tprint(".OPERATION: %", node_get_operation(script, node).name);
        
        case .PROCEDURE_CALL;
            return tprint(".PROCEDURE_CALL: %", node_get_procedure(script, node).name);
        
        case;
            dbg_print("%: ??? ", node.type);
    }
    return "";
}


node_get_value_type :: (script: *Script, node: AST.Node) -> *Type_Info {
    if #complete node.type == {
      case .NUMBER;
        return type_info(float32);
      case .VARIABLE;
        return node_get_variable(script, node).binding.type;
      case .OPERATION;
        return node_get_operation(script, node).type;
      case .PROCEDURE_CALL;
        return node_get_procedure(script, node).type;
    }
    return null;
}


/*
    Convenience accessors for getting the actual variable/procedure/operator from an AST Node.
    Unfortunately these also need the script as context, since this is just an array lookup.
*/

node_get_variable :: (script: *Script, node: AST.Node) -> *External_Variable {
    assert(node.type == .VARIABLE);
    return *script.variables[node.variable_index];
}

node_get_procedure :: (script: *Script, node: AST.Node) -> *Script_Procedure {
    assert(node.type == .PROCEDURE_CALL);
    return *script.procedures[node.procedure_call.index];
}

node_get_operation :: (script: *Script, node: AST.Node) -> *Script_Procedure {
    assert(node.type == .OPERATION);
    return *script.operators[node.operation.index];
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
            variable := node_get_variable(script, node);
            
            ret := variable.binding;
            
            if is_numeric_type(variable.binding.type) {
                ret = Dynamic_New_Any(type_info(float32),, temp);
                f32_value, ok := get_numeric_value_as_f32(variable.binding);
                if !ok  assert(false);
                (cast(*float32)ret.value_pointer).* = f32_value;
            }
            
            dbg_print("returning variable '%': %\n", variable.name, ret);
            return ret, true;
            
        case .OPERATION;
            left, ok := evaluate_expression_node(script, expr, node.operation.left);
            if !ok  return Any.{}, false;
            
            right:, ok = evaluate_expression_node(script, expr, node.operation.right);
            if !ok  return Any.{}, false;
            
            arguments: [2] Any = .[ left, right ];
            
            // // TODO: move this to typecheck phase, but at least for now we only do it once?
            // if !node.operation.ptr {
            //     script_proc, ok := resolve_procedure(script, get_node_operation(script, node).name, .[ left.type, right.type ], true);
            //     if !ok  return Any.{}, false;
            //     node.operation.procedure = script_proc;
            // }
            
            if node.operation.swap_args {
                arguments = .[ right, left ];
            }
            
            procedure := node_get_operation(script, node);
            
            ti_proc := cast(*Type_Info_Procedure) procedure.type;
            assert(ti_proc.return_types.count == 1);
            ret := Dynamic_New_Any(ti_proc.return_types[0],, temp);
            
            if !setup_and_exec_vm_jai(make_procedure_any(procedure), arguments, Any.[ ret ]) {
                dbg_print("failed while trying to execute dyncall!\n");
                return Any.{}, false;
            }
            
            dbg_print("returning result of operation '%': %\n", node_get_operation(script, node).name, ret);
            
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
            // if !node.procedure_call.ptr {
            //     script_proc, ok := resolve_procedure(script, get_node_prcedure(script, node).name, argument_types);
            //     if !ok {
            //         return Any.{}, false;
            //     }
            //     node.procedure_call.procedure = script_proc;
            // }
            
            procedure := node_get_procedure(script, node);
            
            ti_proc := cast(*Type_Info_Procedure) procedure.type;
            assert(ti_proc.return_types.count == 1);
            ret := Dynamic_New_Any(ti_proc.return_types[0],, temp);
            
            if !setup_and_exec_vm_jai(make_procedure_any(procedure), arguments, .[ ret ]) {
                return Any.{}, false;
            }
            
            return ret, true;
    }
    
    assert(false);
    return Any.{}, false;
}



#scope_module

DEBUG :: true;

dbg_print :: (format_string: string, args: .. Any, to_standard_error := false) -> bytes_printed: s64 {
    #if DEBUG  return print(format_string, ..args, to_standard_error);
}

