

Operation_Flags :: enum_flags { SWAP_ARGS; IS_BUILTIN; };

/*
    Thinking about some further refactoring on AST Nodes
    
    seems like most node types are now using an index to refer to data stored on script itself
    this is probabyl good for performance a bit, since it makes nodes smaller
    but also, we can probably use this fact to simplify the data layout too
    
    and, we will probably want to have literals of other types eventually.
    
    node could have a member 'ref_index' that is used differently depending on node type
    and then we can have the union only cover the type-specific values
    but then again, it doesn't really do us any good to factor out the ref_index thing
    
    so nvm, we wont do that. in fact, I may rename the index property on union members to distinguish them further
    
    
*/

AST :: struct {
    Node_Type :: enum {
        NUMBER;
        VARIABLE;
        OPERATION;
        PROCEDURE_CALL;
    }
    Node :: struct {
        type     : Node_Type;
        src_loc  : Source_Location;
        src_text : string; // need for identifiers before typechecking step
        
        union {
            variable_index: int; // index to variable in Script.variables array
            number: float32;
            procedure_call : struct {
                procedure_index : int;
                argument_nodes  : [..] int; // TODO: should make this not be a dynamic array later if possible. for now, its just convenient
            };
            operation: struct {
                operator_index : int; // index to either a builtin operator or an operator procedure
                left, right    : int;
                flags          : Operation_Flags;
            };
        }
        
        flags: Flags;
        Flags :: enum_flags { L_VALUE; };
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
                left  : Expression; // index to variable  // TODO: later we will want to support some kind of expression on left side (primarily for array indexing)
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
            for node.procedure_call.argument_nodes {
                dbg_print_ast(script, it);
                if it_index != node.procedure_call.argument_nodes.count-1  dbg_print(",");
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
            if script.flags & .AST_TYPECHECKED {
                if node.operation.flags & .IS_BUILTIN {
                    return tprint(".OPERATION: %", BUILTIN_OPERATIONS[node.operation.operator_index].operator_ident);
                }
                return tprint(".OPERATION: %", node_get_operation(script, node).name);
            } else {
                return node.src_text;
            } 
        
        case .PROCEDURE_CALL;
            if script.flags & .AST_TYPECHECKED {
                return tprint(".PROCEDURE_CALL: %", node_get_procedure(script, node).name);
            } else {
                return node.src_text;
            } 
            
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
        if node.operation.flags & .IS_BUILTIN {
            return cast(*Type_Info) BUILTIN_OPERATIONS[node.operation.operator_index].ret;
        }
        return node_get_operation(script, node).type.return_types[0];
        
      case .PROCEDURE_CALL;
        return node_get_procedure(script, node).type.return_types[0];
    }
    return null;
}


/*
    Convenience accessors for getting the actual variable/procedure/operator from an AST Node.
    Unfortunately these also need the script as context, since this is just an array lookup.
*/

node_get_variable :: (script: *Script, node: AST.Node) -> *Script_Variable {
    assert(node.type == .VARIABLE);
    return *script.variables[node.variable_index];
}

node_get_procedure :: (script: *Script, node: AST.Node) -> *Script_Procedure {
    assert(node.type == .PROCEDURE_CALL);
    return *script.procedures[node.procedure_call.procedure_index];
}

// TODO: probably rename to node_get_operation_procedure
node_get_operation :: (script: *Script, node: AST.Node) -> *Script_Procedure {
    assert(node.type == .OPERATION);
    assert(node.operation.flags & .IS_BUILTIN == 0);
    return *script.operators[node.operation.operator_index];
}



execute_script :: (script: *Script) -> bool {
    if !(script.flags & .AST_CONSTRUCTED) {
        dbg_print("Error: cannot execute script before AST has been constructed!\n");
        return false;
    }
    if !(script.flags & .AST_TYPECHECKED) {
        dbg_print("Error: cannot execute script before AST has been typechecked!\n");
        return false;
    }
    
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
        r_value, ok := evaluate_expression(script, *stmt.assignment.right);
        if !ok {
            dbg_print("failed to evaluate statement!\n");
            return false;
        }
        
        assert(stmt.type == .ASSIGNMENT);
        left_node     := script.ast.nodes[stmt.assignment.left.root];
        assert(left_node.type == .VARIABLE);
        left_variable := node_get_variable(script, left_node);
        assert(left_variable.binding.type == r_value.type);
        memcpy(left_variable.binding.value_pointer, r_value.value_pointer, left_variable.binding.type.runtime_size);
        
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
// TODO: we will not need to return an Any after we figure out pool allocator, only need to know stack position
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
    dbg_print("evaluating node %: %\n", node_i, node.type);
    
    #if DEBUG {
        context.dbg_print_indent += 1;
        defer context.dbg_print_indent -= 1;
    }
    
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
            
            dbg_print("operator index: %\n", node.operation.operator_index);
            dbg_print("operation flags: %\n", node.operation.flags);
            
            arguments := ifx (node.operation.flags & .SWAP_ARGS)
                then Any.[ right, left ]
                else Any.[ left, right ];
            
            ret: Any;
            if node.operation.flags & .IS_BUILTIN {
                ret, ok = exec_builtin_operation(node.operation.operator_index, arguments[0], arguments[1]);
                if !ok  return Any.{}, false;
            }
            else {
                procedure := node_get_procedure(script, node);
                ti_proc := cast(*Type_Info_Procedure) procedure.type;
                assert(ti_proc.return_types.count == 1);
                ret = Dynamic_New_Any(ti_proc.return_types[0],, temp);
                
                print("exec proc of type: %\n", type_info_to_type(ti_proc));
                print("args: %\n", arguments);
                
                if !setup_and_exec_vm_jai(make_procedure_any(procedure), arguments, Any.[ ret ]) {
                    dbg_print("failed while trying to execute dyncall!\n");
                    return Any.{}, false;
                }
            }
            
            // dbg_print("returning result of operation '%': %\n", node_get_operation(script, node).name, ret);
            return ret, true;
            
        case .PROCEDURE_CALL;
            arguments := NewArray(node.procedure_call.argument_nodes.count, Any,, temp);
            for node.procedure_call.argument_nodes {
                arguments[it_index] = evaluate_expression_node(script, expr, it);
            }
            dbg_print("arguments: %\n", arguments);
            
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

#if DEBUG {#add_context dbg_print_indent: int;}

dbg_print :: (format_string: string, args: .. Any, to_standard_error := false) -> bytes_printed: s64 {
    #if DEBUG  {
        for 0..context.dbg_print_indent  print("  ");
        return print(format_string, ..args, to_standard_error);
    }
}

