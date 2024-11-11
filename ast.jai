

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
        next_arg_node : *AST.Node; 
        
        union {
            variable_index: int; // index to variable in Script.variables array
            number: float32;
            procedure_call: struct {
                procedure_index : int;
                first_arg_node  : *AST.Node;
                arg_count       : int;
            };
            operation: struct {
                operator_index : int; // index to either a builtin operator or an operator procedure
                left, right    : *AST.Node;
                flags          : Operation_Flags;
            };
        }
        
        flags: Flags;
        Flags :: enum_flags { L_VALUE; };
    }
    
    Expression :: struct {
        root: *AST.Node;
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
    // nodes      : [..] Node;
    statements : [..] Statement;
}

free_ast :: (ast: *AST) {
    // array_free(ast.nodes);
    // memset(*ast.nodes, 0, size_of(Resizable_Array));
}

// for now, void means invalid type ?
get_expression_type :: (expr: *AST.Expression) -> Type { 
    if expr.root >= expr.nodes.count  return void;
    return node_get_value_type(expr.nodes[expr.root]);
}

print_ast :: (script: *Script, node: *AST.Node, parent_prec: int = 0) {
    // if node_i < 0 || node_i >= ast.nodes.count  return;
    
    // node := *ast.nodes[node_i];
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
        
        arg_node := node.procedure_call.first_arg_node;
        arg_node_index := 0;
        while arg_node != null {
            dbg_print_ast(script, it);
            if arg_node_index != node.procedure_call.arg_count-1  dbg_print(",");
            
            arg_node = arg_node.next_arg_node;
            arg_node_index += 1;
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
    
    sentinel := cast(*type_of(STACK_SENTINEL_VALUE)) (script.stack_base + script.stack_size);
    if sentinel.* != STACK_SENTINEL_VALUE {
        dbg_print("ERROR: sentinel value was overwritten! was %\n", sentinel.*);
        return false;
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
        
        // right now getting left side as variable is hardcoded
        // TODO: this must change once we have indexing
        assert(stmt.type == .ASSIGNMENT);
        left_node     := stmt.assignment.left.root;
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
    ret, ok := evaluate_expression_node(script, expr, expr.root, script.stack_base); // TODO: assumption that we can use stack base for every expression may not hold in future!
    return ret, ok;
}

// evaluates expression from the given node index
// TODO: we will not need to return an Any after we figure out pool allocator, only need to know stack position
evaluate_expression_node :: (
    script : *Script, 
    expr   : *AST.Expression, // TODO: remove
    node   : *AST.Node,
    stack_pointer: *void
    // TODO: flags for lvalue?
) -> (
    Any, bool
) {
    // if node_i < 0 || node_i >= script.ast.nodes.count {
    //     dbg_print("Invalid node '%'\n", node_i);
    //     return Any.{}, false;
    // }
    
    // node := *script.ast.nodes[node_i];
    dbg_print("evaluating node %: %\n", node, node.type);
    dbg_print("stack_pointer offset: %\n", stack_pointer - script.stack_base);
    
    #if DEBUG {
        context.dbg_print_indent += 1;
        defer context.dbg_print_indent -= 1;
    }
    
    if node.type == {
        case .NUMBER;
            dbg_print("returning number literal: %\n", node.number);
            ret := Any.{ type_info(float32), stack_pointer };
            memcpy(ret.value_pointer, *node.number, size_of(float32));
            return ret, true;
            
        case .VARIABLE;
            variable := node_get_variable(script, node);
            ret: Any = ---;
            
            if is_numeric_type(variable.binding.type) {
                ret = Any.{ type_info(float32), stack_pointer };
                f32_value, ok := get_numeric_value_as_f32(variable.binding);
                if !ok  assert(false);
                (cast(*float32)ret.value_pointer).* = f32_value;
            } else {
                ret = Any.{ variable.binding.type, stack_pointer };
                memcpy(ret.value_pointer, variable.binding.value_pointer, variable.binding.type.runtime_size);
            }
            
            dbg_print("returning variable '%': %\n", variable.name, ret);
            return ret, true;
            
        case .OPERATION;
            sp  := stack_pointer;
            ret := Any.{ node_get_value_type(script, node), sp };
            
            sp += ret.type.runtime_size;
            left, ok := evaluate_expression_node(script, expr, node.operation.left, sp);
            if !ok  return Any.{}, false;
            
            sp += left.type.runtime_size;
            right:, ok = evaluate_expression_node(script, expr, node.operation.right, sp);
            if !ok  return Any.{}, false;
            
            dbg_print("operator index: %\n", node.operation.operator_index);
            dbg_print("operation flags: %\n", node.operation.flags);
            
            arguments := ifx (node.operation.flags & .SWAP_ARGS)
                then Any.[ right, left ]
                else Any.[ left, right ];
            
            if node.operation.flags & .IS_BUILTIN {
                ok = exec_builtin_operation(node.operation.operator_index, arguments[0], arguments[1], ret);
                if !ok  return Any.{}, false;
            }
            else {
                procedure := node_get_procedure(script, node);
                ti_proc := cast(*Type_Info_Procedure) procedure.type;
                assert(ti_proc.return_types.count == 1);
                
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
            sp  := stack_pointer;
            ret := Any.{ node_get_value_type(script, node), sp };
            sp  += ret.type.runtime_size; 
            
            arg_node := node.procedure_call.first_arg_node;
            while arg_node != null {
                evaluate_expression_node(script, expr, arg_node, sp);
                sp += node_get_value_type(script, arg_node).runtime_size; 
                arg_node = arg_node.next_arg_node;
            }
            
            procedure := node_get_procedure(script, node);
            do_dyncall(script.dyncall_vm, procedure, stack_pointer);
                        
            return ret, true;
    }
    
    assert(false);
    return Any.{}, false;
}

do_dyncall :: (vm: *DCCallVM, procedure: *Script_Procedure, stack_pointer: *void) -> bool {
    dcReset(vm);
    
    ti_proc := procedure.type;
    assert(ti_proc.return_types.count == 1);
    
    if !(ti_proc.procedure_flags & .HAS_NO_CONTEXT) {
        dcArgPointer(vm, *context);
    }
    
    ret := Any.{ ti_proc.return_types[0], stack_pointer };
    
    jai_call :: true;
    
    arg_ptr := stack_pointer + ret.type.runtime_size;
    for arg_type: ti_proc.argument_types {
        if arg_type.type == {
          case .POINTER;
            dcArgPointer(vm, (cast(*DCpointer)arg_ptr).*);
            
          case .ENUM; #through;
          case .BOOL; #through;
          case .INTEGER;
            if arg_type.runtime_size == {
              case 1; dcArgInt8 (vm, (cast(*DCint8 )arg_ptr).*); 
              case 2; dcArgInt16(vm, (cast(*DCint16)arg_ptr).*); 
              case 4; dcArgInt32(vm, (cast(*DCint32)arg_ptr).*); 
              case 8; dcArgInt64(vm, (cast(*DCint64)arg_ptr).*); 
              case;
                log("invalid integer size!\n");
                return false;
            }
            
          case .FLOAT;
            if arg_type.runtime_size == {
              case 4; dcArgFloat32(vm, (cast(*DCfloat32)arg_ptr).*); 
              case 8; dcArgFloat64(vm, (cast(*DCfloat64)arg_ptr).*); 
              case;
                log("invalid float size!\n");
                return false;
            }
            
          case .STRUCT;
            if jai_call {
                dcArgPointer(vm, arg_ptr); 
            } else {
                ag := find_or_make_and_register_DCaggr(cast(*Type_Info_Struct)arg_type);
                if !ag  return false;
                dcArgAggr(vm, ag, arg_ptr);
            }
            
          case .ARRAY;
            ti_array := cast(*Type_Info_Array) arg_type;
            if ti_array.array_type == {
              case .VIEW;
                ag := find_or_make_and_register_DCaggr(type_info(Array_View_64));
                if !ag  return false;
                dcArgAggr(vm, ag, arg_ptr);
                
              case .RESIZABLE;
                ag := find_or_make_and_register_DCaggr(type_info(Resizable_Array));
                if !ag  return false;
                dcArgAggr(vm, ag, arg_ptr);
                
              case;
                if !jai_call {
                    log("Error: cannot pass a fixed array to a #c_call procedure!\n");
                    return false;
                }
                
                ag := find_or_make_and_register_DCaggr(xx ti_array);
                if !ag  return false;
                dcArgAggr(vm, ag, arg_ptr);
            }
        }
        
        arg_ptr += arg_type.runtime_size;
    }
    
    dcArgPointer(vm, ret.value_pointer);
    dcCallVoid(vm, xx procedure.ptr);
    
    return true;
}


/*
    Notes about executing scripts
    
    (DONE) for first step, we can just pass 'stack_pointer' argument to evaluate_expression_node so that it knows where to store data behind the Any it returns
    second step we can remove the need to return the Any, since the parent node will just know where the value is and its type
    (DONE) remove need for passing Any's to dyncall
    
    each node places its result at the address pointed to by stack pointer
    no plans to do multiple returns, so we don't need to think about complications of mutliple returns
    
    putting resolved value type on each node might end up being more desirable than using a getter as we have been
        because current method is a bit unweildy
    
    
*/


#scope_module

DEBUG :: true;

#if DEBUG {#add_context dbg_print_indent: int;}

dbg_print :: (format_string: string, args: .. Any, to_standard_error := false) -> bytes_printed: s64 {
    #if DEBUG {
        for 0..context.dbg_print_indent  print("  ");
        return print(format_string, ..args, to_standard_error);
    }
}

