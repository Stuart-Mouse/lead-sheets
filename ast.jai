

AST :: struct {
    Node_Type :: enum {
        NUMBER;
        VARIABLE;
        OPERATION;
        PROCEDURE_CALL;
    }
    Node :: struct {
        type          : Node_Type;
        src_loc       : Source_Location;
        src_text      : string;     // need for identifiers before typechecking step
        next_arg_node : *AST.Node;  // TODO: maybe rename to just next_node, since we could also use this for other purposes
        value_type    : *Type_Info;
        
        union {
            variable_index: int; // index to variable in Script.variables array
            number: float32;
            procedure_call: struct {
                procedure_index : int; // cannot remove yet, as this would destroy ability to print ast
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
    
    Statement_Type :: enum {
        ASSIGNMENT;
    }
    Statement :: struct {
        type : Statement_Type;
        union {
            assignment : struct {
                left  : *AST.Node; // index to variable  // TODO: later we will want to support some kind of expression on left side (primarily for array indexing)
                right : *AST.Node;
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
        if !execute_statement(script, it, script.stack_base)  return false;
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
    statement   : *AST.Statement,
    stack_pointer: *void
) -> bool {
    if statement.type == {
      case .ASSIGNMENT;
        if !evaluate_node(script, statement.assignment.left, stack_pointer) {
            dbg_print("failed to evaluate left side of assignment statement!\n");
            return false;
        }
        if !evaluate_node(script, statement.assignment.right, stack_pointer + size_of(*void)) {
            dbg_print("failed to evaluate right side of assignment statement!\n");
            return false;
        }
        
        l_value_ptr := (cast(**void)stack_pointer).*;
        r_value_ptr := stack_pointer + size_of(*void);
        memcpy(l_value_ptr, r_value_ptr, statement.assignment.right.value_type.runtime_size);
        
        return true;
        
      case;
        return false;
    }
}

evaluate_node :: (
    script : *Script, 
    node   : *AST.Node,
    stack_pointer: *void
) -> bool {
    dbg_print("evaluating node %: %\n", node, node.type);
    dbg_print("stack_pointer offset: %\n", stack_pointer - script.stack_base);
    
    #if DEBUG {
        context.dbg_print_indent += 1;
        defer context.dbg_print_indent -= 1;
    }
    
    if node.type == {
      case .NUMBER;
        memcpy(stack_pointer, *node.number, size_of(float32));
        return true;
        
      case .VARIABLE;
        variable := node_get_variable(script, node);
        if node.flags & .L_VALUE {
            memcpy(stack_pointer, *variable.binding.value_pointer, size_of(*void));
            return true;
        }
        
        // TODO: add back in code for adapting variable to internal type
        memcpy(stack_pointer, variable.binding.value_pointer, variable.binding.type.runtime_size);
        return true;
        
      case .OPERATION;
        sp := stack_pointer;
        
        sp += node.value_type.runtime_size;
        left := sp;
        if !evaluate_node(script, node.operation.left, sp)
            return false;
        
        sp += node.operation.left.value_type.runtime_size;
        right := sp;
        if !evaluate_node(script, node.operation.right, sp)
            return false;
        
        if (node.operation.flags & .SWAP_ARGS) {
            left, right = right, left;
        }
        
        if node.operation.flags & .IS_BUILTIN {
            if !execute_builtin_operation(node.operation.operator_index, left, right, stack_pointer)
                return false;
        } else {
            procedure := node_get_operation(script, node);
            if !do_dyncall(script.dyncall_vm, procedure.ptr, procedure.type, stack_pointer)
                return false;
        }
        
        return true;
        
      case .PROCEDURE_CALL;
        sp := stack_pointer;
        sp += node.value_type.runtime_size; 
        
        arg_node := node.procedure_call.first_arg_node;
        while arg_node != null {
            evaluate_node(script, arg_node, sp);
            sp += arg_node.value_type.runtime_size; 
            arg_node = arg_node.next_arg_node;
        }
        
        procedure := node_get_procedure(script, node);
        if !do_dyncall(script.dyncall_vm, procedure.ptr, procedure.type, stack_pointer) {
            log("Failed while trying to call procedure '%'!", procedure.name);
            return false;
        }
        
        return true;
    }
    
    assert(false);
    return false;
}

do_dyncall :: (vm: *DCCallVM, proc_ptr: *void, proc_type: *Type_Info_Procedure, stack_pointer: *void) -> bool {
    dcReset(vm);
    
    assert(proc_type.return_types.count == 1);
    
    jai_call :: true;
    assert(proc_type.procedure_flags & .IS_C_CALL == 0);
    
    if !jai_call || !(proc_type.procedure_flags & .HAS_NO_CONTEXT) {
        dcArgPointer(vm, *context);
    }
    
    return_type := proc_type.return_types[0];
    
    arg_ptr := stack_pointer + return_type.runtime_size;
    for arg_type: proc_type.argument_types {
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
    
    dcArgPointer(vm, stack_pointer);
    dcCallVoid(vm, xx proc_ptr);
    
    return true;
}


/*
    Notes about executing scripts
    
    (DONE) for first step, we can just pass 'stack_pointer' argument to evaluate_node so that it knows where to store data behind the Any it returns
    (DONE) second step we can remove the need to return the Any, since the parent node will just know where the value is and its type
    (DONE) remove need for passing Any's to dyncall
    (DONE) store resolved value_type on node itself
    
    we can't remove certain redundancies from ast node because they are required for printing ast
    
    we can remove all redundancies if we actually generate a bytecode for execution
        but I am not quite ready to do this yet until some other things are implemented
            majorly, we need to figure out if/for, lvalues, indexing, statement-level organization
    
    we may want to rmeove the Expression type, and also just treat statements as a type of AST node
    that way we can just allocate statement nodes in the pool same as expression nodes
    
    thinking about how we do stack pointer
        right now it kind of grows up
        I thought for a moment maybe we could assign stack offsets / dst any on each node if we grew down instead, but that's not quite right
        we could potentially assign stack positions for each node's result value on first typecheck pass, but it would be wonky and we'd end up with gaps in the stack
        so it seems better to just do as we have done
    
    
    
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

