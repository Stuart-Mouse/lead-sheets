

AST :: struct {
    Node_Type :: enum {
        NUMBER;
        VARIABLE;
        OPERATION;
        PROCEDURE_CALL;
        MEMBER;
        SUBSCRIPT;
    }
    Node :: struct {
        type       : Node_Type;
        value_type : *Type_Info;
        
        src_loc  : Source_Location;
        src_text : string;     // need for identifiers before typechecking step
        
        // usage is context dependent
        // for procedure arguments, points to next argument node
        next  : *AST.Node;
        flags : Flags;
        Flags :: enum_flags { L_VALUE; UNARY_OP; OVERLOAD_OP; };
        
        union {
            variable_index  : int; // index to variable in Script.variables array 
            number          : float32;
            
            procedure_call: struct {
                procedure_index : int; // cannot remove yet, as this would destroy ability to print ast
                arg_nodes       : *AST.Node;
                arg_count       : int;
                dst             : *void;
            }
            operation: struct {
                operator_index  : int; // index to either a builtin operator or an operator procedure
                left, right     : *AST.Node;
                dst             : *void;
            }
            member: struct {
                base        : *AST.Node;
                offset      : int;
                identifier  : string;
            }
            subscript: struct {
                base          : *AST.Node;
                indexing_expr : *AST.Node;
            }
        }
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
    
    // TODO: turn statements into a type of node. no reason for them to be an entirely separate struct
    statements: [..] Statement;
}

free_ast :: (ast: *AST) {
    // array_free(ast.nodes);
    // memset(*ast.nodes, 0, size_of(Resizable_Array));
}

for_expansion :: (node: *AST.Node, body: Code, flags: For_Flags) #expand {
    `it := node;
    `it_index := 0;
    while it != null {
        #insert body;
        it = it.next;
        it_index += 1;
    }
}

// print_ast :: (script: *Script, node: *AST.Node, parent_prec: int = 0) {
//     // if node_i < 0 || node_i >= ast.nodes.count  return;
    
//     // node := *ast.nodes[node_i];
//     if node.type == {
//       case .NUMBER;
//         dbg_print("%", node.number);
        
//       case .VARIABLE;
//         dbg_print("%", get_node_variable(script, node).name);
        
//       case .OPERATION;
//         prec := get_operator_precedence(get_node_operation(script, node).name[0]);
//         do_parens := ifx prec < parent_prec then true;
        
//         if do_parens  dbg_print("(");
//         dbg_print_ast(script, node.operation.left, prec);
//         dbg_print(get_node_operation(script, node).name);
//         dbg_print_ast(script, node.operation.right, prec);
//         if do_parens  dbg_print(")");
        
//       case .PROCEDURE_CALL;
//         dbg_print("%(", get_node_prcedure(script, node).name);
        
//         for node.procedure_call.arg_nodes {
//             dbg_print_ast(script, it);
//             if arg_node_index != node.procedure_call.arg_count-1  dbg_print(",");
            
//             arg_node = arg_node.next         ;
//             arg_node_index += 1;
//         }
//         dbg_print(")");
//     }
// }

sprint_ast_node :: (script: *Script, node: AST.Node) -> string {
    if node.type == {
      case .NUMBER;
        return tprint(".NUMBER: %", node.number);
        
      case .VARIABLE;
        return tprint(".VARIABLE: %", node_get_variable(script, node).name);
        
      case .OPERATION;
        if script.flags & .AST_TYPECHECKED {
            return tprint(".OPERATION: %", BUILTIN_OPERATIONS[node.operation.operator_index].operator_ident);
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
    assert(node.flags & .IS_BUILTIN == 0);
    return *script.operators[node.operation.operator_index];
}



// TODO: move execute procs to their own file, reimplement evaluate procs in their own file as well

/*
    TODO: we should differentiate execute and evaluate procs
    the procs here should be labelled execute, since they work primarily through side effects
        i.e., they push/pop things on the preallocated stack and rely on typecheck phase to have been done correctly

    by contrast, I will probably reintroduce some evaluate procs that basically do things the old way of dynamically typechecking and returning Any's
    these may be useful in the case of scripts that only need to be run once, e.g. evaluating expresions in gon files or seomthing
    
    as it stands, I think the execute procs will probably have decent enough performance characteristics that going to a bytecode will probabyl not be worth it for a good while
    it will be much easier to debug things using the old ast walking method, and because we are typechecked, it's really more like a fat ass bytecode with horrible cache misses
    but we would still have bad cache misses in bytecode with procedure calls anyhow because of the type info navigation that's required
*/

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
        script.stack_ptr = script.stack_base;
        if !execute_statement(script, it)  return false;
    }
    
    sentinel := cast(*type_of(STACK_SENTINEL_VALUE)) (script.stack_base + script.stack_size);
    if sentinel.* != STACK_SENTINEL_VALUE {
        dbg_print("ERROR: sentinel value was overwritten! was %\n", sentinel.*);
        return false;
    }
    
    return true;
}

execute_statement :: (script: *Script, statement: *AST.Statement) -> bool {
    if statement.type == {
      case .ASSIGNMENT;
        if !execute_node(script, statement.assignment.left) {
            dbg_print("failed to evaluate left side of assignment statement!\n");
            return false;
        }
        if !execute_node(script, statement.assignment.right) {
            dbg_print("failed to evaluate right side of assignment statement!\n");
            return false;
        }
        
        l_type := statement.assignment.left.value_type;
        r_type := statement.assignment.right.value_type;
        
        r_value_ptr := stack_pop(*script.stack_ptr, is_aggr(r_type));
        l_value_ptr := stack_pop(*script.stack_ptr, true);
        
        dbg_print("l_value_ptr: % (% %)\n", l_value_ptr, as_type(l_type), Any.{l_type, l_value_ptr});
        dbg_print("r_value_ptr: % (% %)\n", r_value_ptr, as_type(r_type), Any.{r_type, r_value_ptr});
        
        memcpy(l_value_ptr, r_value_ptr, r_type.runtime_size);
        
        return true;
        
      case;
        return false;
    }
}

execute_node :: (script : *Script, node: *AST.Node) -> bool {
    dbg_print("evaluating node %: %\n", node, node.type);
    
    #if DEBUG {
        context.dbg_print_indent += 1;
        defer context.dbg_print_indent -= 1;
    }
    
    dbg_print("stack_pointer offset: %  (%)\n", script.stack_ptr - script.stack_base, script.stack_ptr);
    
    if node.type == {
      case .NUMBER;
        stack_push(*script.stack_ptr, xx **node.number);
        dbg_print("returning number %\n", node.number);
        return true;
        
      case .VARIABLE;
        variable := node_get_variable(script, node);
        by_ptr: bool = (node.flags & .L_VALUE) || (is_aggr(node.value_type));
        stack_push(*script.stack_ptr, *variable.binding.value_pointer, by_ptr);
        return true;
        
      case .OPERATION;
        result_ptr := stack_push(*script.stack_ptr, *node.operation.dst, is_aggr(node.value_type));
        
        if !execute_node(script, node.operation.left )  return false;
        if !execute_node(script, node.operation.right)  return false;
        
        // TODO: maybe use the simpler version of execute_builtin_operation, but with this more dynamic stack popping
        //       this is probably a little bit slower, but it may be worth it to reduce code bloat
        //       the other benefit(?) of going this way is that we could do the swap args swizzle here, rather than swap them at the ast level
        // op    := BUILTIN_OPERATIONS[node.operation.operator_index];
        // right := stack_pop    (*script.stack_ptr, is_aggr(op.right));
        // left  := stack_pop    (*script.stack_ptr, is_aggr(op.left ));
        // ret   := stack_get_top( script.stack_ptr, is_aggr(op.ret  ));
        // if !execute_builtin_operation(node.operation.operator_index, left, right, ret)  return false;
        
        // dbg_print("operation: %\n", op);
        
        execute_builtin_operation(node.operation.operator_index, *script.stack_ptr);
        
        dbg_print("returning result of binary operation: % %\n", as_type(node.value_type), Any.{ node.value_type, result_ptr });
        
        return true;
        
      case .PROCEDURE_CALL;
        // TODO: this will have to be refactored just a bit later for bytecode (to push result ptr after popping args)
        
        return_ptr := script.stack_ptr;
        stack_push(*script.stack_ptr, *node.procedure_call.dst, is_aggr(node.value_type));
        arg_ptr := script.stack_ptr;
        
        for node.procedure_call.arg_nodes {
            if !execute_node(script, it)  return false;
        }
        
        procedure := node_get_procedure(script, node);
        if !do_dyncall(script.dyncall_vm, procedure.ptr, procedure.type, arg_ptr, return_ptr) {
            log("Failed while trying to call procedure '%'!", procedure.name);
            return false;
        }
        
        script.stack_ptr = arg_ptr;
        return true;
        
      case .MEMBER;
        if !execute_node(script, node.member.base)  return false;  
        
        base_ptr := stack_pop(*script.stack_ptr, true);
        stack_push(*script.stack_ptr, *(base_ptr + node.member.offset), is_aggr(node.value_type) || (node.flags & .L_VALUE == .L_VALUE));
        return true;
        
      case .SUBSCRIPT;
        if !execute_node(script, node.subscript.base)  return false;      
        if !execute_node(script, node.subscript.indexing_expr)  return false;      
        
        index_ptr := stack_pop(*script.stack_ptr);
        base_ptr  := stack_pop(*script.stack_ptr, true);
        
        ti_array := cast(*Type_Info_Array) node.subscript.base.value_type;
        indexing_type := node.subscript.indexing_expr.value_type;
        
        // get index as s64
        // TODO: maybe improve this
        //       for bytecode, could do an instruction for inplace type cast
        index: s64;
        if !remap_int(index, Any.{ indexing_type, index_ptr }) {
            dbg_print("ERROR REMAPPING INDEXING EXPRESSION TO INT\n");
        }
        
        // TODO: better method of doing runtime indexing checks ?
        arr_count: int;
        if ti_array.array_type == {
          case .FIXED;
            arr_count = ti_array.array_count;
            
          case .RESIZABLE; #through;
          case .VIEW; 
            raw_array := cast(*Array_View_64) base_ptr;
            arr_count  = raw_array.count;
            base_ptr   = raw_array.data;
        }
        
        if index < 0 || index >= arr_count {
            log("Runtime Error: array index '%' was out of bounds! Max index is %.", index, arr_count-1);
            return false;
        }
        
        stack_push(*script.stack_ptr, *(base_ptr + (index * node.value_type.runtime_size)), is_aggr(node.value_type) || (node.flags & .L_VALUE == .L_VALUE));
        return true;
    }
    
    assert(false);
    return false;
}

/*
    Because we know that all arguments will be contiguous on the stack, we simply bump the stack pointer for each argument and push.
    Of course, we still need to know the argument types, so we need the *Type_Info_Procedure.
*/
do_dyncall :: (vm: *DCCallVM, proc_ptr: *void, proc_type: *Type_Info_Procedure, arg_ptr: *void, return_ptr: *void) -> bool {
    dcReset(vm);
    
    assert(proc_type.return_types.count == 1);
    
    jai_call :: true;
    assert(proc_type.procedure_flags & .IS_C_CALL == 0);
    
    if !jai_call || !(proc_type.procedure_flags & .HAS_NO_CONTEXT) {
        dcArgPointer(vm, *context);
    }
    
    return_type := proc_type.return_types[0];
    
    dbg_print("calling proc %\n", as_type(proc_type));
    
    #if DEBUG {
        context.dbg_print_indent += 1;
        defer context.dbg_print_indent -= 1;
    }
    
    dbg_print("args: ");
    
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
                dcArgPointer(vm, (cast(**void)arg_ptr).*); 
            } else {
                ag := find_or_make_and_register_DCaggr(cast(*Type_Info_Struct)arg_type);
                if !ag  return false;
                dcArgAggr(vm, ag, (cast(**void)arg_ptr).*);
            }
            
          case .ARRAY;
            ti_array := cast(*Type_Info_Array) arg_type;
            if ti_array.array_type == {
              case .VIEW;
                ag := find_or_make_and_register_DCaggr(type_info(Array_View_64));
                if !ag  return false;
                dcArgAggr(vm, ag, (cast(**void)arg_ptr).*);
                
              case .RESIZABLE;
                ag := find_or_make_and_register_DCaggr(type_info(Resizable_Array));
                if !ag  return false;
                dcArgAggr(vm, ag, (cast(**void)arg_ptr).*);
                
              case;
                if !jai_call {
                    log("Error: cannot pass a fixed array to a #c_call procedure!\n");
                    return false;
                }
                
                ag := find_or_make_and_register_DCaggr(xx ti_array);
                if !ag  return false;
                dcArgAggr(vm, ag, (cast(**void)arg_ptr).*);
            }
            
          case; assert(false, "ERROR: Unimplemented case, unsupported type.");
        }
        
        if is_aggr(arg_type) {
            dbg_print("% ", Any.{arg_type, (cast(**void)arg_ptr).*});
            arg_ptr += size_of(*void);
        } else {
            dbg_print("% ", Any.{arg_type, arg_ptr});
            arg_ptr += size_of(*void);
        }
    }
    
    dbg_print("\n");
    
    if is_aggr(return_type) {
        dcArgPointer(vm, (cast(**void)return_ptr).*);
    } else {
        dcArgPointer(vm, return_ptr);
    }
    
    dcCallVoid(vm, xx proc_ptr);
    
    if is_aggr(return_type) {
        dbg_print("returning result of procedure: %\n", Any.{ return_type, (cast(**void)return_ptr).* });
    } else {
        dbg_print("returning result of procedure: %\n", Any.{ return_type, return_ptr });
    }
    
    return true;
}


