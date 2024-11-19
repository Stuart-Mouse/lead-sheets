

Node_Type :: enum {
    // expressions
    NUMBER;
    VARIABLE;
    OPERATION;
    PROCEDURE_CALL;
    MEMBER;
    SUBSCRIPT;
    
    ITERATOR;
    ITERATOR_INDEX;
    
    // statements
    BLOCK;
    ASSIGNMENT;
    IF_STATEMENT;
    WHILE_LOOP;
    FOR_LOOP;
}

Node :: struct {
    type       : Node_Type;
    value_type : *Type_Info;
    
    src_loc  : Source_Location;
    src_text : string;     // need for identifiers before typechecking step
    enclosing_scope: *Node; // points to parent block/loop
    
    // usage is context dependent
    // for procedure arguments, points to next argument node
    // for statements, points to next statement (or block) in block
    next  : *Node;
    flags : Flags;
    Flags :: enum_flags { IS_LVALUE; UNARY_OP; OVERLOAD_OP; DISCARD_VALUE; };
    
    // TODO: in the future, maybe we want to just have Node subtypes that we cast to.
    //       for now it is convenient to just use a union because most node types are 
    //       roughly the same size and it allows use to replace a node inplace if we need 
    //       to change the type (as we do when converting an overloaded operator to a procedure call)
    union {
        variable_index  : int; // index to variable in Script.variables array 
        number          : float32;
        procedure_call: struct {
            procedure_index : int; // cannot remove yet, as this would destroy ability to print ast
            arg_nodes       : *Node;
            arg_count       : int;
            dst             : *void;
        }
        operation: struct {
            operator_index  : int; // index to either a builtin operator or an operator procedure
            left, right     : *Node;
            dst             : *void;
        }
        member: struct {
            base        : *Node;
            offset      : int;
            identifier  : string;
        }
        subscript: struct {
            base          : *Node;
            indexing_expr : *Node;
        }
        
        // TODO: refactor how nodes work, probably stop using a union
        //       make loops a subclass of blocks, so that we can put common variable resolution logic in block
        block: struct {
            statements: *Node;
        }
        assignment: struct {
            left, right: *Node; 
        }
        if_statement: struct {
            condition: *Node;
            statement: *Node;
        }
        while_loop: struct {
            condition: *Node;
            statement: *Node;
        }
        for_loop: struct {
            control_type: enum { RANGE; ARRAY; };
            control: union {
                // range: struct { min, max: *Node; };
                array: *Node;
            }
            statement: *Node;
            
            iterator_ptr: *void;       // points to some value allocated in pool, iterator node will get a copy of this during typecheck 
            iterator_index: int; // backing value. iterator_index node inside loop will get pointer to this.
        }
        iterator: struct {
            parent_for: *Node;
        }
    }
}

for_expansion :: (node: *Node, body: Code, flags: For_Flags) #expand {
    `it := node;
    `it_index := 0;
    while it != null {
        #insert body;
        it = it.next;
        it_index += 1;
    }
}

sprint_ast_node :: (script: *Script, node: Node) -> string {
    if node.type == {
      case .NUMBER;
        return tprint(".NUMBER: %", node.number);
        
      case .VARIABLE;
        return tprint(".VARIABLE: %", node_get_variable(script, node).name);
        
      case .OPERATION;
        if script.flags & .TYPECHECKED {
            return tprint(".OPERATION: %", BUILTIN_OPERATIONS[node.operation.operator_index].operator_ident);
        } else {
            return node.src_text;
        }
        
      case .PROCEDURE_CALL;
        if script.flags & .TYPECHECKED {
            return tprint(".PROCEDURE_CALL: %", node_get_procedure(script, node).name);
        } else {
            return node.src_text;
        }
        
      case .IF_STATEMENT;
        return tprint(".IF_STATEMENT: if %  %", sprint_ast_node(script, node.if_statement.condition), sprint_ast_node(script, node.if_statement.statement));
        
      case .WHILE_LOOP;
        return tprint(".WHILE_LOOP: while %  %", sprint_ast_node(script, node.while_loop.condition), sprint_ast_node(script, node.while_loop.statement));
        
      case .FOR_LOOP;
        return tprint(".FOR_LOOP: for %  %", sprint_ast_node(script, node.while_loop.condition), sprint_ast_node(script, node.while_loop.statement));
        
      case .BLOCK;
        return "{ ... }";
        
      case;
        return tprint("%: ??? ", node.type);
    }
    return "";
}


/*
    Convenience accessors for getting the actual variable/procedure/operator from an AST Node.
    Unfortunately these also need the script as context, since this is just an array lookup.
*/

node_get_variable :: (script: *Script, node: Node) -> *Script_Variable {
    assert(node.type == .VARIABLE);
    return *script.variables[node.variable_index];
}

node_get_procedure :: (script: *Script, node: Node) -> *Script_Procedure {
    assert(node.type == .PROCEDURE_CALL);
    return *script.procedures[node.procedure_call.procedure_index];
}

// TODO: probably rename to node_get_operation_procedure
node_get_operation :: (script: *Script, node: Node) -> *Script_Procedure {
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
    if !(script.flags & .PARSED) {
        dbg_print("Error: cannot execute script before AST has been constructed!\n");
        return false;
    }
    if !(script.flags & .TYPECHECKED) {
        dbg_print("Error: cannot execute script before AST has been typechecked!\n");
        return false;
    }
    
    script.stack_ptr = script.stack_base;
    
    for script.ast_root.block.statements {
        dbg_print("executing statement #%\n", it_index);
        if !execute_node(script, it)  return false;
    }
    
    sentinel := cast(*type_of(STACK_SENTINEL_VALUE)) (script.stack_base + script.stack_size);
    if sentinel.* != STACK_SENTINEL_VALUE {
        dbg_print("ERROR: sentinel value was overwritten! was %\n", sentinel.*);
        return false;
    }
    
    return true;
}

/*
    Even though the script has already been typechecked, I use stack_push_any and stack_pop_any 
        so that it's easier to get the type info when needed for debugging.
    Using these procedures also just makes the code here a bit easier to read and captures the 
        type-specific rules about additional indirections automatically.
    When moving to bytecode though, we should be just use stack_push and stack_pop, 
        which are probably slightly faster and don't require passing type info.
    
*/
execute_node :: (script : *Script, node: *Node) -> bool {
    dbg_print("evaluating node %: %\n", node, node.type);
    
    #if DEBUG {
        context.dbg_print_indent += 1;
        defer context.dbg_print_indent -= 1;
    }
    
    dbg_print("stack_pointer offset: %  (%)\n", script.stack_ptr - script.stack_base, script.stack_ptr);
    
    if !node.value_type {
        dbg_print("BIG TIME ERROR: node has no set value type!\n");
    }
    
    if node.type == {
      case .NUMBER;
        stack_push_any(script, node.number);
        dbg_print("returning number %\n", node.number);
        return true;
        
      case .VARIABLE;
        variable := node_get_variable(script, node);
        stack_push_any(script, variable.binding, node.flags & .IS_LVALUE != 0);
        return true;
        
      case .OPERATION;
        dbg_print("operation is %\n", node.src_text);
        
        if is_aggr(node.value_type) {
            stack_push_any(script, Any.{ node.value_type, node.procedure_call.dst });         
        } else {
            zero: s64 = 0;
            stack_push_any(script, Any.{ node.value_type, *zero });
        }
        
        // result_ptr := stack_push(*script.stack_ptr, *node.operation.dst, is_aggr(node.value_type));
        
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
        
        dbg_print("returning result of binary operation: % %\n", as_type(node.value_type), Any.{ node.value_type, script.stack_ptr });
        
        return true;
        
      case .PROCEDURE_CALL;
        // TODO: this will have to be refactored just a bit later for bytecode (to push result ptr after popping args)
        //       this will require passing script to do_dyncall so that we can push to stack from within that function
        //       and then we might as well just pass the entire node as well so that we dont have to pass return ptr explicitly.
        //       I am considering just inlining the do_dyncall code here, but it is already relatively wide on its own...
        //       maybe we inline it for now so it works all good and stuff and then un-inline it later?
        //       but we are going to need to use is from the bytecode as well, and also possibly from the eval procs.
        
        return_ptr := script.stack_ptr;
        if is_aggr(node.value_type) {
            stack_push_any(script, Any.{ node.value_type, node.procedure_call.dst });         
        } else {
            zero: s64 = 0;
            stack_push_any(script, Any.{ node.value_type, *zero });
        }
        arg_ptr := script.stack_ptr;
        
        for node.procedure_call.arg_nodes {
            if !execute_node(script, it)  return false;
        }
        
        // script.stack_ptr -= node.procedure_call.arg_count * PTR_SIZE;
        
        procedure := node_get_procedure(script, node);
        if !do_dyncall(script.dyncall_vm, procedure.ptr, procedure.type, arg_ptr, return_ptr) {
        // if !do_dyncall(script.dyncall_vm, procedure.ptr, procedure.type, script.stack_ptr, node.procedure_call.dst) {
            log("Failed while trying to call procedure '%'!", procedure.name);
            return false;
        }
        
        script.stack_ptr = arg_ptr;
        return true;
        
      case .MEMBER;
        if !execute_node(script, node.member.base)  return false;  
        base   := stack_pop_any(script, node.member.base.value_type, true);
        member := Any.{ base.value_pointer + node.member.offset, node.value_type };
        stack_push_any(script, member, node.flags & .IS_LVALUE == .IS_LVALUE);
        return true;
        
      case .SUBSCRIPT;
        if !execute_node(script, node.subscript.base)  return false;      
        if !execute_node(script, node.subscript.indexing_expr)  return false;      
        
        index := stack_pop_any(script, node.subscript.indexing_expr.value_type);
        base  := stack_pop_any(script, node.subscript.base.value_type, true);
        
        ti_array := cast(*Type_Info_Array) node.subscript.base.value_type;
        indexing_type := node.subscript.indexing_expr.value_type;
        
        // get index as s64
        // TODO: maybe improve this
        //       for bytecode, could do an instruction for inplace type cast
        index_as_s64: s64;
        if !remap_int(index_as_s64, index) {
            dbg_print("ERROR REMAPPING INDEXING EXPRESSION TO INT\n");
        }
        
        // TODO: better method of doing runtime indexing checks ?
        base_ptr := base.value_pointer;
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
        
        if index_as_s64 < 0 || index_as_s64 >= arr_count {
            log("Runtime Error: array index '%' was out of bounds! Max index is %.", index_as_s64, arr_count-1);
            return false;
        }
        
        element := Any.{ base_ptr + index_as_s64 * node.value_type.runtime_size, node.value_type };
        stack_push_any(script, element, node.flags & .IS_LVALUE == .IS_LVALUE);
        return true;
        
      case .ASSIGNMENT;
        if !execute_node(script, node.assignment.left) {
            dbg_print("failed to evaluate left side of assignment statement!\n");
            return false;
        }
        if !execute_node(script, node.assignment.right) {
            dbg_print("failed to evaluate right side of assignment statement!\n");
            return false;
        }
        
        r_value := stack_pop_any(script, node.assignment.right.value_type);
        l_value := stack_pop_any(script, node.assignment.left.value_type, true);
        
        dbg_print("l_value: % %\n", as_type(l_value.type), l_value);
        dbg_print("r_value: % %\n", as_type(r_value.type), r_value);
        
        memcpy(l_value.value_pointer, r_value.value_pointer, r_value.type.runtime_size);
        
        return true;
        
      case .BLOCK;
        for node.block.statements {
            dbg_print("executing statement #%\n", it_index);
            if !execute_node(script, it)  return false;
        }
        return true;
        
      case .IF_STATEMENT;
        if !execute_node(script, node.if_statement.condition) {
            dbg_print("failed to evaluate left side of assignment statement!\n");
            return false;
        }
        
        condition := stack_pop_any(script, node.if_statement.condition.value_type);
        dbg_print("condition: % %\n", as_type(condition.type), condition);
        
        condition_as_bool: bool;
        any_to_bool(condition_as_bool, condition);
        if condition_as_bool {
            if !execute_node(script, node.if_statement.statement) {
                dbg_print("failed to evaluate left side of assignment statement!\n");
                return false;
            }
        }
        return true;
        
      case .WHILE_LOOP;
        while loop := true {
            if !execute_node(script, node.while_loop.condition) {
                dbg_print("failed to evaluate left side of assignment statement!\n");
                return false;
            }
            
            condition := stack_pop_any(script, node.while_loop.condition.value_type);
            dbg_print("condition: % %\n", as_type(condition.type), condition);
            
            condition_as_bool: bool;
            any_to_bool(condition_as_bool, condition);
            if !condition_as_bool  break loop;
            
            if !execute_node(script, node.while_loop.statement) {
                dbg_print("failed to evaluate left side of assignment statement!\n");
                return false;
            }
        }
        return true;
        
        
      case .FOR_LOOP;
        if !execute_node(script, node.for_loop.control.array) {
            dbg_print("failed to evaluate left side of assignment statement!\n");
            return false;
        }
        
        ti_array := cast(*Type_Info_Array) node.for_loop.control.array.value_type;
        assert(ti_array.type == .ARRAY);
        array := stack_pop_any(script, ti_array, true); // TODO: maybe not use lvalue later?
        
        base_ptr := array.value_pointer;
        array_count: int;
        if ti_array.array_type == {
          case .FIXED;
            array_count = ti_array.array_count;
            
          case .RESIZABLE; #through;
          case .VIEW; 
            raw_array := cast(*Array_View_64) base_ptr;
            array_count = raw_array.count;
            base_ptr    = raw_array.data;
        }
        
        // these will be accessed by iterator and iterator_index nodes during iteration
        node.for_loop.iterator_ptr   = base_ptr;
        node.for_loop.iterator_index = 0;
        
        while node.for_loop.iterator_index < array_count {
            defer {
                node.for_loop.iterator_ptr   += ti_array.element_type.runtime_size;
                node.for_loop.iterator_index += 1;
            }
            
            if !execute_node(script, node.for_loop.statement) {
                dbg_print("failed to evaluate left side of assignment statement!\n");
                return false;
            }
        }
        return true;
        
      case .ITERATOR;
        iterator_any := Any.{ node.value_type, node.iterator.parent_for.for_loop.iterator_ptr };
        stack_push_any(script, iterator_any, node.flags & .IS_LVALUE != 0);
        return true;
        
      case .ITERATOR_INDEX;
        stack_push_any(script, node.iterator.parent_for.for_loop.iterator_index);
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


