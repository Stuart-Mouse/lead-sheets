
/*
        
    I thought that we may need to have 2 passes for typechecking
    second pass being necessary to put info on each node about stack position
    but that should actually not be necessary
    and, we actually should be able to calculate maxiumum require stack space from leaves to root, since we can just add up sizes of types and pass that info back up the call stack
    
    so TODO: return running stack size from typechecking routines
    also TODO: these procedures should take a script constructor rather than jsut a script, since we will probably remove the dynamic arrays from script itself
*/

Typechecking_Flags :: enum_flags {
    NONE :: 0;
    EXPECT_LVALUE :: 1;
}

// TODO: after typechecking, we should probably remove references to source text, since the user may want to free the scripts's source after compiling the script
typecheck_node :: (
    using ctxt  : *Script_Constructor, 
    node        : *Node, 
    check_flags :  Typechecking_Flags = .NONE, 
    hint_type   : *Type_Info          = null
) -> (
    value_type  : *Type_Info
) {
    dbg_print("typechecking node % %\n", node, node.node_type);
    dbg_print("expecting type: %\n", as_type(hint_type));
    dbg_print_push_indent();
    
    // if already typechecked, just return resolved type.
    // this was not possible before, but will be now that multiple identifier nodes may reference same underlying procedure or variable node
    if node.value_type  return node.value_type;
    
    if node.node_type == {
      case .NUMBER;
        number := cast(*Node_Number) node;
        if check_flags & .EXPECT_LVALUE {
            log("Error: number literal cannot be used as an lvalue.\n");
            return null;
        }
        
        // TODO: we could store any numeric literal type in the same space so that we don't have to call remap_data every time we execute node
        if hint_type && (hint_type.type == .INTEGER || hint_type.type == .FLOAT) {
            dbg_print("returning hinted type: %\n", as_type(hint_type));
            number.value_type = hint_type;
            return hint_type;
        }
        
        number.value_type = type_info(type_of(number.float_value)); 
        return number.value_type;
        
        
      case .IDENTIFIER;
        identifier := cast(*Node_Identifier) node;
        // if type was resolved, then we should have returned early above.
        assert(identifier.resolved_type == .UNRESOLVED); 
        
        // try to resolve to internal declaration
        {
            scan := identifier.parent;
            assert(scan.node_type == .BLOCK || scan.node_type == .FOR_LOOP);
            if scan.node_type == {
              case .BLOCK;
                 scan = (cast(*Node_Block)scan).statements;
            
              case .FOR_LOOP;
                for_loop := cast(*Node_For_Loop) scan;
                if for_loop.control_expression.node_type == {
                  case .RANGE;
                    if identifier.name == "it" {
                        identifier.resolved_type = .INTERNAL_VARIABLE;
                        identifier.variable_ptr  = *for_loop.iterator_index;
                        identifier.value_type    = for_loop.control_expression.value_type;
                        identifier.flags;
                        return identifier.value_type;
                    }
                    
                  case;
                    if identifier.name == "it" {
                        identifier.resolved_type = .INTERNAL_VARIABLE;
                        identifier.variable_ptr  = *for_loop.iterator_ptr;
                        identifier.value_type    = (cast(*Type_Info_Array)for_loop.control_expression.value_type).element_type;
                        identifier.flags |= .EXTRA_INDIRECTION; // TODO: kind of a hack
                        if check_flags & .EXPECT_LVALUE {
                            identifier.flags |= .IS_LVALUE;
                        }
                        return identifier.value_type;
                    }
                    if identifier.name == "it_index" {
                        identifier.resolved_type = .INTERNAL_VARIABLE;
                        identifier.variable_ptr  = *for_loop.iterator_index;
                        identifier.value_type    = type_info(int);
                        identifier.flags;
                        return identifier.value_type;
                    }
                }
            }
                            
            // TODO: factor this all very differently
            while scan != null {
                dbg_print("scan = % %\n", scan, scan.*);
                defer {
                    if scan.next == null {
                        scan = ifx scan.parent then scan.parent.parent;
                        if scan != null {
                            assert(scan.node_type == .BLOCK || scan.node_type == .FOR_LOOP);
                            // special case for FOR_LOOP at the moment
                            // maybe in future I figure out how to just insert declarations into for loop scope more nicely
                            if scan.node_type == {
                              case .BLOCK;
                                 scan = (cast(*Node_Block)scan).statements;
                            
                              case .FOR_LOOP;
                                for_loop := cast(*Node_For_Loop) scan;
                                if for_loop.control_expression.node_type == {
                                  case .RANGE;
                                    if identifier.name == "it" {
                                        identifier.resolved_type = .INTERNAL_VARIABLE;
                                        identifier.variable_ptr  = *for_loop.iterator_index;
                                        identifier.value_type    = for_loop.control_expression.value_type;
                                        identifier.flags;
                                        return identifier.value_type;
                                    }
                                    
                                  case;
                                    if identifier.name == "it" {
                                        identifier.resolved_type = .INTERNAL_VARIABLE;
                                        identifier.variable_ptr  = *for_loop.iterator_ptr;
                                        identifier.value_type    = (cast(*Type_Info_Array)for_loop.control_expression.value_type).element_type;
                                        identifier.flags |= .EXTRA_INDIRECTION; // TODO: kind of a hack
                                        if check_flags & .EXPECT_LVALUE {
                                            identifier.flags |= .IS_LVALUE;
                                        }
                                        return identifier.value_type;
                                    }
                                    if identifier.name == "it_index" {
                                        identifier.resolved_type = .INTERNAL_VARIABLE;
                                        identifier.variable_ptr  = *for_loop.iterator_index;
                                        identifier.value_type    = type_info(int);
                                        identifier.flags;
                                        return identifier.value_type;
                                    }
                                }
                            }
                        }
                    }
                    else scan = scan.next;
                }
                
                if scan.node_type == .ASSIGNMENT {
                    decl := cast(*Node_Assignment) scan;
                    if decl.is_declaration {
                        ident := cast(*Node_Identifier) decl.left;
                        assert(ident.node_type == .IDENTIFIER);
                        if ident.name == identifier.name {
                            if ident > identifier  log("Error: identifier % used before its declaration.", identifier.name);
                            identifier.* = ident.*;   // just copy identifier from declaration
                            identifier.flags = xx 0;  // but don't copy flags! 
                            if check_flags & .EXPECT_LVALUE {
                                identifier.flags |= .IS_LVALUE;
                            }
                            return identifier.value_type;
                        }
                    }
                }
            }
        }
        
        // try to resolve identifier as a variable here
        {
            variable_index := resolve_variable(ctxt, identifier.name);
            if variable_index != -1 {
                if check_flags & .EXPECT_LVALUE {
                    identifier.flags |= .IS_LVALUE;
                }
                identifier.resolved_type  = .EXTERNAL_VARIABLE;
                identifier.variable_index = variable_index;
                identifier.value_type     = variables[variable_index].binding.type;
                return identifier.value_type;
            }
        }
        
        // if the identifier is referring to a procedure, that will have to be handled in PROCEDURE_CALL case
        log("Warning: Unable to resolve identifier '%'.", identifier.name);
        return null;
        
    
      case .OPERATION;
        if check_flags & .EXPECT_LVALUE  return null;
        operation := cast(*Node_Operation) node;
        
        left_type := typecheck_node(ctxt, operation.left, check_flags, hint_type);
        if !left_type   return null;
        
        right_type := typecheck_node(ctxt, operation.right, check_flags, hint_type);
        if !right_type  return null;
        
        operator_index, swap_args := resolve_builtin_operation(operation.name, left_type, right_type);
        if operator_index >= 0 {
            operation.operator_index = operator_index;
            
            if swap_args {
                operation.left, operation.right = operation.right, operation.left;
            }
            
            operation.value_type = cast(*Type_Info) BUILTIN_OPERATIONS[operator_index].ret;
            
            if is_aggr(operation.value_type) {
                operation.return_ptr = Dynamic_New(operation.value_type,, ctxt.allocator);
                dbg_print("allocating variable of type % at % for node %\n", as_type(operation.value_type), operation.return_ptr, operation);
            }
            
            return operation.value_type;
        } 
        
        // TODO: fix operator overloading later. for now it is just disabled so I can get the major refactor on nodes working
        
        log("Error: operator overloading is currently broken!");
        return null;
        
        // // if it was not one of the builtin operators, 
        // // then we will try checking for an overloaded procedure that performs the operation
        // operation.left.next = operation.right;
        
        // // need to create a new node for the procedure itself, kinda dumb
        // proc_node := alloc_node(ctxt, Node_Identifier);
        // proc_node.src_text        = node.src_text;
        // proc_node.src_loc         = node.src_loc;
        // proc_node.enclosing_scope = node.enclosing_scope;
        
        // // then replace the current operation node with a procedure_call node
        // proc_call_node := Node.{
        //     type            = .PROCEDURE_CALL,
        //     src_loc         = node.src_loc,
        //     src_text        = node.src_text,
        //     flags           = node.flags | .OVERLOAD_OP,
        //     enclosing_scope = node.enclosing_scope,
        //     procedure_call  = .{
        //         procedure_expression = proc_node,
        //         arg_nodes = operation.left,
        //         arg_count = 2, // TODO: what if unary op
        //     }
        // };
        // node.* = proc_call_node;
        
        // #through;
        
      case .PROCEDURE_CALL;
        if check_flags & .EXPECT_LVALUE  return null;
        procedure_call := cast(*Node_Procedure_Call) node;
        
        dbg_print("typechecking arguments...\n");
        dbg_print("arg_count: %\n", procedure_call.arguments_count);
        
        argument_types := NewArray(procedure_call.arguments_count, *Type_Info,, temp); // TODO
        for procedure_call.arguments {
            argument_type := typecheck_node(ctxt, it);
            if !argument_type  return null;
            argument_types[it_index] = argument_type;
        }
        
        ti_proc := cast(*Type_Info_Procedure) typecheck_node(ctxt, procedure_call.procedure_expression);
        
        if procedure_call.procedure_expression.node_type == .IDENTIFIER && ti_proc == null {
            identifier := cast(*Node_Identifier) procedure_call.procedure_expression;
            assert(identifier.resolved_type == .UNRESOLVED);
            
            procedure_index := resolve_procedure_index(ctxt, identifier.name, argument_types, node.flags & .OVERLOAD_OP != 0);
            if procedure_index == -1  return null; // error logged by above
            
            // TODO: remove need to allocate additional node here
            identifier.resolved_type   = .EXTERNAL_PROCEDURE;
            identifier.procedure_index = procedure_index;
            
            ti_proc, identifier.value_type = procedures[procedure_index].type;
            node.value_type = ifx ti_proc.return_types 
                then ti_proc.return_types[0] 
                else type_info(void);
        }
        else {
            if ti_proc == null {
                log("Error: failed to typecheck procedure_expression.");
                return null;
            } 
            if ti_proc.type != .PROCEDURE {
                log("Error: attempt to call a non-procedure type as if it were a procedure.");
                return null;
            }
            if ti_proc.return_types.count > 1 {
                log("Error: attempt to call a procedure which returns more than one value. This is not currently supported.");
                return null;
            }
            
            // typecheck arguments. we know what they must be, so we just straightforwardly match. can also easily add logic to coerce/cast
            // TODO: coerce/up-cast arguments
            if ti_proc.argument_types.count != argument_types.count {
                dbg_print("Error: incorrect number of arguments provided for procedure call. Expected %, got %.\n", ti_proc.argument_types.count, argument_types.count);
                return null;
            }
            for 0..argument_types.count-1 {
                if ti_proc.argument_types[it] != argument_types[it] {
                    dbg_print("Error: mismatch on argument % for procedure call. Expected %, got %.\n", it, as_type(ti_proc.argument_types[it]), as_type(argument_types[it]));
                    return null;
                }
            }
            
            node.value_type = ifx ti_proc.return_types 
                then ti_proc.return_types[0] 
                else type_info(void);
        }
        
        if is_aggr(node.value_type) {
            procedure_call.return_ptr = Dynamic_New(node.value_type,, ctxt.allocator);
            dbg_print("allocating variable of type % at % for node %\n", as_type(node.value_type), procedure_call.return_ptr, procedure_call);
        }
        return node.value_type;
        
        
      case .MEMBER;
        member := cast(*Node_Member) node;
        base_type := typecheck_node(ctxt, member.base_expression, check_flags & .EXPECT_LVALUE);
        if base_type.type != .STRUCT  return null;
        
        member_ti, member_offset := get_field(xx base_type, member.identifier);
        if member_ti == null  return null;
        
        member.value_type = member_ti.type;
        member.offset     = member_offset;
        
        // dbg_print("member value type: %\n", as_type(member_ti.type));
        // dbg_print("member offset: %\n", member_offset);
        
        if check_flags & .EXPECT_LVALUE {
            member.flags |= .IS_LVALUE;
        }
        return member.value_type;
        
        
      case .SUBSCRIPT;
        subscript := cast(*Node_Subscript) node;
        base_type :=  cast(*Type_Info_Array) typecheck_node(ctxt, subscript.base_expression, check_flags & .EXPECT_LVALUE);
        if base_type.type != .ARRAY {
            log("Typechecking Error: Type '%' cannot be indexed!", as_type(base_type));
            return null;
        }
        
        indexing_type := typecheck_node(ctxt, subscript.indexing_expression/*, hint_type = type_info(int)*/);
        if indexing_type.type != .INTEGER && indexing_type.type != .FLOAT {
            log("Typechecking Error: Type '%' cannot be used in an indexing expression!", as_type(indexing_type));
            return null;
        }
        
        subscript.value_type = base_type.element_type;
        if check_flags & .EXPECT_LVALUE {
            subscript.flags |= .IS_LVALUE;
        }
        return subscript.value_type;
        
        
      case .ASSIGNMENT;
        if check_flags & .EXPECT_LVALUE  return null;
        assignment := cast(*Node_Assignment) node;
        
        if assignment.is_declaration {
            // Currently, type of left side of declaration will always be inferred from right side.
            // This is because we do not have explicit types.
            // Also, this means that all declarations are also assignments. 
            
            // TODO: we should check that we are not redeclaring a previously declared identifier
            right_type := typecheck_node(ctxt, assignment.right);
            if !right_type  return null;
            
            identifier := cast(*Node_Identifier) assignment.left;
            assert(identifier.node_type == .IDENTIFIER);
            assert(identifier.name != "");
            assert(identifier.resolved_type == .UNRESOLVED);
            
            identifier.value_type    = right_type;
            identifier.resolved_type = .INTERNAL_VARIABLE;
            identifier.variable_ptr  = Dynamic_New(right_type,, ctxt.allocator);
            identifier.flags |= .IS_LVALUE;
            dbg_print("allocated space for internal variable '%', type %, node %\n", identifier.name, right_type, node);
            return type_info(void);
        }
        
        // get type of left side
        // we expect left side to be an lvalue
        left_type := typecheck_node(ctxt, assignment.left, check_flags = .EXPECT_LVALUE);
        if !left_type  return null;
        
        // pass left type as 'hint_type' argument so that we can infer type of struct literals
        right_type := typecheck_node(ctxt, assignment.right, hint_type = left_type);
        if !right_type  return null;
        
        if left_type != right_type {
            dbg_print("types on left and right side of assignment statement do not match! % != %\n", as_type(left_type), as_type(right_type));
            return null;
        }
        
        assignment.value_type = type_info(void);
        return assignment.value_type;
        
        
      case .BLOCK;
        if check_flags & .EXPECT_LVALUE  return null;
        block := cast(*Node_Block) node;
        
        for block.statements {
            if !typecheck_node(ctxt, it)  return null;
        }
        
        block.value_type = type_info(void);
        return block.value_type;
        
        
      case .IF_STATEMENT;
        if check_flags & .EXPECT_LVALUE  return null;
        if_statement := cast(*Node_If_Statement) node;
        
        if !typecheck_node(ctxt, if_statement.condition)  return null; // TODO: must be bool or coercible to bool
        if !typecheck_node(ctxt, if_statement.statement)  return null;
        
        if_statement.value_type = type_info(void);
        return if_statement.value_type;
        
        
      case .WHILE_LOOP;
        if check_flags & .EXPECT_LVALUE  return null;
        while_loop := cast(*Node_While_Loop) node;
        
        if !typecheck_node(ctxt, while_loop.condition)  return null; // TODO: must be bool or coercible to bool
        if !typecheck_node(ctxt, while_loop.statement)  return null;
        
        while_loop.value_type = type_info(void);
        return while_loop.value_type;
        
        
      case .FOR_LOOP;
        if check_flags & .EXPECT_LVALUE  return null;
        for_loop := cast(*Node_For_Loop) node;
        
        control_type: *Type_Info;
        if for_loop.control_expression.node_type == {
          case .RANGE;
            control_type = typecheck_node(ctxt, for_loop.control_expression, hint_type = type_info(int)); 
            
          case;
            control_type = typecheck_node(ctxt, for_loop.control_expression, check_flags = .EXPECT_LVALUE);
        }
        
        // TODO: may not allow float here?
        if !control_type || (control_type.type != .ARRAY && control_type.type != .INTEGER && control_type.type != .FLOAT)  return null; 
        
        if !typecheck_node(ctxt, for_loop.statement)  return null;
        
        for_loop.value_type = type_info(void);
        return for_loop.value_type;
        
        
      case .RANGE;
        if check_flags & .EXPECT_LVALUE  return null;
        range := cast(*Node_Range) node;
        
        dbg_print("typechecking range lower\n");
        assert(range.lower != null);
        lower_type := typecheck_node(ctxt, range.lower, hint_type = type_info(int));
        if lower_type == null  return null;
        
        dbg_print("typechecking range upper\n");
        assert(range.upper != null);
        upper_type := typecheck_node(ctxt, range.upper, hint_type = lower_type);
        if lower_type != upper_type { 
            dbg_print("Error: type mismatch on lower and upper bounds of range.\n");
            return null;
        }
        
        range.value_type = lower_type;
        return range.value_type;
        
      // case .ITERATOR;
      //   node.iterator.parent_for = get_enclosing_for_loop(node.enclosing_scope);
      //   if !node.iterator.parent_for {
      //       dbg_print("Error: Attempt to use 'it' ouside of the context of a for loop!\n");
      //       return null;
      //   }
        
      //   control_type := node.iterator.parent_for.for_loop.control_expression.value_type;
      //   if control_type.type == {
      //     case .ARRAY;
      //       node.value_type = (cast(*Type_Info_Array)control_type).element_type;
      //       // in future, may need checks on whether array itself is an lvalue
      //       // this would be important if we wanted to have array literals in scripts
      //       if check_flags & .EXPECT_LVALUE {
      //           node.flags |= .IS_LVALUE;
      //       }
      //       return node.value_type;
            
      //     case .FLOAT; #through; // ehh...
      //     case .INTEGER;
      //       node.value_type = control_type;
      //       if check_flags & .EXPECT_LVALUE {
      //           dbg_print("Error: attempt to use an integer-typed iterator as an lvalue.\n");
      //           return null;
      //       }
      //       return node.value_type;
      //   }
        
      //   dbg_print("Error: Iterator used in a for loop with an invalid control type? (%)\n", as_type(control_type));
      //   return null;
        
        
      // case .ITERATOR_INDEX;
      //   if check_flags & .EXPECT_LVALUE  return null;
        
      //   node.iterator.parent_for = get_enclosing_for_loop(node.enclosing_scope);
      //   if !node.iterator.parent_for  return null;
        
      //   if node.iterator.parent_for.for_loop.control_expression.value_type.type != .ARRAY {
      //       dbg_print("Error: attempt to use it_index in a range-based for loop.\n");
      //       return null;
      //   }
        
      //   node.value_type = type_info(int);
      //   return node.value_type;
    }
    
    return null;
}

typecheck_script :: (script: *Script_Constructor) -> bool {
    if !script.ast_root                 return false;
    if script.ast_root.node_type != .BLOCK  return false;
    
    for *script.ast_root.statements {
        // TODO: probably check specifically that type returned here is void, since statements should not have a value_type.
        if !typecheck_node(script, it, .NONE)  return false;
    }
    
    script.flags |= .TYPECHECKED;
    return true;
}


is_aggr :: (t: Type) -> bool { return is_aggr(cast(*Type_Info)t); }

is_aggr :: (ti: *Type_Info) -> bool {
    if ti.type == {
        case .INTEGER;   return false;
        case .FLOAT;     return false;
        case .ENUM;      return false;
        case .BOOL;      return false;
        case .POINTER;   return false;
        case .PROCEDURE; return false;
        case .TYPE;      return false;
    }
    return true;
}

resolve_variable :: (using ctxt: *Script_Constructor, ident: string) -> int {
    for variables {
        if it.name == ident {
            return it_index;
        }
    }
    return -1;
}

