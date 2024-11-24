
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
        
        // try to resolve to internal declaration
        {
            parent_block := identifier.parent;
            while parent_block {
                if parent_block.node_type == {
                  case .BLOCK;
                    declaration := find_declaration_in_block(xx parent_block, identifier.name);
                    if declaration {
                        if cast(*void)declaration > cast(*void)identifier {
                            log("Error: identifier % used before its declaration.", identifier.name);
                            return null;
                        }
                        identifier.resolved_type = .DECLARATION;
                        identifier.declaration = declaration;
                        identifier.value_type = declaration.value_type;
                        if check_flags & .EXPECT_LVALUE {
                            identifier.flags |= .IS_LVALUE;
                        }
                        return identifier.value_type;
                    }
                    
                  case .FOR_LOOP;
                    dbg_print("checking for iterator with identifier '%'\n", identifier.name);
                    declaration := find_iterator_declaration(xx parent_block, identifier.name, check_flags & .EXPECT_LVALUE != 0);
                    if declaration {
                        identifier.resolved_type = .DECLARATION;
                        identifier.declaration   = declaration;
                        identifier.value_type    = declaration.value_type;
                        if check_flags & .EXPECT_LVALUE {
                            identifier.flags |= .IS_LVALUE;
                        }
                        return identifier.value_type;
                    }
                }
                parent_block = parent_block.parent;
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
        
        
      case .DECLARATION;
        // declaration node cannot be an lvalue itself, but may be ok to use as a left value
        // instead of marking this node with .IS_LVALUE, which would affect all instances where this declaration is used,
        //      we will instead mark the individual identifier nodes which refer to a declaration
        //      in execute proc, thse identifiers will just reach into resolved declaration directly to get value pointer
        if check_flags & .EXPECT_LVALUE  return null;
        
        // NOTE: currently I am placing the value type of the declared variable in the value_type slot of the declaration node.
        //       this is different from how other statements are done, which all return void instead.
        //       may have to change this if it creates issues, I suppose.
        declaration := cast(*Node_Declaration) node;
        
        // if type_expression {
            // TODO
            // must resolve to a Type, obviously
        // }
        
        right_type: *Type_Info;
        if declaration.init_expression {
            // TODO: if type expression is present, then we need to pass resolved type as type hint to init_expression
            //       for now, we will just typecheck the init expression and use that to set declaration LHS type
            right_type = typecheck_node(ctxt, declaration.init_expression);
            if right_type == null  return null;
        } else {
            log("Error: init_expression is missing on declaration node.");
            return null;
        }
        
        declaration.value_type    = right_type;
        declaration.value_pointer = Dynamic_New(right_type,, ctxt.allocator);
        dbg_print("allocated space for internal variable '%', type %, node %\n", declaration.name, right_type, node);
        return declaration.value_type;
        
        
      case .ASSIGNMENT;
        if check_flags & .EXPECT_LVALUE  return null;
        assignment := cast(*Node_Assignment) node;
        
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
        
        // value_pointer for it and it_index decls get set later when actually iterating for loop
        for_loop.it_index_decl = .{
            name       = "it_index",
            value_type = type_info(int),
        };
        
        if #complete for_loop.control_type == {
          case .RANGE;
            dbg_print("typechecking range lower\n");
            assert(for_loop.range.lower != null);
            lower_type := typecheck_node(ctxt, for_loop.range.lower, hint_type = type_info(int));
            if lower_type == null  return null;
            
            dbg_print("typechecking range upper\n");
            assert(for_loop.range.upper != null);
            upper_type := typecheck_node(ctxt, for_loop.range.upper, hint_type = lower_type);
            if lower_type != upper_type { 
                dbg_print("Error: type mismatch on lower and upper bounds of range.\n");
                return null;
            }
            
            if !lower_type || (lower_type.type != .INTEGER && lower_type.type != .FLOAT)  return null; 
            for_loop.it_decl = .{        
                name       = "it",
                value_type = lower_type,
            };
            
          case .ARRAY;
            ti_array := cast(*Type_Info_Array) typecheck_node(ctxt, for_loop.array_expression, check_flags = .EXPECT_LVALUE);
            if !ti_array && ti_array.type != .ARRAY  return null;
            for_loop.it_decl = .{
                name       = "it",
                value_type = ti_array.element_type,
            };
        }
        
        // TODO: may not allow float here?
        
        if !typecheck_node(ctxt, for_loop.statement)  return null;
        
        for_loop.value_type = type_info(void);
        return for_loop.value_type;
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

find_declaration_in_block :: (block: *Node_Block, identifier: string) -> *Node_Declaration {
    for block.statements {
        if it.node_type == .DECLARATION {
            declaration := cast(*Node_Declaration) it;
            if declaration.name == identifier {
                return declaration;
            }
        }
    }
    return null;
}

find_iterator_declaration :: (for_loop: *Node_For_Loop, identifier: string, need_lvalue := false) -> *Node_Declaration {
    if for_loop.it_decl.name == identifier {
        return *for_loop.it_decl;
    }
    if for_loop.it_index_decl.name == identifier {
        if need_lvalue {
            log("Error: it_index cannot be used as an lvalue.");
            return null;
        }
        return *for_loop.it_index_decl;
    }
    return null;
}

