
/*
    TODO: Constant evaluation
        we could mark certain node types as constants and do constant evaluation where applicable
        this would not be something we really want to do on initial pass, since we don't want to modify the AST that much
        but it would certainly be a good idea to do before going to bytecode

*/

Typechecking_Flags :: enum_flags {
    NONE :: 0;
    EXPECT_LVALUE :: 1;
}

// TODO: after typechecking, we should probably remove references to source text, since the user may want to free the scripts's source after compiling the script
typecheck_node :: (
    using script  : *Script, 
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
      case .DIRECTIVE;
        return type_info(void);
        
        
      case .LITERAL;
        if check_flags & .EXPECT_LVALUE {
            log("Error: a literal cannot be used as an lvalue.");
            return null;
        }
        
        literal := cast(*Node_Literal) node;
        if literal.literal_type == {
          case .STRING;
            literal.value_type = type_info(string);
            
          case .NUMBER;
            if hint_type && (hint_type.type == .INTEGER || hint_type.type == .FLOAT) {
                dbg_print("returning hinted type: %\n", as_type(hint_type));
                literal.value_type = hint_type;
                // TODO: we could store any numeric literal type in the same space 
                // so that we don't have to call remap_data every time we execute node
            } else {
                literal.value_type = type_info(type_of(literal.number)); 
            }
            
          case .STRUCT;
            // right now, we need to have a hint type or else we cannot infer what the struct literal is for
            // later we will allow parsing explicit type
            ti_struct := cast(*Type_Info_Struct) hint_type;
            if ti_struct.type != .STRUCT {
                log("Error: type hint provided to struct literal is not a struct type.");
                return null;
            }
            
            literal.value_type = ti_struct;
            literal.aggr.value_pointer = Dynamic_New(literal.value_type,, script.allocator);
            dbg_print("allocated space for literal of type %, node %\n", literal.value_type, literal);
            
            // TODO: we could reduce space required for storing intermediate values
            //       if we pass a value_pointer alongside hint_type in typechecking
            //       that way if hint type is accepted, then it can also use value_pointer 
            //       for its backing value instead of allocating its own space for value
            for literal.aggr.expressions {
                if it_index >= ti_struct.members.count {
                    log("Error: too many values provided in struct literal.");
                    return null;
                }
                member  := ti_struct.members[it_index];
                expr_ti := typecheck_node(script, it, hint_type = member.type); 
                if expr_ti != member.type {
                    log("Error: value type provided in struct initializer for field '%' was incorrect. Expected '%', got '%'.", member.name, as_type(member.type), as_type(expr_ti));
                    return null;
                }
            }
            
            
          case .ARRAY;
            log("Error: array literals not yet implemented.");
        }
        return literal.value_type;
        
        
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
            variable_index := resolve_variable(script, identifier.name);
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
        
        // try to resolve identifier as a type here
        {
            for script.type_table {
                if it.name == identifier.name {
                    identifier.resolved_type = .TYPE;
                    identifier.type_info_pointer = it.type;
                    identifier.value_type = type_info(Type);
                    return identifier.value_type;
                }
            }
        }
        
        // if the identifier is referring to a procedure, that will have to be handled in PROCEDURE_CALL case
        dbg_print("Warning: Unable to resolve identifier '%'.\n", identifier.name);
        return null;
        
        
      case .OPERATION;
        if check_flags & .EXPECT_LVALUE  return null;
        operation := cast(*Node_Operation) node;
        
        left_type := typecheck_node(script, operation.left, check_flags, hint_type);
        if !left_type   return null;
        
        right_type := typecheck_node(script, operation.right, check_flags, hint_type);
        if !right_type  return null;
        
        operator_index, swap_args := resolve_builtin_operation(operation.name, left_type, right_type);
        if operator_index >= 0 {
            operation.operator_index = operator_index;
            
            if swap_args {
                operation.left, operation.right = operation.right, operation.left;
            }
            
            operation.value_type = cast(*Type_Info) BUILTIN_OPERATIONS[operator_index].ret;
            
            if is_aggr(operation.value_type) {
                operation.return_ptr = Dynamic_New(operation.value_type,, script.allocator);
                dbg_print("allocating variable of type % at % for node %\n", as_type(operation.value_type), operation.return_ptr, operation);
            }
            
            return operation.value_type;
        } 
        
        // TODO: fix operator overloading later. for now it is just disabled so I can get the major refactor on nodes working
        
        log("Error: operator overloading is currently broken!");
        return null;

        
      case .PROCEDURE_CALL;
        if check_flags & .EXPECT_LVALUE  return null;
        procedure_call := cast(*Node_Procedure_Call) node;
        
        dbg_print("typechecking arguments...\n");
        dbg_print("arg_count: %\n", procedure_call.arguments_count);
        
        ti_proc := cast(*Type_Info_Procedure) typecheck_node(script, procedure_call.procedure_expression);
        
        if procedure_call.procedure_expression.node_type == .IDENTIFIER && ti_proc == null {
            identifier := cast(*Node_Identifier) procedure_call.procedure_expression;
            assert(identifier.resolved_type == .UNRESOLVED);
            
            argument_types := NewArray(procedure_call.arguments_count, *Type_Info,, temp); // TODO
            for procedure_call.arguments {
                argument_type := typecheck_node(script, it);
                if !argument_type  return null;
                argument_types[it_index] = argument_type;
            }
            
            procedure_index := resolve_procedure_index(script, identifier.name, argument_types, node.flags & .OVERLOAD_OP != 0);
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
            if ti_proc.argument_types.count != procedure_call.arguments_count {
                log("Error: incorrect number of arguments provided for procedure call. Expected %, got %.\n", ti_proc.argument_types.count, procedure_call.arguments_count);
                return null;
            }
            for procedure_call.arguments {
                expected_type := ti_proc.argument_types[it_index];
                argument_type := typecheck_node(script, it, hint_type = expected_type);
                if argument_type != expected_type {
                    log("Error: mismatch on argument % for procedure call. Expected %, got %.\n", it, as_type(expected_type), as_type(argument_type));
                    return null;
                }
            }
            
            node.value_type = ifx ti_proc.return_types 
                then ti_proc.return_types[0] 
                else type_info(void);
        }
        
        if is_aggr(node.value_type) {
            procedure_call.return_ptr = Dynamic_New(node.value_type,, script.allocator);
            dbg_print("allocating variable of type % at % for node %\n", as_type(node.value_type), procedure_call.return_ptr, procedure_call);
        }
        return node.value_type;
        
        
      case .DOT;
        dot := cast(*Node_Dot) node;
        left_type := typecheck_node(script, dot.left, check_flags & .EXPECT_LVALUE);
        if !left_type  return null;
        
        if left_type.type == {
          case .STRUCT;
            dot.dot_type = .STRUCT_MEMBER;
            if dot.right.node_type != .IDENTIFIER {
                log("Error: right side of struct member access expression must be an identifier.");
                return null;
            }
            
            member_identifier := cast(*Node_Identifier)dot.right;
            member_ti, member_offset := get_field(xx left_type, member_identifier.name);
            if member_ti == null  return null;
            
            member_identifier.resolved_type = .STRUCT_MEMBER;
            member_identifier.value_type    = member_ti.type;
            
            dot.value_type    = member_ti.type;
            dot.member_offset = member_offset;
            
            if check_flags & .EXPECT_LVALUE {
                dot.flags |= .IS_LVALUE;
            }
            
          case .TYPE;
            if check_flags & .EXPECT_LVALUE  return null;
            
            assert(dot.left.node_type == .IDENTIFIER);
            type_identifier := cast(*Node_Identifier) dot.left;
            assert(type_identifier.resolved_type == .TYPE);
            literal_type := type_identifier.type_info_pointer;
            
            dot.dot_type = .LITERAL;
            literal := cast(*Node_Literal) dot.right;
            if !literal  log("Error: literal was null!");
            if literal.node_type != .LITERAL {
                log("Error: RHS of dot literal is not a literal.");
                return null;
            }
            
            if typecheck_node(script, literal, hint_type = literal_type) != literal_type {
                log("Error: type of RHS of dot literal did not match type of LHS.");
                return null;
            }
            
            dot.value_type = literal_type;
        }
        return dot.value_type;
        
        
      case .SUBSCRIPT;
        subscript := cast(*Node_Subscript) node;
        base_type :=  cast(*Type_Info_Array) typecheck_node(script, subscript.base_expression, check_flags & .EXPECT_LVALUE);
        if base_type.type != .ARRAY {
            log("Typechecking Error: Type '%' cannot be indexed!", as_type(base_type));
            return null;
        }
        
        indexing_type := typecheck_node(script, subscript.indexing_expression/*, hint_type = type_info(int)*/);
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
        
        explicit_type: *Type_Info;
        if declaration.type_expression {
            assert(declaration.type_expression.node_type == .IDENTIFIER);
            type_identifier := declaration.type_expression;
            type_type := typecheck_node(script, type_identifier, hint_type = type_info(Type));
            if !type_type || type_type.type != .TYPE || type_identifier.resolved_type != .TYPE {
                log("Error: Type expression in declaration did not resolve to a Type.");
                return null;
            }
            explicit_type = type_identifier.type_info_pointer;
        }
        
        right_type: *Type_Info;
        if declaration.init_expression {
            right_type = typecheck_node(script, declaration.init_expression, hint_type = explicit_type);
            if right_type == null  return null;
            if explicit_type && explicit_type != right_type {
                log("Error: right side of declaration does not match given type. Expected '%', got '%'.", explicit_type, right_type);
            }
        }
        
        declaration.value_type    = ifx explicit_type else right_type;
        declaration.value_pointer = Dynamic_New(declaration.value_type,, script.allocator);
        dbg_print("allocated space for internal variable '%', type %, node %\n", declaration.name, right_type, node);
        return declaration.value_type;
        
        
      case .ASSIGNMENT;
        if check_flags & .EXPECT_LVALUE  return null;
        assignment := cast(*Node_Assignment) node;
        
        // get type of left side
        // we expect left side to be an lvalue
        left_type := typecheck_node(script, assignment.left, check_flags = .EXPECT_LVALUE);
        if !left_type  return null;
        
        // pass left type as 'hint_type' argument so that we can infer type of struct literals
        right_type := typecheck_node(script, assignment.right, hint_type = left_type);
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
            if !typecheck_node(script, it)  return null;
        }
        
        block.value_type = type_info(void);
        return block.value_type;
        
        
      case .IF_STATEMENT;
        if check_flags & .EXPECT_LVALUE  return null;
        if_statement := cast(*Node_If_Statement) node;
        
        if !typecheck_node(script, if_statement.condition)  return null; // TODO: must be bool or coercible to bool
        if !typecheck_node(script, if_statement.statement)  return null;
        
        if_statement.value_type = type_info(void);
        return if_statement.value_type;
        
        
      case .WHILE_LOOP;
        if check_flags & .EXPECT_LVALUE  return null;
        while_loop := cast(*Node_While_Loop) node;
        
        if !typecheck_node(script, while_loop.condition)  return null; // TODO: must be bool or coercible to bool
        if !typecheck_node(script, while_loop.statement)  return null;
        
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
            lower_type := typecheck_node(script, for_loop.range.lower, hint_type = type_info(int));
            if lower_type == null  return null;
            
            dbg_print("typechecking range upper\n");
            assert(for_loop.range.upper != null);
            upper_type := typecheck_node(script, for_loop.range.upper, hint_type = lower_type);
            if lower_type != upper_type { 
                dbg_print("Error: type mismatch on lower and upper bounds of range.\n");
                return null;
            }
            
            // TODO: may not allow float here?
            if !lower_type || (lower_type.type != .INTEGER && lower_type.type != .FLOAT)  return null; 
            for_loop.it_decl = .{        
                name       = "it",
                value_type = lower_type,
            };
            
          case .ARRAY;
            ti_array := cast(*Type_Info_Array) typecheck_node(script, for_loop.array_expression, check_flags = .EXPECT_LVALUE);
            // TODO: special case for something like a Many_Any / Any_Array_View
            // better option than creating new array view type info's dynamically
            if !ti_array && ti_array.type != .ARRAY  return null;
            for_loop.it_decl = .{
                name       = "it",
                value_type = ti_array.element_type,
            };
        }
        
        
        if !typecheck_node(script, for_loop.statement)  return null;
        
        for_loop.value_type = type_info(void);
        return for_loop.value_type;
    }
    
    return null;
}

typecheck_script :: (script: *Script) -> bool {
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

resolve_variable :: (using script: *Script, ident: string) -> int {
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

