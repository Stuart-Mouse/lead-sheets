
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
    using script:   *Script, 
    node:           *Node, 
    check_flags:    Typechecking_Flags = .NONE, 
    hint_type:      *Type_Info         = null
    // hint_storage:   *void              = null
) -> (
    value_type:     *Type_Info
) {
    dbg_print("typechecking node % %\n", node, node.node_type);
    dbg_print("expecting type: %\n", as_type(hint_type));
    dbg_print_push_indent();
    
    // if already typechecked, just return resolved type.
    // this was not possible before, but will be now that multiple identifier nodes may reference same underlying procedure or variable node
    if node.value_type  return node.value_type;
    
    if node.node_type == {
      case Node_Directive;
        return type_info(void);
        
        
      case Node_Literal;
        literal := node.(*Node_Literal);
        if check_flags & .EXPECT_LVALUE {
            if !(literal.flags & .IS_MALLEABLE) {
                log("Error: a literal cannot be used as an lvalue, unless it is marked as being malleable with `?`.");
                return null;
            }
            literal.flags |= .IS_LVALUE;
        }
        
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
            ti_struct := hint_type.(*Type_Info_Struct);
            if ti_struct.type != .STRUCT {
                log("Error: type hint provided to struct literal is not a struct type.");
                return null;
            }
            
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
            
            // TODO: we could reduce space required for storing intermediate values
            //       if we pass a value_pointer alongside hint_type in typechecking
            //       that way if hint type is accepted, then it can also use value_pointer 
            //       for its backing value instead of allocating its own space for value
            literal.value_type = ti_struct;
            literal.aggr.value_pointer = Dynamic_New(literal.value_type,, script.allocator);
            dbg_print("allocated space for literal of type %, node %\n", literal.value_type, literal);
            
          case .ARRAY;
            log("Error: array literals not yet implemented.");
        }
        return literal.value_type;
        
        
      case Node_Identifier;
        identifier := node.(*Node_Identifier);
        
        // try to resolve to internal declaration
        {
            parent_block := identifier.parent;
            while parent_block {
                if parent_block.node_type == {
                  case Node_Block;
                    declaration := find_declaration_in_block(xx parent_block, identifier.name);
                    if declaration {
                        // TODO: this is dumb, we should actually compare source location, and report both in error message.
                        if declaration.(*void) > identifier.(*void) {
                            log("Error: identifier % used before its declaration.", identifier.name);
                            return null;
                        }
                        identifier.resolved_type = .DECLARATION;
                        identifier.declaration = declaration;
                        identifier.value_type = declaration.value_type;
                        if check_flags & .EXPECT_LVALUE {
                            if (declaration.flags & .IS_CONSTANT) && !declaration_is_malleable_literal(declaration) {
                                log("Error: Attempting to use identifier '%' as an lvalue, but this identifier refers to a constant declaration which is not malleable.", identifier.name);
                                return null;                                
                            }
                            identifier.flags |= .IS_LVALUE;
                        }
                        return identifier.value_type;
                    }
                    
                  case Node_For_Loop;
                    dbg_print("checking for iterator with identifier '%'\n", identifier.name);
                    declaration := find_iterator_declaration(xx parent_block, identifier.name, check_flags & .EXPECT_LVALUE != 0);
                    if declaration {
                        identifier.resolved_type = .DECLARATION;
                        identifier.declaration   = declaration;
                        identifier.value_type    = declaration.value_type;
                        if check_flags & .EXPECT_LVALUE {
                            if (declaration.flags & .IS_CONSTANT) && !declaration_is_malleable_literal(declaration) {
                                log("Error: Attempting to use identifier '%' as an lvalue, but this identifier refers to a constant declaration which is not malleable.", identifier.name);
                                return null;                                
                            }
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
                if check_flags & .EXPECT_LVALUE {
                    log("Error: a type cannot be used as an lvalue.");
                    return null;
                }
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
        
        
      case Node_Operation;
        if check_flags & .EXPECT_LVALUE  return null;
        operation := node.(*Node_Operation);
        
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
            
            operation.value_type = BUILTIN_OPERATIONS[operator_index].ret.(*Type_Info);
            
            if is_aggr(operation.value_type) {
                operation.return_ptr = Dynamic_New(operation.value_type,, script.allocator);
                dbg_print("allocating variable of type % at % for node %\n", as_type(operation.value_type), operation.return_ptr, operation);
            }
            
            return operation.value_type;
        } 
        
        // if operation was not a builtin, then we will try to resolve the operation as an overloaded procedure
        procedure_call := alloc_node(script, Node_Procedure_Call);
        procedure_call.procedure_expression = make_identifier(script, operation.name);
        procedure_call.arguments = operation.left;
        operation.left.next = operation.right;      // link these as we do for procedure arguments
        procedure_call.arguments_count = 2;
        operation.overload_procedure = procedure_call;
        
        operation.value_type = typecheck_node(script, procedure_call);
        return operation.value_type;
        
        
      case Node_Procedure_Call;
        if check_flags & .EXPECT_LVALUE  return null;
        procedure_call := node.(*Node_Procedure_Call);
        
        dbg_print("typechecking arguments...\n");
        dbg_print("arg_count: %\n", procedure_call.arguments_count);
        
        ti_proc := typecheck_node(script, procedure_call.procedure_expression).(*Type_Info_Procedure);
        
        if procedure_call.procedure_expression.node_type == Node_Identifier && ti_proc == null {
            identifier := procedure_call.procedure_expression.(*Node_Identifier);
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
        
        
      case Node_Dot;
        dot := node.(*Node_Dot);
        left_type := typecheck_node(script, dot.left, check_flags & .EXPECT_LVALUE);
        if !left_type  return null;
        
        if left_type.type == {
          case .STRUCT;
            dot.dot_type = .STRUCT_MEMBER;
            if dot.right.node_type != Node_Identifier {
                log("Error: right side of struct member access expression must be an identifier.");
                return null;
            }
            
            member_identifier := dot.right.(*Node_Identifier);
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
            
            assert(dot.left.node_type == Node_Identifier);
            type_identifier := dot.left.(*Node_Identifier);
            assert(type_identifier.resolved_type == .TYPE);
            literal_type := type_identifier.type_info_pointer;
            
            dot.dot_type = .LITERAL;
            literal := dot.right.(*Node_Literal);
            if !literal  log("Error: literal was null!");
            if literal.node_type != Node_Literal {
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
        
        
      case Node_Subscript;
        subscript := node.(*Node_Subscript);
        base_type := typecheck_node(script, subscript.base_expression, check_flags & .EXPECT_LVALUE).(*Type_Info_Array);
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
        
        
      case Node_Declaration;
        // declaration node cannot be an lvalue itself, but may be ok to use as a left value
        // instead of marking this node with .IS_LVALUE, which would affect all instances where this declaration is used,
        //      we will instead mark the individual identifier nodes which refer to a declaration
        //      in execute proc, thse identifiers will just reach into resolved declaration directly to get value pointer
        if check_flags & .EXPECT_LVALUE  return null;
        
        // NOTE: currently I am placing the value type of the declared variable in the value_type slot of the declaration node.
        //       this is different from how other statements are done, which all return void instead.
        //       may have to change this if it creates issues, I suppose.
        declaration := node.(*Node_Declaration);
        
        explicit_type: *Type_Info;
        if declaration.type_expression {
            assert(declaration.type_expression.node_type == Node_Identifier);
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
        
        // hard-coding this for now, just to get it working, but this is not a good long term thing...
        // for now, only constant assignment we can have is a single literal
        // later, we will do some work to pass info back up the syntax tree about whether an expresion can be constant
        if declaration.flags & .IS_CONSTANT {
            if declaration.init_expression.node_type != Node_Literal {
                log("Error: constant declarations can only be made to simple literals at this time.");
                return null;
            }
        }        
        
        declaration.value_type    = ifx explicit_type else right_type;
        declaration.value_pointer = Dynamic_New(declaration.value_type,, script.allocator);
        dbg_print("allocated space for internal variable '%', type %, node %\n", declaration.name, right_type, node);
        return declaration.value_type;
        
        
      case Node_Assignment;
        if check_flags & .EXPECT_LVALUE  return null;
        assignment := node.(*Node_Assignment);
        
        // get type of left side
        // we expect left side to be an lvalue
        left_type := typecheck_node(script, assignment.left, check_flags = .EXPECT_LVALUE);
        if !left_type  return null;
        
        // pass left type as 'hint_type' argument so that we can infer type of struct literals
        right_type := typecheck_node(script, assignment.right, hint_type = left_type);
        if !right_type  return null;
        
        if left_type != right_type {
            log("Error: left and right types in assignment statement do not match! % != %\n", as_type(left_type), as_type(right_type));
            return null;
        }
        
        assignment.value_type = type_info(void);
        return assignment.value_type;
        
        
      case Node_Block;
        if check_flags & .EXPECT_LVALUE  return null;
        block := node.(*Node_Block);
        
        for block.statements {
            if !typecheck_node(script, it)  return null;
        }
        
        block.value_type = type_info(void);
        return block.value_type;
        
        
      case Node_If_Statement;
        if check_flags & .EXPECT_LVALUE  return null;
        if_statement := node.(*Node_If_Statement);
        
        if !typecheck_node(script, if_statement.condition)  return null; // TODO: must be bool or coercible to bool
        if !typecheck_node(script, if_statement.statement)  return null;
        
        if_statement.value_type = type_info(void);
        return if_statement.value_type;
        
        
      case Node_While_Loop;
        if check_flags & .EXPECT_LVALUE  return null;
        while_loop := node.(*Node_While_Loop);
        
        if !typecheck_node(script, while_loop.condition)  return null; // TODO: must be bool or coercible to bool
        if !typecheck_node(script, while_loop.statement)  return null;
        
        while_loop.value_type = type_info(void);
        return while_loop.value_type;
        
        
      case Node_For_Loop;
        if check_flags & .EXPECT_LVALUE  return null;
        for_loop := node.(*Node_For_Loop);
        
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
                log("Error: type mismatch on lower and upper bounds of range.\n");
                return null;
            }
            
            // TODO: may not allow float here?
            if !lower_type || (lower_type.type != .INTEGER && lower_type.type != .FLOAT)  return null; 
            for_loop.it_decl = .{        
                name       = "it",
                value_type = lower_type,
            };
            
          case .ARRAY;
            ti_array := typecheck_node(script, for_loop.array_expression, check_flags = .EXPECT_LVALUE).(*Type_Info_Array);
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
    if script.ast_root == null                  return false;
    if script.ast_root.node_type != Node_Block  return false;
    
    for *script.ast_root.statements {
        // TODO: probably check specifically that type returned here is void, since statements should not have a value_type.
        if !typecheck_node(script, it, .NONE)  return false;
    }
    
    script.flags |= .TYPECHECKED;
    return true;
}

identifier_refers_to_malleable_literal :: (identifier: Node_Literal) -> bool {
    assert(identifier.resolved_type == .DECLARATION);
    return declaration_is_malleable_literal(identifier.declaration);
}

declaration_is_malleable_literal :: (declaration: Node_Declaration) -> bool {
    literal := declaration.init_expression;
    return literal.node_type == Node_Literal 
        && (literal.flags & .IS_MALLEABLE);
}

// TODO: 
// declaration_can_be_used_as_lvalue :: (declaration: Node_Declaration) {
    
// }

is_aggr :: (t: Type) -> bool { return is_aggr(t.(*Type_Info)); }

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
        if it.node_type == Node_Declaration {
            declaration := it.(*Node_Declaration);
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

