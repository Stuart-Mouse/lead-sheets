
/*
    TODO: Constant evaluation
        we could mark certain node types as constants and do constant evaluation where applicable
        this would not be something we really want to do on initial pass, since we don't want to modify the AST that much
        but it would certainly be a good idea to do before going to bytecode

*/

Typechecking_Flags :: enum_flags {
    NONE                    :: 0;
    EXPECT_LVALUE           :: 1 << 0;
    IS_STATEMENT_ROOT       :: 1 << 1;
    IS_PROCEDURE_EXPRESSION :: 1 << 2;
}

// TODO: after typechecking, we should probably remove references to source text, since the user may want to free the scripts's source after compiling the script
// FUTURE REFERENCE: for lowering to bytecode, we will actually want to put result stack location on all instructions rather than pushing and popping from stack
typecheck_node :: (
    using script:   *Script, 
    node:           *Node, 
    check_flags:    Typechecking_Flags = .NONE, 
    hint_type:      *Type_Info         = null
    // hint_storage:   *void              = null
) -> (
    value_type:     *Type_Info
) {
    dprint("typechecking node % %\n", node, node.node_type);
    dprint("expecting type: %\n", as_type(hint_type));
    dprint_push_indent();
    
    // if already typechecked, just return resolved type
    // this was not possible before, but will be now that multiple identifier nodes may reference same underlying procedure or variable node
    if node.flags & .TYPECHECKED {
        assert(node.value_type != null);
        return node.value_type;
    }
    
    // Extra checks to run on exit, make sure we are doing things right
    // TODO: can't break from a case, but maybe we should refactor this procedure so that we don't need to defer here...
    defer if node.value_type {
        // if an lvalue is expected, then ensure the node's typechecking acknowledged this fact
        if check_flags & .EXPECT_LVALUE then assert(node.flags & .IS_LVALUE == .IS_LVALUE);
        node.flags |= .TYPECHECKED;
    }
    
    // weird special case for assignment operators. needs to be done up top so we can remove the flag right away.
    // TODO: print source code location on error as well. really, we need to support this in many places for better error reporting.
    if node.flags & .MUST_BE_STATEMENT_ROOT {
        if !(check_flags & .IS_STATEMENT_ROOT) {
            log("Error: This expression '%' can only be used as the root of a statement, not just as an arbitrary expression.", sprint_node(script, node,, temp));
            return null;
        }
    }
    check_flags &= ~.IS_STATEMENT_ROOT;
    
    // TODO: callback here to manually typecheck node
    // return type and error separately
    // if type is set, skip the below
    
    if node.node_type == {
      case Node_Directive;
        node.flags |= .TYPECHECKED; // @Hack? We are getting an error when trying to execute the directive, saying it was not typechecked?
        return type_info(void);
        
        
      case Node_Literal;
        literal := node.(*Node_Literal);
        if check_flags & .EXPECT_LVALUE {
            if !(literal.flags & .IS_MALLEABLE) && literal.literal_type != .ANY {
                log("Error: a literal cannot be used as an lvalue, unless it is marked as being malleable with `?`.");
                return null;
            }
            literal.flags |= .IS_LVALUE;
        }
        
        if literal.literal_type == {
          case .ANY;
            literal.value_type = literal.any.type;
            
          case .STRING;
            literal.value_type = type_info(string);
            
          case .NUMBER;
            if hint_type && (hint_type.type == .INTEGER || hint_type.type == .FLOAT) {
                // dprint("returning hinted type: %\n", as_type(hint_type));
                
                // NOTE: we now remap to value_type just once here, so that we don't have to do it each time in execute
                // TODO: think about rules around automatic casting here...
                //       maybe we don't want to automatically go float -> int, 
                //       since we will now only get float here when the literal had a decimal part
                new_number := Small_Any.{ hint_type, null };
                remap_data(to_any(new_number), to_any(literal.number));
                literal.number = new_number;
            } else {
                // NOTE: also doing a little remap here in default case, 
                //       since we actually want the default float type to be float32
                // if literal.number.type == type_info(float64) {
                    new_number := Small_Any.{ type_info(float32), null };
                    remap_data(to_any(new_number), to_any(literal.number));
                    literal.number = new_number;
                // }
            }
            literal.value_type = literal.number.type; 
            
          case .ENUM;
            if hint_type == null {
                log("Error: no type hint provided to unary dot enum value.");
                return null;
            }
            ti_enum := hint_type.(*Type_Info_Enum);
            if ti_enum.type != .ENUM {
                log("Error: type hint provided to unary dot enum was not an enum type.");
                return null;
            }
            if check_flags & .EXPECT_LVALUE {
                log("Error: an enum literal cannot be used as an lvalue.");
                return null;
            }
            value, found := enum_name_to_value(ti_enum, literal._enum.identifier);
            if !found {
                log("Error: invalid enum value name '%' for enum type '%'.", literal._enum.identifier, as_type(ti_enum));
                return null;
            }
            literal._enum.value = value;
            literal.value_type = ti_enum;
            
          case .STRUCT;
            // type can either be gotten explicitly from literal.aggr.type_identifier, or from the hint type.
            // it is not necessarily an error at this point if the two do not agree, even though it probably will be further up the tree
            ti_struct: *Type_Info_Struct;
            if literal.aggr.type_identifier != null {
                type_identifier_type := typecheck_node(script, literal.aggr.type_identifier);
                if type_identifier_type == null || type_identifier_type.type != .TYPE || literal.aggr.type_identifier.identifier_type != .TYPE {
                    log("Error: Type expression on left-hand side of struct literal did not resolve to a Type.");
                    return null;
                }
                ti_struct = xx literal.aggr.type_identifier.type_info_pointer;
            } else {
                ti_struct = xx hint_type;
            }
            
            if ti_struct == null  return null;
            if ti_struct.type != .STRUCT {
                log("Error: type hint provided to struct literal is not a struct type. (was '%')", as_type(ti_struct));
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
            dprint("allocated space for literal of type %, node %\n", literal.value_type, literal);
            
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
                        identifier.identifier_type = .DECLARATION;
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
                    dprint("checking for iterator with identifier '%'\n", identifier.name);
                    declaration := find_iterator_declaration(xx parent_block, identifier.name, check_flags & .EXPECT_LVALUE != 0);
                    if declaration {
                        identifier.identifier_type = .DECLARATION;
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
        
        // try to resolve identifier as an external variable here
        {
            variable_index := resolve_variable(script, identifier.name);
            if variable_index != -1 {
                if check_flags & .EXPECT_LVALUE {
                    identifier.flags |= .IS_LVALUE;
                }
                identifier.identifier_type  = .EXTERNAL_VARIABLE;
                identifier.variable_index = variable_index;
                identifier.value_type     = variables[variable_index].binding.type;
                return identifier.value_type;
            }
        }
        
        // try to resolve identifier as a type here
        {
            for script.type_table {
                if it.name == identifier.name {
                    if check_flags & .EXPECT_LVALUE {
                        log("Error: a type cannot be used as an lvalue.");
                        return null;
                    }
                    
                    identifier.identifier_type = .TYPE;
                    identifier.type_info_pointer = it.type;
                    identifier.value_type = type_info(Type);
                    return identifier.value_type;
                }
            }
        }
        
        // if the identifier is referring to an external procedure, that will have to be handled in PROCEDURE_CALL case, 
        //     since we need the additional context for overlaod resolution.
        if !(check_flags & .IS_PROCEDURE_EXPRESSION) {
            dprint("Error: Unable to resolve identifier '%'.\n", identifier.name);
        }
        return null;
        
        
      case Node_Operation;
        if check_flags & .EXPECT_LVALUE  return null;
        operation := node.(*Node_Operation);
        dprint("Operation is % '%'\n", operation.operator_type, operation.name);
        
        left_check_flags := check_flags;
        if operation.operator_type == .ASSIGNMENT {
            left_check_flags |= .EXPECT_LVALUE;
            hint_type = null; // just in case?
        }
        left_type := typecheck_node(script, operation.left, left_check_flags, hint_type);
        if !left_type   return null;
        if operation.operator_type == .ASSIGNMENT {
            hint_type = left_type; // so that we can hint right side based on left
        }
        
        right_type: *Type_Info;
        if operation.operator_type != .UNARY {
            right_type = typecheck_node(script, operation.right, check_flags, hint_type);
            if !right_type  return null;
        }
        
        // special case: operator = is always a simple memcpy
        if operation.name == "=" {
            if left_type != right_type {
                log("Error: left and right types must match in a simple assignment statement!");
                return null;
            }
            operation.value_type = left_type;
            return operation.value_type;
        }
        
        operator_index, _, swap_args := resolve_builtin_operation(operation.name, operation.operator_type, left_type, right_type);
        if operator_index >= 0 {
            operation.operator_index = operator_index;
            
            if swap_args {
                operation.left, operation.right = operation.right, operation.left;
            }
            
            operation.value_type = BUILTIN_OPERATIONS[operator_index].ret.(*Type_Info);
            
            if is_aggr(operation.value_type) {
                operation.return_ptr = Dynamic_New(operation.value_type,, script.allocator);
                dprint("allocating variable of type % at % for node %\n", as_type(operation.value_type), operation.return_ptr, operation);
            }
            
            return operation.value_type;
        }
        
        // if operation was not a builtin, then we will try to resolve the operation as an overloaded procedure
        procedure_call := alloc_node(script, Node_Procedure_Call);
        procedure_call.procedure_expression = make_identifier(script, operation.name);
        
        procedure_call.arguments_count  = ifx operation.operator_type == .UNARY then 1 else 2;
        procedure_call.arguments        = operation.left;
        operation.left.next             = operation.right; // link these as we do for procedure arguments
        operation.overload_procedure    = procedure_call;
        operation.value_type            = typecheck_node(script, procedure_call);
        operation.flags                |= .OVERLOAD;
        
        return operation.value_type;
        
        
      case Node_Procedure_Call;
        if check_flags & .EXPECT_LVALUE  return null;
        procedure_call := node.(*Node_Procedure_Call);
        
        dprint("typechecking arguments...\n");
        dprint("arg_count: %\n", procedure_call.arguments_count);
        
        ti_proc := typecheck_node(script, procedure_call.procedure_expression, .IS_PROCEDURE_EXPRESSION).(*Type_Info_Procedure);
        
        if procedure_call.procedure_expression.node_type == Node_Identifier && ti_proc == null {
            identifier := procedure_call.procedure_expression.(*Node_Identifier);
            assert(identifier.identifier_type == .UNRESOLVED);
            
            argument_types := NewArray(procedure_call.arguments_count, *Type_Info,, temp); // TODO
            for procedure_call.arguments {
                argument_type := typecheck_node(script, it);
                if !argument_type  return null;
                argument_types[it_index] = argument_type;
            }
            
            procedure_index := resolve_procedure_index(script, identifier.name, argument_types, node.flags & .OVERLOAD == .OVERLOAD);
            if procedure_index == -1  return null; // error logged by above
            
            identifier.flags |= .TYPECHECKED;
            identifier.identifier_type   = .EXTERNAL_PROCEDURE;
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
            dprint("allocating variable of type % at % for node %\n", as_type(node.value_type), procedure_call.return_ptr, procedure_call);
        }
        return node.value_type;
        
        
      case Node_Cast;
        node_cast := node.(*Node_Cast);
        left_type := typecheck_node(script, node_cast.left);
        if left_type == null  return null;
        
        type_identifier := node_cast.right;
        type_type := typecheck_node(script, type_identifier, hint_type = type_info(Type));
        if !type_type || type_type.type != .TYPE || type_identifier.identifier_type != .TYPE {
            log("Error: Type expression in RHS of cast did not resolve to a Type.");
            return null;
        }
        right_type := type_identifier.type_info_pointer;
        
        // TODO: check if we can actually perform the cast here
        //       for now, we won't precheck this since we will just use remap_data
        
        node.value_type = type_identifier.type_info_pointer;
        return node.value_type;
        
        
      case Node_Dot;
        dot := node.(*Node_Dot);
        
        left_type := typecheck_node(script, dot.left, check_flags & .EXPECT_LVALUE);
        if left_type == null  return null;
        
        if dot.right.node_type != Node_Identifier {
            log("Error: right side of dot expression must be an identifier.");
            return null;
        }
        
        if left_type.type == {
          case .STRUCT;
            // TODO: we should check that member offset is non-negative so as to preven tuse of constants until we can properly support those
            member_identifier := dot.right.(*Node_Identifier);
            member_ti, member_offset := get_field(xx left_type, member_identifier.name);
            if member_ti == null  return null;
            
            member_identifier.identifier_type = .STRUCT_MEMBER;
            member_identifier.value_type      = member_ti.type;
            member_identifier.flags |= .TYPECHECKED;
            
            dot.value_type    = member_ti.type;
            dot.member_offset = member_offset;
            
            if check_flags & .EXPECT_LVALUE then dot.flags |= .IS_LVALUE;
            return dot.value_type;
            
          case .TYPE;   // TODO: maybe we want to allow anything here that can act as a namespace?
            type_identifier := dot.left.(*Node_Identifier);
            if left_type.type != .TYPE || type_identifier.identifier_type != .TYPE {
                log("Error: Type expression on left-hand side of enum literal did not resolve to a Type.");
                return null;
            }
            
            ti_enum := type_identifier.type_info_pointer.(*Type_Info_Enum);
            if ti_enum.type != .ENUM {
                log("Error: type expression on left-hand side of dot must be an enum type.");
                return null;
            }
            
            if check_flags & .EXPECT_LVALUE {
                log("Error: an enum literal cannot be used as an lvalue.");
                return null;
            }
            member_identifier := dot.right.(*Node_Identifier);
            value, found := enum_name_to_value(ti_enum, member_identifier.name);
            if !found {
                log("Error: invalid enum value name '%' for enum type '%'.", member_identifier.name, as_type(ti_enum));
                return null;
            }
            
            member_identifier.identifier_type = .ENUM_VALUE;
            member_identifier.value_type      = ti_enum;
            member_identifier.enum_value      = value;
            member_identifier.flags |= .TYPECHECKED;
            
            dot.value_type = ti_enum;
            
            return dot.value_type;
        }
        
        log("Error: left side of dot expression must be a struct or type.");
        return null;
        
      case Node_Subscript;
        subscript := node.(*Node_Subscript);
        base_type := typecheck_node(script, subscript.base_expression, check_flags & .EXPECT_LVALUE).(*Type_Info_Array);
        if base_type == null || base_type.type != .ARRAY {
            log("Typechecking Error: Type '%' cannot be indexed!", as_type(base_type));
            return null;
        }
        
        indexing_type := typecheck_node(script, subscript.indexing_expression/*, hint_type = type_info(int)*/);
        if indexing_type == null || (indexing_type.type != .INTEGER && indexing_type.type != .FLOAT) {
            log("Typechecking Error: Type '%' cannot be used in an indexing expression!", as_type(indexing_type));
            return null;
        }
        
        subscript.value_type = base_type.element_type;
        if check_flags & .EXPECT_LVALUE then subscript.flags |= .IS_LVALUE;
        return subscript.value_type;
        
        
      case Node_Declaration;
        // declaration node cannot be an lvalue itself, but may be ok to use as a left value
        // instead of marking this node with .IS_LVALUE, which would affect all instances where this declaration is used,
        //      we will instead mark the individual identifier nodes which refer to a declaration
        //      in execute proc, these identifiers will just reach into resolved declaration directly to get value pointer (if that is legal)
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
            if !type_type || type_type.type != .TYPE || type_identifier.identifier_type != .TYPE {
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
        dprint("allocated space for internal variable '%', type %, node %\n", declaration.name, right_type, node);
        return declaration.value_type;
        
        
      case Node_Block;
        if check_flags & .EXPECT_LVALUE  return null;
        block := node.(*Node_Block);
        
        for block.statements {
            if !typecheck_node(script, it, .IS_STATEMENT_ROOT)  return null;
        }
        
        block.value_type = type_info(void);
        return block.value_type;
        
        
      case Node_If_Statement;
        if check_flags & .EXPECT_LVALUE  return null;
        if_statement := node.(*Node_If_Statement);
        
        if !typecheck_node(script, if_statement.condition)  return null; // TODO: must be bool or coercible to bool
        if !typecheck_node(script, if_statement.statement, .IS_STATEMENT_ROOT)  return null;
        
        if_statement.value_type = type_info(void);
        return if_statement.value_type;
        
        
      case Node_While_Loop;
        if check_flags & .EXPECT_LVALUE  return null;
        while_loop := node.(*Node_While_Loop);
        
        if !typecheck_node(script, while_loop.condition)  return null; // TODO: must be bool or coercible to bool
        if !typecheck_node(script, while_loop.statement, .IS_STATEMENT_ROOT)  return null;
        
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
            dprint("typechecking range lower\n");
            assert(for_loop.range.lower != null);
            lower_type := typecheck_node(script, for_loop.range.lower, hint_type = type_info(int));
            if lower_type == null  return null;
            
            dprint("typechecking range upper\n");
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
        
        
        if !typecheck_node(script, for_loop.statement, .IS_STATEMENT_ROOT)  return null;
        
        for_loop.value_type = type_info(void);
        return for_loop.value_type;
    }
    
    return null;
}

typecheck_script :: (script: *Script) -> bool {
    if script.ast_root == null                  return false;
    if script.ast_root.node_type != Node_Block  return false;
    
    for script.ast_root.statements {
        // TODO: probably check specifically that type returned here is void, since statements should not have a value_type.
        if !typecheck_node(script, it, .IS_STATEMENT_ROOT)  return false;
    }
    
    script.flags |= .TYPECHECKED;
    return true;
}

identifier_refers_to_malleable_literal :: (identifier: Node_Literal) -> bool {
    assert(identifier.identifier_type == .DECLARATION);
    return declaration_is_malleable_literal(identifier.declaration);
}

declaration_is_malleable_literal :: (declaration: Node_Declaration) -> bool {
    if declaration.init_expression == null  return false;
    literal := declaration.init_expression;
    return literal.node_type == Node_Literal && (literal.flags & .IS_MALLEABLE);
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


// extracted out primarily so that I can use this in evaluate_node
// I could just call typecheck_node on the identifier but I guess this makes me feel beetter somehow?
resolve_identifier :: (
    using script:   *Script, 
    identifier:     *Node_Identifier, 
    check_flags:    Typechecking_Flags = .NONE
    // hint_type:      *Type_Info         = null
) -> (
    value_type:     *Type_Info
) {
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
                    identifier.identifier_type = .DECLARATION;
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
                dprint("checking for iterator with identifier '%'\n", identifier.name);
                declaration := find_iterator_declaration(xx parent_block, identifier.name, check_flags & .EXPECT_LVALUE != 0);
                if declaration {
                    identifier.identifier_type = .DECLARATION;
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
    
    // try to resolve identifier as an external variable here
    {
        variable_index := resolve_variable(script, identifier.name);
        if variable_index != -1 {
            if check_flags & .EXPECT_LVALUE {
                identifier.flags |= .IS_LVALUE;
            }
            identifier.identifier_type  = .EXTERNAL_VARIABLE;
            identifier.variable_index = variable_index;
            identifier.value_type     = variables[variable_index].binding.type;
            return identifier.value_type;
        }
    }
    
    // try to resolve identifier as a type here
    {
        for script.type_table {
            if it.name == identifier.name {
                if check_flags & .EXPECT_LVALUE {
                    log("Error: a type cannot be used as an lvalue.");
                    return null;
                }
                
                identifier.identifier_type = .TYPE;
                identifier.type_info_pointer = it.type;
                identifier.value_type = type_info(Type);
                return identifier.value_type;
            }
        }
    }
    
    // if the identifier is referring to an external procedure, that will have to be handled in PROCEDURE_CALL case, 
    //     since we need the additional context for overlaod resolution.
    if !(check_flags & .IS_PROCEDURE_EXPRESSION) {
        dprint("Error: Unable to resolve identifier '%'.\n", identifier.name);
    }
    return null;
}