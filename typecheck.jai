
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
    defer if node.value_type {
        if check_flags & .EXPECT_LVALUE {
            ok, message := is_valid_lvalue(node);
            if !ok {
                log(message);
                if node.node_type == Node_Identifier {
                    log("Identifier was '%'", node.(*Node_Identifier).name);
                }
                return null;
            }
            node.flags |= .IS_LVALUE;
        }
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
        
        declaration := resolve_declaration_by_identifier(identifier.name, identifier.parent, check_flags & .EXPECT_LVALUE != 0);
        if declaration {
            if declaration.(*void) > identifier.(*void) {
                log("Error: identifier % used before its declaration.", identifier.name);
                return null;
            }
            identifier.identifier_type = .DECLARATION;
            identifier.declaration     = declaration;
            identifier.value_type      = declaration.value_type;
            return identifier.value_type;
        }
        
        // TODO: maybe we should add some checks to make sure that all externally inserted identifiers don't conflict across categories
        //       for example, we should not have a type identifier which conflicts with an external variable or procedure
        
        // try to resolve identifier as a type here
        {
            for script.type_table {
                if it.name == identifier.name {
                    identifier.identifier_type = .TYPE;
                    identifier.type_info_pointer = it.type;
                    identifier.value_type = type_info(Type);
                    return identifier.value_type;
                }
            }
        }
        
        // try to resolve identifier as an external variable here
        {
            variable_index := resolve_variable(script, identifier.name);
            if variable_index != -1 {
                identifier.identifier_type  = .EXTERNAL_VARIABLE;
                identifier.variable_index = variable_index;
                identifier.value_type     = variables[variable_index].binding.type;
                return identifier.value_type;
            }
        }
        
        // if the identifier is referring to an external procedure, that will have to be handled in PROCEDURE_CALL case, 
        //     since we need the additional context for overlaod resolution.
        if !(check_flags & .IS_PROCEDURE_EXPRESSION) {
            dprint("Error: Unable to resolve identifier '%'.\n", identifier.name);
        }
        return null;
        
        
      case Node_Operation;
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
                ok, replacement := try_implicit_cast(script, operation.right, left_type); 
                if !ok {
                    log("Error: left and right types must match in a simple assignment statement!");
                    return null;
                }
                if replacement {
                    replacement.parent = operation.right.parent;
                    replacement.next   = operation.right.next;
                    operation.right    = replacement;
                }
            }
            operation.value_type = left_type;
            return operation.value_type;
        }
        
        // TODO: figure out better casting system and remove this horrible hack
        // enums are coerced to their underlying type for the purposes of binary operations, then will be cast back to proper type
        // for now, we will just coerce all to s64/int 
        // in order for this to be ok, the values being pointed to need to not have junk in the unused bytes... 
        // and currently we don't do anything to really intentionally ensure this
        coerce_enum_return_type: *Type_Info;
        if left_type.type == .ENUM && left_type == right_type {
            coerce_enum_return_type = left_type;
            left_type  = type_info(s64);
            right_type = type_info(s64);
        }
        
        operator_index, _, swap_args := resolve_builtin_operation(operation.name, operation.operator_type, left_type, right_type);
        if operator_index >= 0 {
            operation.operator_index = operator_index;
            
            if swap_args {
                operation.left, operation.right = operation.right, operation.left;
            }
            
            operation.value_type = BUILTIN_OPERATIONS[operator_index].ret.(*Type_Info);
            
            // TODO: figure out better casting system and remove this horrible hack
            if coerce_enum_return_type != null && operation.value_type == type_info(s64) {
                operation.value_type = coerce_enum_return_type;
            }
            
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
        procedure_call := node.(*Node_Procedure_Call);
        
        dprint("typechecking arguments...\n");
        dprint("arg_count: %\n", procedure_call.arguments_count);
        
        ti_proc := typecheck_node(script, procedure_call.procedure_expression, .IS_PROCEDURE_EXPRESSION).(*Type_Info_Procedure);

        if ti_proc == null && procedure_call.procedure_expression.node_type == Node_Identifier {
            identifier := procedure_call.procedure_expression.(*Node_Identifier);
            assert(identifier.identifier_type == .UNRESOLVED);
            
            // TODO: we should probably refactor typechecking to return a real error code
            //       then we can notify caller if failure is due to a missing type hint
            //       so that we can handle that case more intelligently here when resolving procedure
            
            // pre-typecheck arguments without type hinting
            // if we fail to typecheck a certain node, we ignore it here for now
            // since it's possible it only failed due to lack of a type hint that we can 
            // provide later, after we've found an otherwise suitable overload
            // we also put the arguments in a nice little array view to make things simpler in resolve_procedure_index
            arguments := NewArray(procedure_call.arguments_count, *Node,, temp);
            for procedure_call.arguments {
                typecheck_node(script, it);
                arguments[it_index] = it;
            }
            
            procedure_index := resolve_procedure_index(script, identifier.name, arguments, node.flags & .OVERLOAD == .OVERLOAD);
            if procedure_index == -1  return null; // error message is logged by resolve_procedure_index
            
            identifier.identifier_type = .EXTERNAL_PROCEDURE;
            identifier.procedure_index = procedure_index;
            identifier.flags |= .TYPECHECKED;
            ti_proc, identifier.value_type = procedures[procedure_index].type;
            
            // go back and verify types for all arguments, re-typecheck those that failed due to a missing type hint
            for arguments {
                expected_type := ti_proc.argument_types[it_index];
                if it.value_type == null {
                    dprint("re-typechecking argument with hint_type %", as_type(expected_type));
                    type := typecheck_node(script, it, hint_type = expected_type);
                    if type != expected_type {
                        log("Error: we deferred typechecking a procedure argument in the hopes that we could later hint the type and resolve it that way, but that failed.");
                        log("\tProcedure identifier: '%'", identifier.name);
                        log("\tArgument expression: '%'", sprint_node(script, it,, temp));
                        log("\tExpected type '%', got '%'.", expected_type, type);
                        return null;
                    }
                } else {
                    ok, replacement := try_implicit_cast(script, it, expected_type);
                    if !ok {
                        log("Error: failed while trying to implicitly cast a procedure argument.");
                        log("\tProcedure identifier: '%'", identifier.name);
                        log("\tArgument expression: '%'", sprint_node(script, it,, temp));
                        log("\tExpected type '%', got '%'.", expected_type, it.value_type);
                        return null;
                    }
                    if replacement {
                        // TODO: we need a better means of replacing nodes all throughout typechecking, because it's very hacky right now.
                        replacement.parent = it.parent;
                        replacement.next   = it.next;
                        if it_index == 0
                            then procedure_call.arguments   = replacement;
                            else arguments[it_index-1].next = replacement;
                    }
                }
            }
            
            node.value_type = ifx ti_proc.return_types 
                then ti_proc.return_types[0] 
                else type_info(void);
        }
        // procedure was the result of some larger expression, so it already has a strict type
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
            
            // typecheck arguments. we know what they must be, so we just straightforwardly match, and can hint type trivially
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
        value_type := typecheck_node(script, node_cast.value);
        if value_type == null  return null;
        
        if node_cast.flags & .IMPLICIT {
            node.value_type = node_cast.implicit_type;
        } else {
            type_identifier := node_cast.type_identifier;
            type_type := typecheck_node(script, type_identifier, hint_type = type_info(Type));
            if !type_type || type_type.type != .TYPE || type_identifier.identifier_type != .TYPE {
                log("Error: Type expression in RHS of cast did not resolve to a Type.");
                return null;
            }
            node.value_type = type_identifier.type_info_pointer;
        }
        
        // TODO: check if we can actually perform the cast here
        //       for now, we won't precheck this since we will just use remap_data
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
        return subscript.value_type;
        
        
      case Node_Declaration;
        // NOTE: currently I am placing the value type of the declared variable in the value_type slot of the declaration node.
        //       this is different from how other statements are done, which all return void instead.
        //       may have to change this if it creates issues, I suppose.
        declaration := node.(*Node_Declaration);
        
        // TODO: I am dumb! we are not actually checking for redeclarations right now at all!
        // since declarations don't use an identifier node for the identifier, we actually will want to factor out identifier resolution so that it just takes a string and not a node
        
        other_declaration := resolve_declaration_by_identifier(declaration.name, declaration.parent);
        if other_declaration {
            log("Error: redeclaration of identifier '%'.", declaration.name);
            return null;
        }
        
        
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
        block := node.(*Node_Block);
        
        for block.statements {
            if !typecheck_node(script, it, .IS_STATEMENT_ROOT)  return null;
        }
        
        block.value_type = type_info(void);
        return block.value_type;
        
        
      case Node_If_Statement;
        if_statement := node.(*Node_If_Statement);
        
        if !typecheck_node(script, if_statement.condition)  return null; // TODO: must be bool or coercible to bool
        if !typecheck_node(script, if_statement.statement, .IS_STATEMENT_ROOT)  return null;
        
        if_statement.value_type = type_info(void);
        return if_statement.value_type;
        
        
      case Node_While_Loop;
        while_loop := node.(*Node_While_Loop);
        
        if !typecheck_node(script, while_loop.condition)  return null; // TODO: must be bool or coercible to bool
        if !typecheck_node(script, while_loop.statement, .IS_STATEMENT_ROOT)  return null;
        
        while_loop.value_type = type_info(void);
        return while_loop.value_type;
        
        
      case Node_For_Loop;
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
            
          case .LIST;
            first_type := typecheck_node(script, for_loop.list, check_flags = .EXPECT_LVALUE);
            if first_type == null  return null;
            for for_loop.list.next {
                type := typecheck_node(script, it, check_flags = .EXPECT_LVALUE);
                if type != first_type {
                    log("Error: all expressions in comma-separated iteration list must be of the same type. First type was %, type of element % was %", as_type(first_type), it_index + 2, as_type(type));
                    return null;
                }
            }
            for_loop.it_decl = .{
                name       = "it",
                value_type = first_type,
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

resolve_declaration_by_identifier :: (identifier: string, parent_block: *Node, need_lvalue := false) -> *Node_Declaration {
    scope := parent_block;
    while scope {
        if scope.node_type == {
          case Node_Block;
            declaration := find_declaration_in_block(xx scope, identifier);
            if declaration  return declaration;
            
          case Node_For_Loop;
            // dprint("checking for iterator with identifier '%'\n", identifier);
            declaration := find_iterator_declaration(xx scope, identifier, need_lvalue);
            if declaration  return declaration;
        }
        scope = scope.parent;
    }
    return null;
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



/*
    Coercions and implicit casts
    
    coercion here refers to altering the value_type of a Node_Literal in-place.
    an implicit cast is just an extra cast node that is inserted in to the AST, and the cast still has to be executed at runtime.
    these two things are different, but also really similar and we usually want to just do the correct thing for the givne node, whehter it be a literal or not

    I woud like to make these procedures nicer to use, 
    but there's two different uses I need to keep in sync
        1. checking if an implicit cast or coercion can be done (and getting the 'distance' of that cast)
            this needs to *not* modify the AST
        2. actually doing the implict cast or coercion, which will modify the AST
    These two functions could be separate, but it's less likely that we'll get bugs if the actual doing of the cast 
        just calls the pre-checking function instead of duplicating logic
    but, I still can't avoid duplicating some of the most high-level logic which is the selection between a cast or coercion
    
*/


make_implicit_cast :: (script: *Script, node: *Node, type: *Type_Info) -> *Node {
    _cast := alloc_node(script, Node_Cast);
    _cast.flags |= .IMPLICIT | .TYPECHECKED;
    _cast.implicit_type = type;
    _cast.value         = node;
    _cast.value_type    = type;
    if node.flags & .TYPECHECKED {
        _cast.flags |= .TYPECHECKED;
    }
    return _cast;
}

get_integer_cast_distance :: (from: *Type_Info_Integer, to: *Type_Info_Integer) -> int {
    if (from.signed && !to.signed) 
    || (from.runtime_size > to.runtime_size)
    || (!from.signed && to.signed && to.runtime_size == from.runtime_size)
        then return -1;
    
    // NOTE: for now, we just add +1 for signed. maybe this should be weighted differently.
    from_size := from.runtime_size + from.signed.(int);
    to_size   :=   to.runtime_size +   to.signed.(int);
    return to_size - from_size;
}

get_implicit_cast_distance :: (node: *Node, to: *Type_Info) -> int {
    from := node.value_type;
    if to == from return 0; 
    
    if is_numeric_type(from) && is_numeric_type(to) {
        if node.node_type == Node_Literal
        && node.(*Node_Literal).literal_type == .NUMBER {
            return 0;
        }
        if to.type == from.type {
            if to.type == {
              case .FLOAT;
                distance := to.runtime_size - from.runtime_size;
                return distance;
                
              case .INTEGER;
                distance := get_integer_cast_distance(xx from, xx to);
                return distance;
            }
        }
    }
    
    return -1;
}

try_implicit_cast :: (script: *Script, node: *Node, type: *Type_Info) -> (ok: bool, replacement: *Node) {
    if node.value_type == type  return true, null;
    if try_implicit_reference(node, type) {
        dprint("took implicit reference to node with value_type '%'\n", as_type(node.value_type));
        return true, null;  
    }
    
    if node.node_type == Node_Literal {
        return coerce_literal_type(xx node, type), null;
    }
    
    if get_implicit_cast_distance(node, type) >= 0 {
        return true, make_implicit_cast(script, node, type);
    }
    
    log("Error: unable to implicitly cast type '%' to '%'", as_type(node.value_type), as_type(type));
    return false, null;
}

// we just reach into the literal and re-type it in place
coerce_literal_type :: (literal: *Node_Literal, type: *Type_Info) -> bool {
    if literal.value_type == type  return true;
    if literal.literal_type == {
      case .NUMBER;
        if !is_numeric_type(type)  return false;
        new_number := Small_Any.{ type, null };
        remap_data(to_any(new_number), to_any(literal.number));
        literal.number = new_number;
        return true;
    }
    log("Error: unable to coerce % literal of type '%' to '%'", literal.literal_type, as_type(literal.value_type), as_type(type));
    return false;
}

get_cast_result_type :: (_cast: *Node_Cast) -> *Type_Info {
    return ifx _cast.flags & .IMPLICIT then _cast.implicit_type else _cast.type_identifier.type_info_pointer;
}

can_do_implicit_reference :: (node: *Node, type: *Type_Info) -> bool {
    ti_ptr := type.(*Type_Info_Pointer);
    if ti_ptr.type != .POINTER || ti_ptr.pointer_to != node.value_type  return false;
    
    // TODO: put more logic in here to allow implicit dereference one using #as members?
    return true;
}


// just flags the node as an lvalue if successful
try_implicit_reference :: (node: *Node, type: *Type_Info) -> bool {
    if !can_do_implicit_reference(node, type)  return false;
    // TODO: we may need to recurse in order to check or fix up child nodes that didn't previously know they are expected to be an lvalue? 
    node.flags |= .IS_LVALUE;
    return true;
}

// second return value is an error string. In order to prevent spamming temp storage, I'm not using tprint to actually make the error message here more specific/situational.
// so the caller (probably just typecheck_node) may want to follow up with some additional information such as the identifier name.
// maybe we could come up with some standard system for inserting info from a node so that we can return a format string here...
is_valid_lvalue :: (node: *Node) -> bool, string {
    if node.node_type == {
      case Node_Literal;
        literal := node.(*Node_Literal);
        if (literal.flags & .IS_MALLEABLE) || literal.literal_type == .ANY {
            return true, "";
        }
        return false, "Error: a literal cannot be used as an lvalue, unless it is marked as being malleable with `?`.";
        
      case Node_Identifier;
        identifier := node.(*Node_Identifier);
        if #complete identifier.identifier_type == {
          case .UNRESOLVED;         return false, "Error: an unresolved identifier cannot be used as an lvalue.";
          case .TYPE;               return false, "Error: a type identifier cannot be used as an lvalue.";
          case .ENUM_VALUE;         return false, "Error: using an enum literal as an lvalue is not currently supported.";
          case .EXTERNAL_VARIABLE;  return true,  "";
          case .EXTERNAL_PROCEDURE; return false, "Error: an external procedure cannot be used as an lvalue.";
          case .STRUCT_MEMBER;      return true,  "";
          case .DECLARATION;
            if (identifier.declaration.flags & .IS_CONSTANT) 
            && !declaration_is_malleable_literal(identifier.declaration) {
                return false, "Error: Attempting to use an identifier as an lvalue which refers to a constant declaration which is not malleable.";
            }
            return true, "";
        }
        
      case Node_Dot;
        if node.(*Node_Dot).left.value_type.type == {
          case .STRUCT;     return true,  "";
          case .TYPE;       return false, "Error: an enum literal cannot be used as an lvalue.";
        }
        
      case Node_Subscript; return true,  "";
      
      // NOTE: it is possible these nodes may be able to produce valid lvalues in the future
      case Node_Operation;         return false, "Error: using the result of an operation as an lvalue is not currently supported";
      case Node_Procedure_Call;    return false, "Error: using the result of a procedure call as an lvalue is not currently supported";
      case Node_Cast;              return false, "Error: using the result of a cast as an lvalue is not currently supported";
      
      // TODO: These cases really should not even remotely happen... so maybe we should not even bother with an error message?
      case Node_Declaration;       return false, "Error: a declaration cannot be used as an lvalue;";
      case Node_Block;             return false, "Error: a block cannot be used as an lvalue;";
      case Node_If_Statement;      return false, "Error: an if statement cannot be used as an lvalue;";
      case Node_While_Loop;        return false, "Error: a while loop cannot be used as an lvalue;";
      case Node_For_Loop;          return false, "Error: a for loop cannot be used as an lvalue;";
    }
    
    assert(false, "Invalid node type '%' in is_valid_lvalue.", node.node_type);
    return false, "";
}

// we use this for getting the proper type to hint to procedure arguments
get_base_if_type_is_pointer :: (type: *Type_Info) -> *Type_Info {
    if type.type == .POINTER {
        // TODO: do something about multiple indirections case?
        return type.(*Type_Info_Pointer).pointer_to;
    }
    return type;
}