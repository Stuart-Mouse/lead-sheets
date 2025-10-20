
/*
    TODO: Constant evaluation
        we could mark certain node types as constants and do constant evaluation where applicable
        this would not be something we really want to do on initial pass, since we don't want to modify the AST that much
        but it would certainly be a good idea to do before going to bytecode
*/

Typechecking_Flags :: enum_flags {
    NONE                    :: 0;
    // EXPECT_LVALUE           :: 1 << 0;
    IS_STATEMENT_ROOT       :: 1 << 1;
}

typecheck_node_or_return :: (
    script:         *Script, 
    node:           *Node, 
    check_flags:    Typechecking_Flags = .NONE,
    hint_type:      *Type_Info         = null
) -> (
    value_type:     *Type_Info
) #expand {
    result := script.my_typecheck_node(script, node, check_flags, hint_type);
    if has_error(script) then `return;
    assert(node.flags & .TYPECHECKED == .TYPECHECKED, "Node was not flagged as typechecked, but no error was set!");
    assert(node.value_type != null, "Node returned null value_type, but no error was set!");
    return result;
}

typecheck_node :: (
    using script:   *Script, 
    node:           *Node, 
    check_flags:    Typechecking_Flags = .NONE,
    hint_type:      *Type_Info         = null
) -> (
    value_type:     *Type_Info
) {
    dprint("typechecking % (%) @ %", node.node_type, node, get_location(node));
    dprint("expecting type: %", as_type(hint_type));
    // dprint("%", sprint_node(script, node));
    dprint_push_indent();
    
    // if already typechecked, just return resolved type
    // this was not possible before, but will be now that multiple identifier nodes may reference same underlying procedure or variable node
    if node.flags & .TYPECHECKED {
        assert(node.value_type != null);
        return node.value_type;
    }
    
    // weird special case for assignment operators. needs to be done up top so we can remove the flag right away.
    if node.flags & .MUST_BE_STATEMENT_ROOT {
        if !(check_flags & .IS_STATEMENT_ROOT) {
            set_typecheck_error(script, "This expression '%' can only be used as the root of a statement, not just as an arbitrary expression.", sprint_node(script, node,, temp), node = node);
            return null;
        }
    }
    check_flags &= ~.IS_STATEMENT_ROOT;
    
    if node.node_type == {
      case Node_Directive;       typecheck_directive       (script, xx node, check_flags, hint_type);
      case Node_Literal;         typecheck_literal         (script, xx node, check_flags, hint_type);
      case Node_Identifier;      typecheck_identifier      (script, xx node, check_flags, hint_type);
      case Node_Operation;       typecheck_operation       (script, xx node, check_flags, hint_type);
      case Node_Procedure_Call;  typecheck_procedure_call  (script, xx node, check_flags, hint_type);
      case Node_Cast;            typecheck_cast            (script, xx node, check_flags, hint_type);
      case Node_Dot;             typecheck_dot             (script, xx node, check_flags, hint_type);
      case Node_Subscript;       typecheck_subscript       (script, xx node, check_flags, hint_type);
      case Node_Declaration;     typecheck_declaration     (script, xx node, check_flags, hint_type);
      case Node_Block;           typecheck_block           (script, xx node, check_flags, hint_type);
      case Node_If_Statement;    typecheck_if_statement    (script, xx node, check_flags, hint_type);
      case Node_While_Loop;      typecheck_while_loop      (script, xx node, check_flags, hint_type);
      case Node_For_Loop;        typecheck_for_loop        (script, xx node, check_flags, hint_type);
      case;
        set_typecheck_error(script, "Unhandled node type % in typecheck_node.", node.node_type, node = node);
    }
    if has_error(script)  return null;
    
    node.flags |= .TYPECHECKED;
    return node.value_type;
}

typecheck_script :: (script: *Script) -> bool {
    if script.ast_root == null                  return false;
    if script.ast_root.node_type != Node_Block  return false;
    
    script.my_typecheck_node(script, script.ast_root, .IS_STATEMENT_ROOT);
    if has_error(script)  return false;
    
    script.flags |= .TYPECHECKED;
    return true;
}

// TODO: try to remove the need for 'need_lvalue' and make it so that we can just check the returned node with is_valid_lvalue if need be
//       it appears the only reason we have this in place atm is to prevent using it_index as an lvalue
resolve_declaration_by_identifier :: (script: *Script, identifier: string, scope_block: *Node) -> *Node_Declaration {
    scope := scope_block;
    while scope {
        if scope.node_type == {
          case Node_Block;
            declaration := find_declaration_in_block(script, xx scope, identifier);
            if declaration  return declaration;
            
          case Node_For_Loop;
            declaration := find_iterator_declaration(script, xx scope, identifier);
            if declaration  return declaration;
        }
        scope = scope.scope;
    }
    for script.global_declarations {
        if get_declaration_name(script, it) == identifier {
            return it;
        }
    }
    return null;
}

find_declaration_in_block :: (script: *Script, block: *Node_Block, identifier: string) -> *Node_Declaration {
    for block.statements {
        if it.node_type == Node_Declaration {
            if get_declaration_name(script, xx it) == identifier {
                return xx it;
            }
        }
    }
    return null;
}

find_iterator_declaration :: (script: *Script, for_loop: *Node_For_Loop, identifier: string) -> *Node_Declaration {
    if for_loop.it_decl && get_declaration_name(script, for_loop.it_decl) == identifier {
        return for_loop.it_decl;
    }
    if for_loop.it_index_decl && get_declaration_name(script, for_loop.it_index_decl) == identifier {
        return for_loop.it_index_decl;
    }
    return null;
}


// If some expression is fundamentally just a literal, this procedure will give you that terminal literal node.
// This procedure primarily exists so that we have a consistent way to check if some expresison can be user as a malleable literal.
get_terminal_literal :: (node: *Node) -> *Node_Literal {
    if node.node_type == {
      case Node_Literal;
        return xx node;
        
      case Node_Dot;
        dot := node.(*Node_Dot);
        if dot.right.node_type == {
          case Node_Literal;
            return xx dot.right;
            
          case Node_Identifier;
            identifier := dot.right.(*Node_Identifier);
            if identifier.identifier_type == {
              case .LITERAL;
                return identifier.literal;
            }
        }
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
    dprint("%: Info: created implicit cast from % to %", get_location(node), as_type(_cast.value.value_type), as_type(_cast.implicit_type));
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
    
    // TODO
    if from == xx Any { return 0; }
    if to   == xx Any { return 0; }
    
    return -1;
}

try_implicit_cast :: (script: *Script, node: *Node, type: *Type_Info) -> (ok: bool, replacement: *Node) {
    if node.value_type == type  return true, null;
    
    if is_valid_lvalue(script, node)
    && can_do_implicit_reference(node, type) {
        // TODO: we may need to recurse in order to check or fix up child nodes that didn't previously know they are expected to be an lvalue? 
        node.flags |= .PUSH_BY_POINTER;
        dprint("made implicit cast for implicit reference: %", as_type(type));
        return true, make_implicit_cast(script, node, type);
    }
    
    if node.node_type == Node_Literal {
        if !coerce_literal_type(xx node, type) {
            set_typecheck_error(script, "Unable to coerce literal of type '%' to '%'", as_type(node.value_type), as_type(type), node = node, type = .FAILED_IMPLICIT_CAST);
            return false, null;
        }
        return true, null;
    }
    
    if get_implicit_cast_distance(node, type) >= 0 {
        return true, make_implicit_cast(script, node, type);
    }
    
    set_typecheck_error(script, "Unable to implicitly cast type '%' to '%'", as_type(node.value_type), as_type(type), node = node, type = .FAILED_IMPLICIT_CAST);
    return false, null;
}

// we just reach into the literal and re-type it in place
coerce_literal_type :: (literal: *Node_Literal, type: *Type_Info) -> bool {
    if literal.value_type == type  return true;
    if literal.literal_type == {
      case .NUMBER;
        if !is_numeric_type(type)  return false;
        new_number := Any_Number.zero_of_type(type);
        Convert.any_to_any(to_any(*new_number), to_any(*literal.number));
        literal.number = new_number;
        literal.value_type = type;
        return true;
    }
    dprint("%: Error: unable to coerce % literal of type '%' to '%'", get_location(literal), literal.literal_type, as_type(literal.value_type), as_type(type));
    return false;
}

get_cast_result_type :: (_cast: *Node_Cast) -> *Type_Info {
    return ifx _cast.flags & .IMPLICIT then _cast.implicit_type else _cast.type_identifier.type_info_pointer;
}

can_do_implicit_reference :: (node: *Node, type: *Type_Info) -> bool {
    ptr_info := type.(*Type_Info_Pointer);
    if ptr_info.type != .POINTER return false;
    
    if ptr_info.pointer_to == node.value_type  return true;
    
    // NOTE: when implementing the below, need to check that #as only works on the first member
    // if is_using_as(node.value_type, ptr_info.pointer_to) return true;   // TODO
    
    return false;
}

// NOTE: if this returns false, we have an error
set_as_lvalue :: (script: *Script, node: *Node) -> bool {
    return script.my_is_valid_lvalue(script, node, set_as_lvalue = true);
}

// TODO: maybe add a unique error type for INVALID_LVALUE
// NOTE: if need_lvalue is true, then we must set the script error if the node is not a valid lvlaue
is_valid_lvalue :: (script: *Script, node: *Node, need_lvalue := false, set_as_lvalue := false) -> bool {
    need_lvalue ||= set_as_lvalue;
    if set_as_lvalue then node.flags |= .PUSH_BY_POINTER;
    
    if node.node_type == {
      case Node_Literal;
        literal := node.(*Node_Literal);
        if (literal.flags & .IS_MALLEABLE) || literal.literal_type == .ANY {
            return true;
        }
        if need_lvalue then set_typecheck_error(script, "A literal cannot be used as an lvalue, unless it is marked as being malleable with `?`.", node = node);
        return false;
        
      case Node_Identifier;
        identifier := node.(*Node_Identifier);
        if #complete identifier.identifier_type == {
          case .UNRESOLVED;         
            if need_lvalue then set_typecheck_error(script, "An unresolved identifier cannot be used as an lvalue.", node = node);
            return false;
            
          case .TYPE;               
            if need_lvalue then set_typecheck_error(script, "A type identifier cannot be used as an lvalue.", node = node);
            return false;
            
          case .EXTERNAL_PROCEDURE; 
            if need_lvalue then set_typecheck_error(script, "An external procedure cannot be used as an lvalue.", node = node);
            return false;
            
          case .STRUCT_MEMBER;
            return true;
            
          case .EXTERNAL_VARIABLE;
            variable := *script.variables[identifier.index];
            if variable.flags & .CONSTANT {
                if need_lvalue then set_typecheck_error(script, "An external variable cannot be used as an lvalue when marked as constant.", node = node);
                return false;
            }
            if variable.flags & .READONLY {
                if need_lvalue then set_typecheck_error(script, "An external variable cannot be used as an lvalue when marked as readonly.", node = node);
                return false;
            }
            return true;
            
          case .LITERAL;
            return script.my_is_valid_lvalue(script, identifier.literal, need_lvalue, set_as_lvalue);
            
          case .DECLARATION;
            // TODO: update this logic for the new semantics around constant declarations and macros
            // if (identifier.declaration.flags & .IS_CONSTANT) 
            // && !declaration_is_malleable_literal(identifier.declaration) {
            //     return false, "Error: Attempting to use an identifier as an lvalue which refers to a constant declaration which is not malleable.";
            // }
            // if identifier.declaration.flags & .CANNOT_BE_USED_AS_LVALUE {
            //     if need_lvalue then set_typecheck_error(script, "The declaration '%' cannot be used as an lvalue.", get_declaration_name(script, identifier.declaration), node = node);
            //     return false;
            // }
            return true;
        }
        
      case Node_Dot;
        dot := node.(*Node_Dot);
        // TODO: make sure that this covers all our cases properly
        if dot.left.value_type.type == .STRUCT {
            return script.my_is_valid_lvalue(script, dot.left, need_lvalue, set_as_lvalue)
                && script.my_is_valid_lvalue(script, dot.right, need_lvalue);
        }
        return script.my_is_valid_lvalue(script, dot.right, need_lvalue);
        
      case Node_Subscript;          
        subscript := node.(*Node_Subscript);
        return script.my_is_valid_lvalue(script, subscript.base_expression, need_lvalue);
        
      case Node_Cast;
        _cast := node.(*Node_Cast);
        if _cast.value.value_type == xx Any {
            return script.my_is_valid_lvalue(script, _cast.value, need_lvalue, set_as_lvalue);
        }
        if need_lvalue then set_typecheck_error(script, "Using the result of a cast as an lvalue is not currently supported, unless casting from an Any", node = node);
        return false;
        
      // NOTE: it is possible these nodes may be able to produce valid lvalues in the future
      case Node_Operation;
        if need_lvalue then set_typecheck_error(script, "Using the result of an operation as an lvalue is not currently supported", node = node);
        return false;
        
      case Node_Procedure_Call;     
        if need_lvalue then set_typecheck_error(script, "Using the result of a procedure call as an lvalue is not currently supported", node = node);
        return false;
        
      // TODO: These cases really should not even remotely happen... so maybe we should not even bother with an error message?
      case Node_Declaration;        
        if need_lvalue then set_typecheck_error(script, "A declaration cannot be used as an lvalue", node = node);
        return false;
      case Node_Block;              
        if need_lvalue then set_typecheck_error(script, "A block cannot be used as an lvalue", node = node);
        return false;
      case Node_If_Statement;       
        if need_lvalue then set_typecheck_error(script, "An if statement cannot be used as an lvalue", node = node);
        return false;
      case Node_While_Loop;         
        if need_lvalue then set_typecheck_error(script, "A while loop cannot be used as an lvalue", node = node);
        return false;
      case Node_For_Loop;           
        if need_lvalue then set_typecheck_error(script, "A for loop cannot be used as an lvalue", node = node);
        return false;
    }
    
    assert(false, "Invalid node type '%' in is_valid_lvalue.", node.node_type);
    return false;
}



/*
    Separate sub-routines for typechecking specific node types.
    These are broken out so that Jai does not allocate a ridiculous stack frame size for typecheck_node, limiting the maximum recursion depth to like, 10. 
    
    Since we are doing this, we may as well refactor the main typecheck_node procedure to be a bit clearer.
    One thinkg that's always been a bit weird about it is that we need to not only set the value type on the node, but also return that same type info as the return value
    we could just get rid of that return value an return only an error object, which we can just declare at the top of the proceudre much like the result object we were using before
    Then if we still want to have the same semantics of getting a return type back on typecheck_node, we can do so only from that proc by explicitly returing the node.value_type
        which will prevent there ever being a mismatch betweenwhat is returned and what is set on the node
*/

typecheck_directive :: (using script: *Script, directive: *Node_Directive, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    directive.value_type = type_info(void); // by default
    
    // we always call the directive a second time during typechecking, 
    // even if the directive already returned some replacement node during parsing
    if !evaluate_directive(script, directive, .TYPECHECK) {
        builder: String_Builder;
        print(*builder, "Failed to evaluate directive % in typechecking phase. Evaluation error:\n", directive.name);
        format_error(*builder, script, false, true);
        set_typecheck_error(script, builder_to_string(*builder,, temp), node = directive);
        return;
    }
    if directive.runtime_node {
        directive.value_type = typecheck_node_or_return(script, directive.runtime_node, check_flags, hint_type);
    }
}

typecheck_literal :: (using script: *Script, literal: *Node_Literal, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    if #complete literal.literal_type == {
      case .ANY;
        literal.value_type = literal.any.type;
        
      case .STRING;
        literal.value_type = xx string;
        literal.flags |= .IS_CONSTANT;
        
      case .BOOLEAN;
        literal.value_type = literal.number.type;
        literal.flags |= .IS_CONSTANT;
        
      case .NUMBER;
        if is_numeric_type(hint_type) {
            // NOTE: we now remap to value_type just once here, so that we don't have to do it each time in execute
            // TODO: think about rules around automatic casting here...
            //       maybe we don't want to automatically go float -> int, 
            //       since we will now only get float here when the literal had a decimal part
            new_number := Any_Number.zero_of_type(hint_type);
            Convert.any_to_any(to_any(*new_number), to_any(*literal.number));
            literal.number = new_number;
        } else {
            // NOTE: also doing a little remap here in default case, 
            //       since we actually want the default float type to be float32
            if literal.number.type == xx float64 {
                new_number := Any_Number.zero_of_type(float32);
                Convert.any_to_any(to_any(*new_number), to_any(*literal.number));
                literal.number = new_number;
            }
        }
        literal.value_type = literal.number.type;
        literal.flags |= .IS_CONSTANT;
        
      case .STRUCT;
        ti_struct := hint_type.(*Type_Info_Struct);
        if ti_struct == null {
            set_typecheck_error(script, 
                "Unable to typecheck struct literal without a type hint.", 
                type = .MISSING_TYPE_HINT, 
                node = literal
            );
            return;
        }
        if ti_struct.type != .STRUCT {
            set_typecheck_error(script, "Type hint provided to struct literal is not a struct type. (was '%')", as_type(ti_struct), node = literal);
            return;
        }
        
        all_members_are_constant := true;
        for literal.aggr.expressions {
            if it_index >= ti_struct.members.count {
                set_typecheck_error(script, "Too many values provided in struct literal.", node = literal);
                return;
            }
            member := ti_struct.members[it_index];
            
            expr_ti := typecheck_node_or_return(script, it, hint_type = member.type); 
            if expr_ti != member.type {
                set_typecheck_error(script, "Value type provided in struct initializer for field '%' was incorrect. Expected '%', got '%'.", member.name, as_type(member.type), as_type(expr_ti), node = it);
                return;
            }
            
            all_members_are_constant &&= (it.flags & .IS_CONSTANT).(bool);
        }
        
        if all_members_are_constant {
            literal.flags |= .IS_CONSTANT;
        }
        
        literal.value_type = ti_struct;
        
        // TODO: we should not always allocate for a literal aggr. Only need to do this if the literal is marked with ?
        if is_malleable(literal) {
            literal.aggr.value_pointer = Dynamic_New(literal.value_type,, get_pool_allocator(*script.pool));
            dprint("allocated space for literal of type %, node %", literal.value_type, literal);
        }
    }
    
    assert(literal.value_type != null);
}

typecheck_identifier :: (using script: *Script, identifier: *Node_Identifier, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    // NOTE: identifier may have already been typechecked e.g. in parameters of procedure call
    //       in this case we obviously don't want to check it again (not to mention accessing name will now be invalid!)
    if identifier.identifier_type != .UNRESOLVED {
        dprint("identifier was not UNRESOLVED");
        return;
    }
    
    declaration := resolve_declaration_by_identifier(script, identifier.name, identifier.scope);
    if declaration {
        if declaration.value_type == null {
            set_typecheck_error(script, "Identifier '%' used before its declaration.", identifier.name, node = identifier);
            return;
        }
        // TODO: we cannot mark the identifier as constant based on the declaration yet, 
        //       since the declaration may later be referenced by another identifier as an lvalue, making the declaration non-constant
        //       This is fine since we don't really do any constant evaluation on the AST, but when we lower to bytecode, 
        //       we will need to consider that a DECLARATION identifier may yet need to be checked for constancy.
        // TODO: if the declaration is a macro though, and the init_expression of the macro is constant, then we can still go ahead and safely mark the identifier as constant
        //       this may be a good idea to go ahead and implement since it would allow us to use such an identifier in a type expression.
        if declaration.flags & .IS_CONSTANT {
            // TODO: probably fix this case, it will matter eventually
            // if check_flags & .EXPECT_LVALUE {
            //     dprint("removing IS_CONSTANT flag from declaration '%' because its identifier is being used as an lvalue", get_declaration_name(script, declaration));
            //     declaration.flags &= ~.IS_CONSTANT;
            // }
            // if declaration.flags & .MACRO {
            //     identifier.flags |= .CONSTANT;
            // }
        }
        
        identifier.identifier_type = .DECLARATION;
        identifier.declaration     = declaration;
        identifier.value_type      = declaration.value_type;
        return;
    }
    
    // TODO: maybe we should add some checks to make sure that all externally inserted identifiers don't conflict across categories
    //       for example, we should not have a type identifier which conflicts with an external variable or procedure
    
    for type_table {
        if it.name == identifier.name {
            identifier.flags |= .IS_CONSTANT;   // the type table is assumed to be constant during the script's lifetime
            identifier.identifier_type = .TYPE;
            identifier.index           = it_index;
            identifier.value_type      = xx Type;
            return;
        }
    }
    
    for variables {
        if it.name == identifier.name {
            identifier.identifier_type = .EXTERNAL_VARIABLE;
            identifier.index           = it_index;
            identifier.value_type      = ifx it.flags & .ANY then xx Any else variables[it_index].type;
            return;
        }
    }
    
    // NOTE: we don't attach an error message here
    set_typecheck_error(script, "", type = .UNRESOLVED_IDENTIFIER, node = identifier);
    return;
}


typecheck_operation :: (using script: *Script, operation: *Node_Operation, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    if operation.directive {
        typecheck_node_or_return(script, operation.directive, check_flags, hint_type);
        if operation.directive.runtime_node  return;
    }
    
    left_type, right_type: *Type_Info;
    
    _operator := get_operator(script, operation);
    if _operator.kind == {
      case .PREFIX; #through;
      case .POSTFIX;
        left_type = typecheck_node_or_return(script, operation.left, check_flags, hint_type);
        
      case .BINARY;
        // Binary operation checking is a bit more complicated, because either side may require a type hint from the other.
        left_type  = my_typecheck_node(script, operation.left,  check_flags, hint_type);
        right_type = my_typecheck_node(script, operation.right, check_flags, hint_type);
        
        // If both come back null on first try, we return the upstream error.
        if left_type == null && right_type == null {
            assert(has_error(script));
            return;
        }
        clear_error(script);
        
        // Otherwise, try to typecheck whichever one previously failed with the type hint of the other side.
        if left_type == null {
            dprint("Info: Failed to typecheck left operand on first attempt, trying again with type hint: %", as_type(right_type));
            left_type  = typecheck_node_or_return(script, operation.left,  check_flags, right_type);
        }
        if right_type == null {
            dprint("Info: Failed to typecheck right operand on first attempt, trying again with type hint: %", as_type(left_type));
            right_type = typecheck_node(script, operation.right, check_flags, left_type);
        }
        
      case .ASSIGNMENT;
        // NOTE: we ignore hint_type because assignment expressions must be used at statement-level anyhow.
        left_type  = typecheck_node_or_return(script, operation.left, check_flags, null);
        if !set_as_lvalue(script, operation.left)  return;
        
        right_type = typecheck_node_or_return(script, operation.right, check_flags, left_type);
        
        // special case: operator = is always a simple memcpy
        if operation.name == "=" {
            if left_type != right_type {
                ok, replacement := try_implicit_cast(script, operation.right, left_type); 
                if !ok  return;
                if replacement  replace_node(*operation.right, replacement);
            }
            operation.value_type = xx void;
            return;
        }
    }
    
    // TODO: We could still use some smarter type coercion here, probably. 
    //       Will have to see what specific cases come up later and try to fix those.
    left\_underlying_type := ifx left\_type && left\_type.type == .ENUM then left\_type.(*Type_Info_Enum).internal_type.(*Type_Info) else left\_type;
    right_underlying_type := ifx right_type && right_type.type == .ENUM then right_type.(*Type_Info_Enum).internal_type.(*Type_Info) else right_type;
    
    builtin_operation_index, swap_args := resolve_builtin_operation(operation.name, _operator.kind, left_underlying_type, right_underlying_type);
    if builtin_operation_index >= 0 {
        operation.builtin_operation_index = builtin_operation_index;
        
        if swap_args then operation.flags |= .SWAP_OPERANDS;
        
        // NOTE: only uses the enum type if both left and right enums match, 
        //       otherwise we set return type as the matching underlying type.
        operation.value_type = BUILTIN_OPERATIONS[builtin_operation_index].result.(*Type_Info);
        if left_type.type == .ENUM && left_type == right_type {
            if operation.value_type != left_underlying_type {
                set_typecheck_error(script, "We tried to coerce the enum type '%' to its underlying int type '%' for the purposes of making this binary operation work, but the return type of the resolved binary operator '%' does not match the underlying int type of the enum. So unfortunately we're gonna have to fail typechecking here...", as_type(left_type), as_type(left_underlying_type), as_type(operation.value_type), node = operation);
                return;
            }
            operation.value_type = left_type;
        }
        return;
    }
    
    // if operation was not a builtin, then we will try to resolve the operation as an overloaded procedure
    procedure_call := alloc_node(script, Node_Procedure_Call);
    procedure_call.procedure_expression = make_identifier(script, operation.name);
    
    arguments_count := ifx is_unary(_operator) then 1 else 2;
    procedure_call.arguments = array_copy(array_view((*Node).[ operation.left, operation.right ], 0, arguments_count),, get_pool_allocator(*script.pool));
    
    operation.overload_procedure    = procedure_call;
    operation.flags                |= .OVERLOAD;
    
    operation.value_type = typecheck_node_or_return(script, procedure_call);
}

typecheck_procedure_call :: (using script: *Script, procedure_call: *Node_Procedure_Call, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    // TODO: we should probably be more rigorous about how we filter check flags as we traverse the AST
    //       here, for instance, we could use some constnat like PASSTHROUGH_FLAGS_FOR_PROCEDURE_EXPRESSION and | with that 
    //       as it stands though, we don't actually have many check flags and maybe even the ones we do have could possibly be removed if we refactored some things...
    //       So, here is my little note in an obscure place where I will probably just forget about it and move on
    proc_info := my_typecheck_node(script, procedure_call.procedure_expression).(*Type_Info_Procedure);
    
    if script.error.type == .UNRESOLVED_IDENTIFIER 
    && procedure_call.procedure_expression.node_type == Node_Identifier {
        clear_error(script);
        
        identifier := procedure_call.procedure_expression.(*Node_Identifier);
        assert(identifier.identifier_type == .UNRESOLVED); // so we can access name member validly
        
        // Procedure arguments are sort of typechecked twice, once before selecting an overload and once after.
        // In the first check, we do not provide any type hints, which means we may fail to typecheck arguments where the type hint is required.
        // But we ignore this error on the first pass, since we may be able to provide a type hint later after selecting an overload.
        for procedure_call.arguments {
            my_typecheck_node(script, it);
            if script.error.type == .MISSING_TYPE_HINT  clear_error(script);
            if has_error(script)  return;
        }
        
        procedure_name  := identifier.name;
        procedure_index := resolve_procedure_index(script, procedure_name, procedure_call);
        if procedure_index == -1  return;
        
        identifier.identifier_type = .EXTERNAL_PROCEDURE;
        identifier.index           = procedure_index;
        identifier.flags          |= .TYPECHECKED;
        proc_info:, identifier.value_type = procedures[procedure_index].proc_info;
        
        // For the second pass over the arguments, we go back and verify the types for all arguments, re-typechecking those that may have failed due to a missing type hint.
        // We also iterate more manually here so that we can replace nodes if we need to insert any implicit casts, since we need a **Node for that.
        for 0..procedure_call.arguments.count-1 {
            node_ptr_ptr  := *procedure_call.arguments[it];
            node          := node_ptr_ptr.*;
            expected_type := proc_info.argument_types[it];
            if node.value_type == null {
                dprint("re-typechecking argument with hint_type %", as_type(expected_type));
                type := typecheck_node_or_return(script, node, hint_type = expected_type);
                if type != expected_type {
                    builder: String_Builder;
                    print(*builder, "We deferred typechecking a procedure argument in the hopes that we could later hint the type and resolve it that way, but that failed.\n");
                    print(*builder, "\tProcedure identifier: '%'\n", procedure_name);
                    print(*builder, "\tArgument expression: '%'\n", sprint_node(script, node,, temp));
                    print(*builder, "\tExpected type '%', got '%'\n", expected_type, type);
                    set_typecheck_error(script, builder_to_string(*builder,, temp), node = node, type = .MISSING_TYPE_HINT);
                    return;
                }
            } else {
                ok, replacement := try_implicit_cast(script, node, expected_type);
                if !ok {
                    builder: String_Builder;
                    print(*builder, "Failed while trying to implicitly cast a procedure argument.\n");
                    print(*builder, "\tProcedure identifier: '%'\n", procedure_name);
                    print(*builder, "\tArgument expression: '%'\n", sprint_node(script, node,, temp));
                    print(*builder, "\tExpected type '%', got '%'\n", expected_type, node.value_type);
                    copy_from_string(*script.error.message, builder_to_string(*builder,, temp));
                    return;
                }
                if replacement  replace_node(node_ptr_ptr, replacement);
            }
        }
        
        procedure_call.value_type = ifx proc_info.return_types 
            then proc_info.return_types[0] 
            else type_info(void);
    }
    // procedure was the result of some larger expression, so it already has a strict type
    else {
        if proc_info == null {
            set_typecheck_error(script, "Failed to typecheck procedure_expression.", node = procedure_call);
            return;
        }
        if proc_info.type != .PROCEDURE {
            set_typecheck_error(script, "Attempt to call a non-procedure type as if it were a procedure.", node = procedure_call);
            return;
        }
        if proc_info.return_types.count > 1 {
            set_typecheck_error(script, "Attempt to call a procedure which returns more than one value. This is not currently supported.", node = procedure_call);
            return;
        }
        
        // typecheck arguments. we know what they must be, so we just straightforwardly match, and can hint type trivially
        if proc_info.argument_types.count != procedure_call.arguments.count {
            set_typecheck_error(script, "Incorrect number of arguments provided for procedure call. Expected %, got %.\n", proc_info.argument_types.count, procedure_call.arguments.count, node = procedure_call);
            return;
        }
        for procedure_call.arguments {
            expected_type := proc_info.argument_types[it_index];
            argument_type := typecheck_node_or_return(script, it, hint_type = expected_type);
            if argument_type != expected_type {
                set_typecheck_error(script, "Mismatched types on argument % for procedure call. Expected %, got %.\n", it_index, as_type(expected_type), as_type(argument_type), node = it);
                return;
            }
        }
        
        procedure_call.value_type = ifx proc_info.return_types 
            then proc_info.return_types[0] 
            else type_info(void);
    }
    
    assert(procedure_call.value_type != null);
}

typecheck_cast :: (using script: *Script, _cast: *Node_Cast, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    value_type := typecheck_node_or_return(script, _cast.value);
    
    if _cast.flags & .IMPLICIT {
        _cast.value_type = _cast.implicit_type;
        if is_aggregate(_cast.value.value_type) {
            assert(is_pointer_to(_cast.value_type, _cast.value.value_type), "Implicit cast with aggregate value was not an implicit reference.");
        }
    } else {
        if is_aggregate(_cast.value.value_type) && _cast.value.value_type != xx Any {
            set_typecheck_error(script, "No explicit casts are permitted on aggregate types at this time.", type = .INVALID_CAST, node = _cast);
        }
        
        type_expression := _cast.type_expression;
        type_type := typecheck_node_or_return(script, type_expression, hint_type = xx Type);
        
        if type_type.type != .TYPE {
            set_typecheck_error(script, "Type expression in cast did not resolve to a Type.", node = type_expression);
            return;
        }
        if !(type_expression.flags & .IS_CONSTANT) {
            set_typecheck_error(script, "Type expression in cast must be constant.", node = type_expression);
            return;
        }
        
        my_evaluate_node(script, type_expression);
        if has_error(script)  return;
        _cast.value_type = pop_as(*stack, *Type_Info);
    }
    
    return;
}

typecheck_dot :: (using script: *Script, dot: *Node_Dot, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    if dot.flags & .IS_ARROW {
        set_typecheck_error(script, "Arrow is not currently implemented.", node = dot);
        return;
    }
    
    // Currently, the only valid case for dot.left being null is for unary dot enums.
    // Setting left_type to 'Type' when dot.left is null gets us into the proper case down below.
    left_type: *Type_Info;
    if dot.left {
        left_type = typecheck_node_or_return(script, dot.left);
    } else {
        left_type = xx Type;
    }
    
    if left_type.type == {
      case .POINTER;
        // if left type is a pointer to a struct, override left_type with the type info for struct being pointed at
        // this is to allow implicit dereference on pointers to structs for member access
        pointer_info := left_type.(*Type_Info_Pointer);
        if pointer_info.pointer_to.type != .STRUCT {
            set_typecheck_error(script, "If left side of struct member dot dereference expression is a pointer, then it must point to a struct type.", node = dot.left);
            return;
        }
        struct_info := pointer_info.pointer_to.(*Type_Info_Struct);
        left_type = struct_info; // override for below struct case
        
        #through;
        
      case .STRUCT;
        if dot.right.node_type != Node_Identifier {
            set_typecheck_error(script, "Right side of struct member dot dereference expression must be an identifier.", node = dot.right);
            return;
        }
        
        member_identifier := dot.right.(*Node_Identifier);
        assert(member_identifier.node_type == Node_Identifier);
        assert(member_identifier.identifier_type == .UNRESOLVED);
        member_name := member_identifier.name;
        
        member_ti, member_offset := get_field(xx left_type, member_name);
        if member_ti == null {
            set_typecheck_error(script, "Attempt to access struct member which does not exist.", node = dot.right);
            return;
        }
        
        // tricky case. left may be some complex expression where the result is an instance of a struct type.
        // so we can't just set the left type to be a namespace.
        //      but we do still want to do this if left type is an identifier? or no, since it could be a type identifier? which maybe we want to leave as is?
        if member_ti.flags & .CONSTANT {
            set_typecheck_error(script, "Access of constant struct members is not yet allowed.", node = dot.right);
            return;
            // member_identifier.identifier_type = .LITERAL;
            // member_identifier.literal = alloc_node(script, Node_Literal);
            // TODO: need to be able to call some simple make_literal() proc here and actually init the literal value
        } else {
            member_identifier.identifier_type = .STRUCT_MEMBER;
            member_identifier.member = member_ti;
        }
        
        member_identifier.value_type = member_ti.type;
        member_identifier.flags |= .TYPECHECKED;
        dot.value_type = member_ti.type;
        
        // NOTE: If we are not implicitly dereferencing the left side here,
        //       then we would prefer for the value to be pushed by pointer.
        //       However, this may not be possible if the LHS is not a valid lvalue,
        //       and in that case, we still should be able to access its members.
        if dot.left.value_type.type != .POINTER { 
            if !set_as_lvalue(script, dot.left) {
                // clear_error(script);
                return;
            }
        }
        return;
        
      // case .ENUM;
        // TODO: access enum namespace through instance of enum type
        
      case .TYPE;
        namespace: *Type_Info;
        if dot.left {
            // TODO: make sure that type expression is constant before evaluating it
            if !(dot.left.flags & .IS_CONSTANT) {
                set_typecheck_error(script, "Type expression on left-hand side of dot operator must be constant.", node = dot);
                return;
            }
            
            my_evaluate_node(script, dot.left);
            if has_error(script)  return;
            namespace = pop_as(*stack, *Type_Info);
        } else {
            if hint_type == null || (hint_type.type != .ENUM && hint_type.type != .STRUCT) {
                set_typecheck_error(script, "Unary dot can only be used for enums, but hint_type was %", as_type(hint_type), type = .MISSING_TYPE_HINT, node = dot);
                return;
            }
            namespace = hint_type;
        }
        
        if namespace.type == {
          case .STRUCT;
            if dot.right.node_type == {
              case Node_Literal;
                right_type := typecheck_node_or_return(script, dot.right, hint_type = namespace);
                if namespace != right_type {
                    set_typecheck_error(script, "Type returned by right side of struct literal expression did not match type specified on left side of dot.", node = dot.right);
                    return;
                }
                dot.value_type = namespace;
                return; // no error
                
              // case .Node_Identifier;
            }
            
            /*
                Do we want to support use of non-constant struct members on a struct type if the eventual result is some constant literal?
                for example: `Some_Struct.some_member_thats_an_anonymous_enum.SOME_ENUM_VALUE`
                in order to do this kind of thing, we need to pass some flags back up when we return from typecheck node, e.g. IS_CONSTANT
                but also, not exactly, since we need to tell the first in chain about the last
                so we would need to peek dot.right for constancy, then tell dot.left about that in the flags we pass up to that node
                idk, this is all very strange... may just say fuck it and not support this.
            */
            set_typecheck_error(script, "Use of struct type as a namespace is not yet implemented!", node = dot);
            return;
            
          case .ENUM;
            ti_enum := namespace.(*Type_Info_Enum);
            
            right := dot.right.(*Node_Identifier);
            assert(right.node_type == Node_Identifier && right.identifier_type == .UNRESOLVED);
            member_name := right.name;
            
            value, found := enum_name_to_value(ti_enum, member_name);
            if !found {
                set_typecheck_error(script, "Invalid enum value name '%' for enum type '%'.", member_name, as_type(ti_enum), node = dot);
                return;
            }
            
            right.identifier_type = .LITERAL;
            right.literal         = make_literal(script, Any.{ ti_enum, *value });
            right.literal.flags  |= .TYPECHECKED;
            right.value_type      = ti_enum;
            right.flags          |= .TYPECHECKED;
            
            dot.value_type = ti_enum;
            return; // no error
            
          case;
            set_typecheck_error(script, "A % type cannot be used as a namespace.", namespace.type, node = dot);
            return;
        }
    }
    
    set_typecheck_error(script, "Left side of dot expression must be a struct or type. Got: %", as_type(dot.left.value_type), node = dot);
    return;
}

typecheck_subscript :: (using script: *Script, subscript: *Node_Subscript, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    typecheck_node_or_return(script, subscript.base_expression);
    if !set_as_lvalue(script, subscript.base_expression)  return; // TODO
    
    base_type := subscript.base_expression.value_type.(*Type_Info_Array);
    if base_type.type != .ARRAY {
        set_typecheck_error(script, "Type '%' cannot be indexed!", as_type(base_type), node = subscript);
        return;
    }
    
    indexing_type := typecheck_node_or_return(script, subscript.indexing_expression, hint_type = xx int);
    
    ok, replacement := try_implicit_cast(script, subscript.indexing_expression, xx int);
    if !ok {
        set_typecheck_error(script, "Type '%' cannot be used in an indexing expression!", as_type(indexing_type), node = subscript);
        return;
    }
    if replacement  replace_node(*subscript.indexing_expression, replacement);
    
    subscript.value_type = base_type.element_type;
    return;
}

typecheck_declaration :: (using script: *Script, declaration: *Node_Declaration, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    // NOTE: we currently only allow declarations to be used as statements, so we flag them all with .VALUE_TYPE_IS_NOMINAL
    //       this currently serves no funcitonal purpose other than to make our assertions happy in evaluate_node_or_return 
    declaration.flags |= .VALUE_TYPE_IS_NOMINAL;
    
    explicit_type: *Type_Info;
    if declaration.type_expression {
        type_expression := declaration.type_expression;
        type_type := typecheck_node_or_return(script, type_expression, hint_type = xx Type);
        if type_type.type != .TYPE {
            set_typecheck_error(script, "Type expression in declaration did not resolve to a Type.", node = declaration);
            return;
        }
        // TODO: make sure that type expression is constant before evaluating it
        if !(type_expression.flags & .IS_CONSTANT) {
            set_typecheck_error(script, "Type expression in declaration must be constant.", node = declaration);
            return;
        }
        
        my_evaluate_node(script, type_expression);
        if has_error(script)  return;
        explicit_type = pop_as(*stack, *Type_Info);
    }
    
    right_type: *Type_Info;
    if declaration.init_expression {
        right_type = typecheck_node_or_return(script, declaration.init_expression, hint_type = explicit_type);
        
        if explicit_type && explicit_type != right_type {
            set_typecheck_error(script, "Init expression of declaration does not match given type. Expected '%', got '%'.", explicit_type, right_type, node = declaration);
            return;
        }
        // if the init expression of a declaration is marked as constant, then the declaration is also marked as constant
        // however, if the declaration is later re-assigned to a new value, the declaration will lose this designation
        if declaration.init_expression.flags & .IS_CONSTANT {
            declaration.flags |= .IS_CONSTANT;
        }
    } else {
        // TODO: can we validly assume that default-initialized values are essentially constant?
        // assert(explicit_type != null);
        // declaration.flags |= .IS_CONSTANT;
    }
    
    if declaration.left.node_type == Node_Identifier {
        identifier := declaration.left.(*Node_Identifier);
        assert(identifier.identifier_type == .UNRESOLVED);
        
        other_declaration := resolve_declaration_by_identifier(script, identifier.name, declaration.scope);
        if other_declaration != null && declaration != other_declaration {
            set_typecheck_error(script, "Redeclaration of identifier '%'.", identifier.name, node = declaration);
            return;
        }
    } else {
        set_typecheck_error(script, "Left-hand side of declaration must be a simple identifier.", node = declaration);
        return;
    }
    
    dprint("explicit_type: %", explicit_type);
    dprint("right_type: %", right_type);
    
    declaration.value_type = ifx explicit_type else right_type;
    
    if declaration.flags & .MACRO {
        // NOTE: currently, macro declarations are only used to bind to malleable literals
        literal := get_terminal_literal(declaration.init_expression);
        if !literal || !is_malleable(literal) {
            set_typecheck_error(script, "Macro declarations can only be made to simple literals at this time.", node = declaration);
            return;
        }
        literal.flags |= .IS_MALLEABLE;
        
        declaration.value_pointer = get_literal_value_as_any(literal).value_pointer;
        my_evaluate_node(script, declaration.init_expression);
        if has_error(script)  return;
        pop_any(*script.stack, declaration.init_expression.value_type, should_push_by_pointer(declaration.init_expression));
    } else {
        declaration.stack_offset = get_stack_space(script, declaration.value_type.runtime_size, declaration.scope);
    }
    
    return;
}

typecheck_block :: (using script: *Script, block: *Node_Block, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    for block.statements {
        typecheck_node_or_return(script, it, .IS_STATEMENT_ROOT);
    }
    block.value_type = xx void;
    return;
}

typecheck_if_statement :: (using script: *Script, if_statement: *Node_If_Statement, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    typecheck_node_or_return(script, if_statement.condition); // TODO: must be bool or coercible to bool
    typecheck_node_or_return(script, if_statement.statement, .IS_STATEMENT_ROOT);
    
    if_statement.value_type = type_info(void);
    return;
}

typecheck_while_loop :: (using script: *Script, while_loop: *Node_While_Loop, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    typecheck_node_or_return(script, while_loop.condition); // TODO: must be bool or coercible to bool
    typecheck_node_or_return(script, while_loop.statement, .IS_STATEMENT_ROOT);
    
    while_loop.value_type = type_info(void);
    return;
}

typecheck_for_loop :: (using script: *Script, for_loop: *Node_For_Loop, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    // NOTE: stack_offset for it and it_index decls get set later when actually iterating for loop
    
    if #complete for_loop.control_type == {
      case .RANGE;
        dprint("typechecking range lower");
        lower_type := typecheck_node_or_return(script, for_loop.range.lower, hint_type = xx int);
        {
            ok, replacement := try_implicit_cast(script, for_loop.range.lower, xx int);
            if !ok {
                set_typecheck_error(script, "Value must be of type int or implicitly convertable to int.", node = for_loop.range.lower);
                return;
            }
            if replacement  replace_node(*for_loop.range.lower, replacement);
        }
        
        dprint("typechecking range upper");
        upper_type := typecheck_node_or_return(script, for_loop.range.upper, hint_type = lower_type);
        {
            ok, replacement := try_implicit_cast(script, for_loop.range.upper, xx int);
            if !ok {
                set_typecheck_error(script, "Value must be of type int or implicitly convertable to int.", node = for_loop.range.upper);
                return;
            }
            if replacement  replace_node(*for_loop.range.upper, replacement);
        }
        
        if !for_loop.it_decl {
            for_loop.it_decl      = alloc_node(script, Node_Declaration);
            for_loop.it_decl.left = make_identifier(script, "it");
        }
        for_loop.it_decl.value_type = xx int;
        for_loop.it_decl.flags |= .MACRO; // @HACK: should just push stack space in evaluation
        // for_loop.it_decl.flags |= .CANNOT_BE_USED_AS_LVALUE;
        
        if for_loop.it_index_decl {
            set_typecheck_error(script, "Cannot define an `it_index` in a range-based for loop.", node = for_loop.it_index_decl);
            return;
        }
        
        
      case .ARRAY;
        typecheck_node_or_return(script, for_loop.array_expression);
        // if !set_as_lvalue(script, for_loop.array_expression)  return;
    
        array_info := for_loop.array_expression.value_type.(*Type_Info_Array);
        
        // TODO: special case for something like a Many_Any / Any_Array_View
        // better option than creating new array view type info's dynamically
        if array_info.type != .ARRAY {
            set_typecheck_error(script, "Expected an array type.", node = for_loop.array_expression);
            return;
        }
        
        if !for_loop.it_decl {
            for_loop.it_decl      = alloc_node(script, Node_Declaration);
            for_loop.it_decl.left = make_identifier(script, "it");
        }
        for_loop.it_decl.value_type = array_info.element_type;
        for_loop.it_decl.flags |= .MACRO; // @HACK: should just push stack space in evaluation
        // for_loop.it_decl.flags |= .CANNOT_BE_USED_AS_LVALUE;
        
        if !for_loop.it_index_decl {
            for_loop.it_index_decl      = alloc_node(script, Node_Declaration);
            for_loop.it_index_decl.left = make_identifier(script, "it_index");
        }
        for_loop.it_index_decl.value_type = xx int;
        for_loop.it_index_decl.flags |= .MACRO; // @HACK: should just push stack space in evaluation
        // for_loop.it_index_decl.flags |= .CANNOT_BE_USED_AS_LVALUE;
        
        
      case .LIST;
        first_type := typecheck_node_or_return(script, for_loop.list[0]);
        if !set_as_lvalue(script, for_loop.list[0])  return;
        
        for 1..for_loop.list.count-1 {
            node := for_loop.list[it];
            type := typecheck_node_or_return(script, node);
            if !set_as_lvalue(script, node)  return;
            
            if type != first_type {
                set_typecheck_error(script, "All expressions in comma-separated iteration list must be of the same type. First type was %, type of element % was %", as_type(first_type), it + 2, as_type(type), node = node);
                return;
            }
        }
        
        if !for_loop.it_decl {
            for_loop.it_decl      = alloc_node(script, Node_Declaration);
            for_loop.it_decl.left = make_identifier(script, "it");
        }
        for_loop.it_decl.value_type = first_type;
        for_loop.it_decl.flags |= .MACRO; // @HACK: should just push stack space in evaluation
        // for_loop.it_decl.flags |= .CANNOT_BE_USED_AS_LVALUE;
        
        if !for_loop.it_index_decl {
            for_loop.it_index_decl      = alloc_node(script, Node_Declaration);
            for_loop.it_index_decl.left = make_identifier(script, "it_index");
        }
        for_loop.it_index_decl.value_type = xx int;
        for_loop.it_index_decl.flags |= .MACRO; // @HACK: should just push stack space in evaluation
        // for_loop.it_index_decl.flags |= .CANNOT_BE_USED_AS_LVALUE;
    }
    
    typecheck_node_or_return(script, for_loop.statement, .IS_STATEMENT_ROOT);
    
    for_loop.value_type = xx void;
    return;
}
