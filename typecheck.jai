
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
    IS_PROCEDURE_EXPRESSION :: 1 << 2; // TODO: what are we using this for?
}

// before using this method, we may want to double check that it does explode the stack too much, as I anticipate it may.
// typecheck and maybe propogate error
typecheck_node_or_return :: (
    script:         *Script, 
    node:           *Node, 
    check_flags:    Typechecking_Flags = .NONE,
    hint_type:      *Type_Info         = null
) -> (
    value_type:     *Type_Info
) #expand {
    result := typecheck_node(script, node, check_flags, hint_type);
    if script.error.type != .NO_ERROR then `return null;
    return result;
}

// For internal use, returns a result object for easier internal error handling and reporting.
typecheck_node :: (
    using script:   *Script, 
    node:           *Node, 
    check_flags:    Typechecking_Flags = .NONE,
    hint_type:      *Type_Info         = null
) -> (
    value_type:     *Type_Info
) {
    dprint("typechecking node % % (%)\n", node, node.node_type, get_location(node));
    dprint("expecting type: %\n", as_type(hint_type));
    // dprint("%\n", sprint_node(script, node));
    dprint_push_indent();
    
    // if already typechecked, just return resolved type
    // this was not possible before, but will be now that multiple identifier nodes may reference same underlying procedure or variable node
    if node.flags & .TYPECHECKED {
        assert(node.value_type != null);
        return node.value_type;
    }
    
    // weird special case for assignment operators. needs to be done up top so we can remove the flag right away.
    // TODO: print source code location on error as well. really, we need to support this in many places for better error reporting.
    if node.flags & .MUST_BE_STATEMENT_ROOT {
        if !(check_flags & .IS_STATEMENT_ROOT) {
            set_general_error(script, "%: Error: This expression '%' can only be used as the root of a statement, not just as an arbitrary expression.", get_location(node), sprint_node(script, node,, temp));
            return null;
        }
    }
    check_flags &= ~.IS_STATEMENT_ROOT;
    
    // TODO: callback here to manually typecheck node
    // return type and error separately
    // if type is set, skip the below
    
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
        set_general_error(script, "%: Error: Unhandled node type % in typecheck_node.", get_location(node), node.node_type);
    }
    maybe_return_error(script, null);
    
    if node.value_type != null {
        if check_flags & .EXPECT_LVALUE {
            ok, message := is_valid_lvalue(node);
            if !ok {
                log(message);
                if node.node_type == Node_Identifier {
                    log("Identifier was '%'", get_identifier_name(script, xx node));
                }
                set_general_error(script, message); // TODO: we should remove above logging and return concat'd message
                return null;
            }
            node.flags |= .IS_LVALUE;
        }
        node.flags |= .TYPECHECKED;
    }
    
    return node.value_type;
}

typecheck_script :: (script: *Script) -> bool {
    if script.ast_root == null                  return false;
    if script.ast_root.node_type != Node_Block  return false;
    
    for script.ast_root.statements {
        typecheck_node(script, it, .IS_STATEMENT_ROOT);
        maybe_return_error(script, false);
    }
    
    script.flags |= .TYPECHECKED;
    return true;
}

resolve_declaration_by_identifier :: (script: *Script, identifier: string, scope_block: *Node, need_lvalue := false) -> *Node_Declaration {
    scope := scope_block;
    while scope {
        if scope.node_type == {
          case Node_Block;
            declaration := find_declaration_in_block(xx scope, identifier);
            if declaration  return declaration;
            
          case Node_For_Loop;
            declaration := find_iterator_declaration(xx scope, identifier, need_lvalue);
            if declaration  return declaration;
        }
        scope = scope.scope;
    }
    for script.global_declarations {
        if it.name == identifier {
            return it;
        }
    }
    return null;
}

declaration_is_malleable_literal :: (declaration: Node_Declaration) -> bool {
    if declaration.init_expression == null  return false;
    literal := declaration.init_expression;
    return literal.node_type == Node_Literal && (literal.flags & .IS_MALLEABLE);
}

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
            // TODO: this is a bad error message since it's reported with the for loop's location and not the identifier's location.
            log("%: Error: it_index cannot be used as an lvalue.", get_location(for_loop));
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
    
    log("%: Error: unable to implicitly cast type '%' to '%'", get_location(node), as_type(node.value_type), as_type(type));
    return false, null;
}

// we just reach into the literal and re-type it in place
coerce_literal_type :: (literal: *Node_Literal, type: *Type_Info) -> bool {
    if literal.value_type == type  return true;
    if literal.literal_type == {
      case .NUMBER;
        if !is_numeric_type(type)  return false;
        new_number := Small_Any.{ type };
        remap_data(to_any(*new_number), to_any(*literal.number));
        literal.number = new_number;
        return true;
    }
    log("%: Error: unable to coerce % literal of type '%' to '%'", get_location(literal), literal.literal_type, as_type(literal.value_type), as_type(type));
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
          case .EXTERNAL_VARIABLE;  return true,  "";
          case .EXTERNAL_PROCEDURE; return false, "Error: an external procedure cannot be used as an lvalue.";
          case .STRUCT_MEMBER;      return true,  "";
            
          case .LITERAL;
            ok, msg := is_valid_lvalue(identifier.literal);
            return ok, msg;
            
          case .DECLARATION;
            // TODO: update this logic for the new semantics around constant declarations and macros
            // if (identifier.declaration.flags & .IS_CONSTANT) 
            // && !declaration_is_malleable_literal(identifier.declaration) {
            //     return false, "Error: Attempting to use an identifier as an lvalue which refers to a constant declaration which is not malleable.";
            // }
            return true, "";
        }
        
      case Node_Dot;
        dot := node.(*Node_Dot);
        if dot.left.value_type.type == .STRUCT {
            ok, msg := is_valid_lvalue(dot.left);
            return ok, msg;
        }
        ok, msg := is_valid_lvalue(dot.right);
        return ok, msg;
        
      case Node_Subscript;          return true,  "";
      
      // NOTE: it is possible these nodes may be} able to produce valid lvalues in the future
      case Node_Operation;          return false, "Error: using the result of an operation as an lvalue is not currently supported";
      case Node_Procedure_Call;     return false, "Error: using the result of a procedure call as an lvalue is not currently supported";
      case Node_Cast;               return false, "Error: using the result of a cast as an lvalue is not currently supported";
      
      // TODO: These cases really should not even }remotely happen... so maybe we should not even bother with an error message?
      case Node_Declaration;        return false, "Error: a declaration cannot be used as an lvalue;";
      case Node_Block;              return false, "Error: a block cannot be used as an lvalue;";
      case Node_If_Statement;       return false, "Error: an if statement cannot be used as an lvalue;";
      case Node_While_Loop;         return false, "Error: a while loop cannot be used as an lvalue;";
      case Node_For_Loop;           return false, "Error: a for loop cannot be used as an lvalue;";
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
        set_general_error(script, "Error: failed to evaluate directive in typechecking phase.");
        return;
    }
    if directive.runtime_node {
        directive.value_type = typecheck_node(script, directive.runtime_node, check_flags, hint_type);
    }
}

typecheck_literal :: (using script: *Script, literal: *Node_Literal, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    if #complete literal.literal_type == {
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
            new_number := Small_Any.{ type = hint_type };
            remap_data(to_any(*new_number), to_any(*literal.number));
            literal.number = new_number;
        } else {
            // NOTE: also doing a little remap here in default case, 
            //       since we actually want the default float type to be float32
            // if literal.number.type == type_info(float64) {
                new_number := Small_Any.{ type = type_info(float32) };
                remap_data(to_any(*new_number), to_any(*literal.number));
                literal.number = new_number;
            // }
        }
        literal.value_type = literal.number.type;
        
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
            set_general_error(script, "%: Error: type hint provided to struct literal is not a struct type. (was '%')", get_location(literal), as_type(ti_struct));
            return;
        }
        
        for literal.aggr.expressions {
            if it_index >= ti_struct.members.count {
                set_general_error(script, "%: Error: too many values provided in struct literal.", get_location(literal));
                return;
            }
            member := ti_struct.members[it_index];
            expr_ti := typecheck_node(script, it, hint_type = member.type); 
            maybe_return_error(script);
            if expr_ti != member.type {
                set_general_error(script, "%: Error: value type provided in struct initializer for field '%' was incorrect. Expected '%', got '%'.", get_location(it), member.name, as_type(member.type), as_type(expr_ti));
                return;
            }
        }
        
        // TODO: we could reduce space required for storing intermediate values
        //       if we pass a value_pointer alongside hint_type in typechecking
        //       that way if hint type is accepted, then it can also use value_pointer 
        //       for its backing value instead of allocating its own space for value
        literal.value_type = ti_struct;
        literal.aggr.value_pointer = Dynamic_New(literal.value_type,, script.allocator);
        dprint("allocated space for literal of type %, node %\n", literal.value_type, literal);
    }
    
    // all literals are assumed to be constants except for ANY literals, since we want to be able to use those as anonymous lvalues.
    if literal.literal_type != .ANY {
        literal.flags |= .IS_CONSTANT;
    }
    assert(literal.value_type != null);
}

typecheck_identifier :: (using script: *Script, identifier: *Node_Identifier, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    assert(identifier.identifier_type == .UNRESOLVED);
    
    declaration := resolve_declaration_by_identifier(script, identifier.name, identifier.scope, check_flags & .EXPECT_LVALUE != 0);
    if declaration {
        if declaration.(*void) > identifier.(*void) {
            set_general_error(script, "%: Error: identifier % used before its declaration.", get_location(identifier), identifier.name);
            return;
        }
        // TODO: we cannot mark the identifier as constant based on the declaration yet, 
        //       since the declaration may later be referenced by another identifier as an lvalue, making the declaration non-constant
        //       This is fine since we don't really do any constant evaluation on the AST, but when we lower to bytecode, 
        //       we will need to consider that a DECLARATION identifier may yet need to be checked for constancy.
        // TODO: if the declaration is a macro though, and the init_expression of the macro is constant, then we can still go ahead and safely mark the identifier as constant
        //       this may be a good idea to go ahead and implement since it would allow us to use such an identifier in a type expression.
        if declaration.flags & .IS_CONSTANT {
            if check_flags & .EXPECT_LVALUE {
                dprint("removing IS_CONSTANT flag from declaration '%' because its identifier is being used as an lvalue\n", declaration.name);
                declaration.flags &= ~.IS_CONSTANT;
            }
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
            identifier.value_type      = type_info(Type);
            return;
        }
    }
    
    for variables {
        if it.name == identifier.name {
            identifier.identifier_type = .EXTERNAL_VARIABLE;
            identifier.index           = it_index;
            identifier.value_type      = variables[it_index].binding.type;
            return;
        }
    }
    
    // if the identifier is referring to an external procedure, 
    //     then that will have to be handled in PROCEDURE_CALL case, 
    //     since we need the additional context for overload resolution.
    if !(check_flags & .IS_PROCEDURE_EXPRESSION) {
        dprint("%: Error: Unable to resolve identifier '%'.\n", get_location(identifier), identifier.name);
    }
    
    // NOTE: we don't attach an error message here
    set_error(script, .UNRESOLVED_IDENTIFIER, "");
    return;
}


typecheck_operation :: (using script: *Script, operation: *Node_Operation, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    left_check_flags := check_flags;
    if operation.operator_type == .ASSIGNMENT {
        left_check_flags |= .EXPECT_LVALUE;
        hint_type = null; // just in case?
    }
    
    left_type := typecheck_node(script, operation.left, left_check_flags, hint_type);
    maybe_return_error(script);
    
    if operation.operator_type == .ASSIGNMENT {
        hint_type = left_type; // so that we can hint right side based on left
    }
    
    right_type: *Type_Info;
    if operation.operator_type != .UNARY {
        right_type = typecheck_node(script, operation.right, check_flags, hint_type);
        maybe_return_error(script);
    }
    
    // special case: operator = is always a simple memcpy
    if operation.name == "=" {
        if left_type != right_type {
            ok, replacement := try_implicit_cast(script, operation.right, left_type); 
            if !ok {
                set_general_error(script, "%: Error: left and right types must match in a simple assignment statement!", get_location(operation));
                return;
            }
            if replacement  replace_node(*operation.right, replacement);
        }
        operation.value_type = left_type;
        return;
    }
    
    // TODO: figure out better casting system and remove this horrible hack
    // enums are coerced to their underlying type for the purposes of binary operations, then will be cast back to proper type
    // for now, we will just coerce all to s64/int 
    // in order for this to be ok, the values being pointed to need to not have junk in the unused bytes... 
    // and currently we don't do anything to really intentionally ensure this
    coerce_enum_return_type: *Type_Info;
    if left_type.type == .ENUM 
    && left_type == right_type {
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
            // dprint("allocating variable of type % at % for node %\n", as_type(operation.value_type), operation.return_ptr, operation);
        }
        
        return;
    }
    
    // if operation was not a builtin, then we will try to resolve the operation as an overloaded procedure
    procedure_call := alloc_node(script, Node_Procedure_Call);
    procedure_call.procedure_expression = make_identifier(script, operation.name);
    
    if operation.operator_type == .UNARY 
        then procedure_call.arguments = array_copy((*Node).[ operation.left ],, script.allocator);
        else procedure_call.arguments = array_copy((*Node).[ operation.left, operation.right ],, script.allocator);
    operation.overload_procedure    = procedure_call;
    operation.flags                |= .OVERLOAD;
    
    operation.value_type = typecheck_node(script, procedure_call);
}

typecheck_procedure_call :: (using script: *Script, procedure_call: *Node_Procedure_Call, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    ti_proc := typecheck_node(script, procedure_call.procedure_expression, .IS_PROCEDURE_EXPRESSION).(*Type_Info_Procedure);
    
    if script.error.type == .UNRESOLVED_IDENTIFIER 
    && procedure_call.procedure_expression.node_type == Node_Identifier {
        script.error = .{}; // clear the error and continue
        
        identifier := procedure_call.procedure_expression.(*Node_Identifier);
        assert(identifier.identifier_type == .UNRESOLVED); // so we can access name member validly
        
        // TODO: we should probably refactor typechecking to return a real error code
        //       then we can notify caller if failure is due to a missing type hint
        //       so that we can handle that case more intelligently here when resolving procedure
        
        /*
            Procedure arguments are sort of typechecked twice, once before selecting an overload and once after.
            In the first check, we do not provide any type hints, which means we may fail to typecheck arguments where the type hint is required.
            But we ignore this error on the first pass, since we may be able to provide a type hint later after selecting an overload.
        */
        for procedure_call.arguments  typecheck_node(script, it);
        script.error = .{}; // clear the error and continue
        
        procedure_name  := identifier.name;
        procedure_index := resolve_procedure_index(script, procedure_name, procedure_call.arguments, procedure_call.flags & .OVERLOAD == .OVERLOAD);
        if procedure_index == -1 {
            set_general_error(script, ""); // error message is logged by resolve_procedure_index
            return;
        }
        
        identifier.identifier_type = .EXTERNAL_PROCEDURE;
        identifier.index           = procedure_index;
        identifier.flags          |= .TYPECHECKED;
        ti_proc:, identifier.value_type = procedures[procedure_index].type;
        
        /*
            For the second pass over the arguments, we go back and verify the types for all arguments, re-typechecking those that failed due to a missing type hint.
            We also iterate more manually here so that we can replace nodes if we need to insert any implicit casts, since we need a **Node for that.
        */
        for 0..procedure_call.arguments.count-1 {
            node_ptr_ptr  := *procedure_call.arguments[it];
            node          := node_ptr_ptr.*;
            expected_type := ti_proc.argument_types[it];
            if node.value_type == null {
                dprint("re-typechecking argument with hint_type %", as_type(expected_type));
                type := typecheck_node(script, node, hint_type = expected_type);
                maybe_return_error(script);
                if type != expected_type {
                    log("%: Error: we deferred typechecking a procedure argument in the hopes that we could later hint the type and resolve it that way, but that failed.", get_location(node));
                    log("\tProcedure identifier: '%'", procedure_name);
                    log("\tArgument expression: '%'", sprint_node(script, node,, temp));
                    log("\tExpected type '%', got '%'.", expected_type, type);
                    set_general_error(script, ""); // TODO: 
                    return;
                }
            } else {
                ok, replacement := try_implicit_cast(script, node, expected_type);
                if !ok {
                    log("%: Error: failed while trying to implicitly cast a procedure argument.", get_location(node));
                    log("\tProcedure identifier: '%'", procedure_name);
                    log("\tArgument expression: '%'", sprint_node(script, node,, temp));
                    log("\tExpected type '%', got '%'.", expected_type, node.value_type);
                    set_general_error(script, ""); // TODO: 
                    return;
                }
                if replacement  replace_node(node_ptr_ptr, replacement);
            }
        }
        
        procedure_call.value_type = ifx ti_proc.return_types 
            then ti_proc.return_types[0] 
            else type_info(void);
    }
    // procedure was the result of some larger expression, so it already has a strict type
    else {
        if ti_proc == null {
            set_general_error(script, "%: Error: failed to typecheck procedure_expression.", get_location(procedure_call));
            return;
        }
        if ti_proc.type != .PROCEDURE {
            set_general_error(script, "%: Error: attempt to call a non-procedure type as if it were a procedure.", get_location(procedure_call));
            return;
        }
        if ti_proc.return_types.count > 1 {
            set_general_error(script, "%: Error: attempt to call a procedure which returns more than one value. This is not currently supported.", get_location(procedure_call));
            return;
        }
        
        // typecheck arguments. we know what they must be, so we just straightforwardly match, and can hint type trivially
        if ti_proc.argument_types.count != procedure_call.arguments.count {
            set_general_error(script, "%: Error: incorrect number of arguments provided for procedure call. Expected %, got %.\n", get_location(procedure_call), ti_proc.argument_types.count, procedure_call.arguments.count);
            return;
        }
        for procedure_call.arguments {
            expected_type := ti_proc.argument_types[it_index];
            argument_type := typecheck_node(script, it, hint_type = expected_type);
            maybe_return_error(script);
            if argument_type != expected_type {
                set_general_error(script, "%: Error: mismatch on argument % for procedure call. Expected %, got %.\n", get_location(it), it, as_type(expected_type), as_type(argument_type));
                return;
            }
        }
        
        procedure_call.value_type = ifx ti_proc.return_types 
            then ti_proc.return_types[0] 
            else type_info(void);
    }
    
    if is_aggr(procedure_call.value_type) {
        procedure_call.return_ptr = Dynamic_New(procedure_call.value_type,, script.allocator);
        // dprint("allocating variable of type % at % for node %\n", as_type(procedure_call.value_type), procedure_call.return_ptr, procedure_call);
    }
    assert(procedure_call.value_type != null);
}

typecheck_cast :: (using script: *Script, node_cast: *Node_Cast, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    value_type := typecheck_node(script, node_cast.value);
    maybe_return_error(script);
    
    if node_cast.flags & .IMPLICIT {
        node_cast.value_type = node_cast.implicit_type;
    } else {
        type_expression := node_cast.type_expression;
        type_type := typecheck_node(script, type_expression, hint_type = type_info(Type));
        maybe_return_error(script);
        if type_type.type != .TYPE {
            set_general_error(script, "%: Error: Type expression in cast did not resolve to a Type.", get_location(type_expression));
            return;
        }
        // TODO: make sure that type expression is constant before evaluating it
        if !(type_expression.flags & .IS_CONSTANT) {
            set_general_error(script, "%: Error: Type expression in cast must be constant.", get_location(type_expression));
            return;
        }
        type_any, ok := evaluate_node(script, type_expression);
        if !ok {
            set_general_error(script, "%: Error: failed to evaluate type expression in cast!", get_location(type_expression));
            return;
        }
        node_cast.value_type = type_any.value_pointer.(**Type_Info).*;
    }
    
    // TODO: check if we can actually perform the cast here
    //       for now, we won't precheck this since we will just use remap_data
    return;
}

typecheck_dot :: (using script: *Script, dot: *Node_Dot, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    // Currently, the only valid case for dot.left being null is for unary dot enums.
    // Setting left_type to 'Type' when dot.left is null gets us into the proper case down below.
    left_type: *Type_Info;
    if dot.left {
        left_type = typecheck_node(script, dot.left, check_flags & .EXPECT_LVALUE);
        maybe_return_error(script);
    } else {
        left_type = type_info(Type); 
    }
    
    if left_type.type == {
      case .STRUCT;
        if dot.right.node_type != Node_Identifier {
            set_general_error(script, "%: Error: right side of struct member dot dereference expression must be an identifier.", get_location(dot));
            return;
        }
        
        // TODO: we should check that member offset is non-negative so as to prevent use of constants until we can properly support those
        member_identifier := dot.right.(*Node_Identifier);
        assert(member_identifier.node_type == Node_Identifier && member_identifier.identifier_type == .UNRESOLVED);
        member_name := member_identifier.name;
        dprint("typechecking struct member identifier at % with name '%'\n", member_identifier, member_name);
        
        member_ti, member_offset := get_field(xx left_type, member_name);
        if member_ti == null {
            set_general_error(script, ""); // TODO:
            return;
        }
        
        // tricky case. left may be some complex expression where the result is an instance of a struct type.
        // so we can't just set the left type to be a namespace.
        //      but we do still want to do this if left type is an identifier? or no, since it could be a type identifier? which maybe we want to leave as is?
        if member_ti.flags & .CONSTANT {
            set_general_error(script, "%: Error: access of constant struct members is not yet allowed.", get_location(dot));
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
        return; // no error
        
      // case .ENUM;
        // TODO: access enum namespace through instance of enum type
        
      case .TYPE;
        namespace: *Type_Info;
        if dot.left {
            // TODO: make sure that type expression is constant before evaluating it
            if !(dot.left.flags & .IS_CONSTANT) {
                set_general_error(script, "%: Error: Type expression on left-hand side of dot operator must be constant.", get_location(dot));
                return;
            }
            left_any, ok := evaluate_node(script, dot.left);
            if !ok {
                set_general_error(script, ""); // TODO:
                return;
            }
            namespace = left_any.value_pointer.(**Type_Info).*;
        } else {
            if hint_type == null || (hint_type.type != .ENUM && hint_type.type != .STRUCT) {
                set_general_error(script, "%: Error: unary dot can only be used for enums, but hint_type was %", get_location(dot), as_type(hint_type));
                return;
            }
            namespace = hint_type;
        }
        
        if namespace.type == {
          case .STRUCT;
            if dot.right.node_type == {
              case Node_Literal;
                right_type := typecheck_node(script, dot.right, hint_type = namespace);
                maybe_return_error(script);
                if namespace != right_type {
                    set_general_error(script, "%: Error: type returned by right side of struct literal expression did not match type specified on left side of dot.", get_location(dot.right));
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
            set_general_error(script, "%: Error: use of struct type as a namespace is not yet implemented!", get_location(dot));
            return;
            
          case .ENUM;
            ti_enum := namespace.(*Type_Info_Enum);
            
            right := dot.right.(*Node_Identifier);
            assert(right.node_type == Node_Identifier && right.identifier_type == .UNRESOLVED);
            member_name := right.name;
            
            value, found := enum_name_to_value(ti_enum, member_name);
            if !found {
                set_general_error(script, "%: Error: invalid enum value name '%' for enum type '%'.", get_location(dot), member_name, as_type(ti_enum));
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
            set_general_error(script, "%: Error: a % type cannot be used as a namespace.", get_location(dot), namespace.type);
            return;
        }
    }
    
    set_general_error(script, "%: Error: left side of dot expression must be a struct or type.", get_location(dot));
    return;
}

typecheck_subscript :: (using script: *Script, subscript: *Node_Subscript, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    typecheck_node(script, subscript.base_expression, check_flags & .EXPECT_LVALUE);
    maybe_return_error(script);
    
    base_type := subscript.base_expression.value_type.(*Type_Info_Array);
    if base_type.type != .ARRAY {
        set_general_error(script, "%: Error: Type '%' cannot be indexed!", get_location(subscript), as_type(base_type));
        return;
    }
    
    typecheck_node(script, subscript.indexing_expression);
    maybe_return_error(script);
    
    indexing_type := subscript.indexing_expression.value_type;
    if !is_numeric_type(indexing_type) {
        set_general_error(script, "%: Error: Type '%' cannot be used in an indexing expression!", get_location(subscript), as_type(indexing_type));
        return;
    }
    
    subscript.value_type = base_type.element_type;
    return;
}

typecheck_declaration :: (using script: *Script, declaration: *Node_Declaration, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    // NOTE: currently I am placing the value type of the declared variable in the value_type slot of the declaration node.
    //       this is different from how other statements are done, which all return void instead.
    //       may have to change this if it creates issues, I suppose.
    other_declaration := resolve_declaration_by_identifier(script, declaration.name, declaration.scope);
    if other_declaration != null && declaration != other_declaration {
        set_general_error(script, "%: Error: redeclaration of identifier '%'.", get_location(declaration), declaration.name);
        return;
    }
    
    explicit_type: *Type_Info;
    if declaration.type_expression {
        type_expression := declaration.type_expression;
        type_type := typecheck_node(script, type_expression, hint_type = type_info(Type));
        maybe_return_error(script);
        if type_type.type != .TYPE {
            set_general_error(script, "%: Error: Type expression in declaration did not resolve to a Type.", get_location(declaration));
            return;
        }
        // TODO: make sure that type expression is constant before evaluating it
        if !(type_expression.flags & .IS_CONSTANT) {
            set_general_error(script, "%: Error: Type expression in declaration must be constant.", get_location(declaration));
            return;
        }
        type_any, ok := evaluate_node(script, type_expression);
        if !ok {
            set_general_error(script, ""); // TODO:
            return;
        }
        explicit_type = type_any.value_pointer.(**Type_Info).*;
    }
    
    right_type: *Type_Info;
    if declaration.init_expression {
        right_type = typecheck_node(script, declaration.init_expression, hint_type = explicit_type);
        maybe_return_error(script);
        
        if explicit_type && explicit_type != right_type {
            set_general_error(script, "%: Error: init expression of declaration does not match given type. Expected '%', got '%'.", get_location(declaration), explicit_type, right_type);
            return;
        }
        // if the init expression of a declaration is marked as constant, then the declaration is also marked as constant
        // however, if the declaration is later re-assigned to a new value, the declaration will lose this designation
        if declaration.init_expression.flags & .IS_CONSTANT {
            declaration.flags |= .IS_CONSTANT;
        }
    }
    
    print("explicit_type: %\n", explicit_type);
    print("right_type: %\n", right_type);
    
    declaration.value_type    = ifx explicit_type else right_type;
    declaration.value_pointer = Dynamic_New(declaration.value_type,, script.allocator);
    // dprint("allocated space for internal variable '%', type %, node %\n", declaration.name, right_type, node);
    
    if declaration.flags & .MACRO {
        if declaration.init_expression.node_type != Node_Literal {
            set_general_error(script, "%: Error: macro declarations can only be made to simple literals at this time.", get_location(declaration));
            return;
        }
        value, ok := evaluate_node(script, declaration.init_expression, declaration.value_pointer);
        if !ok {
            set_general_error(script, "%: Error: failed to evaulate init expression of macro declaration. We do this during typechecking at the moment as a hack to make malleable literals work, and I'm not fixing this until we really make macros actually a thing.", get_location(declaration));
            return;
        }
    }
    
    return;
}

typecheck_block :: (using script: *Script, block: *Node_Block, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    for block.statements {
        typecheck_node(script, it, .IS_STATEMENT_ROOT);
        maybe_return_error(script);
    }
    
    block.value_type = type_info(void);
    return;
}

typecheck_if_statement :: (using script: *Script, if_statement: *Node_If_Statement, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    typecheck_node(script, if_statement.condition); // TODO: must be bool or coercible to bool
    maybe_return_error(script);
    
    typecheck_node(script, if_statement.statement, .IS_STATEMENT_ROOT);
    maybe_return_error(script);
    
    if_statement.value_type = type_info(void);
    return;
}

typecheck_while_loop :: (using script: *Script, while_loop: *Node_While_Loop, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    typecheck_node(script, while_loop.condition); // TODO: must be bool or coercible to bool
    maybe_return_error(script);
    
    typecheck_node(script, while_loop.statement, .IS_STATEMENT_ROOT);
    maybe_return_error(script);
    
    while_loop.value_type = type_info(void);
    return;
}

typecheck_for_loop :: (using script: *Script, for_loop: *Node_For_Loop, check_flags := Typechecking_Flags.NONE, hint_type: *Type_Info = null) {
    // value_pointer for it and it_index decls get set later when actually iterating for loop
    for_loop.it_index_decl = .{
        name       = "it_index",
        value_type = type_info(int),
    };
    
    if #complete for_loop.control_type == {
      case .RANGE;
        dprint("typechecking range lower\n");
        lower_type := typecheck_node(script, for_loop.range.lower, hint_type = type_info(int));
        maybe_return_error(script);
        
        dprint("typechecking range upper\n");
        upper_type := typecheck_node(script, for_loop.range.upper, hint_type = lower_type);
        maybe_return_error(script);
        
        if lower_type != upper_type { 
            set_general_error(script, "%: Error: type mismatch on lower and upper bounds of range.\n", get_location(for_loop));
            return;
        }
        
        // TODO: may not allow float here?
        if !is_numeric_type(lower_type) {
            set_general_error(script, ""); // TODO:  
            return;
        }
        for_loop.it_decl = .{        
            name       = "it",
            value_type = lower_type,
        };
        
      case .ARRAY;
        typecheck_node(script, for_loop.array_expression, check_flags = .EXPECT_LVALUE);
        maybe_return_error(script);
        ti_array := for_loop.array_expression.value_type.(*Type_Info_Array);
        
        // TODO: special case for something like a Many_Any / Any_Array_View
        // better option than creating new array view type info's dynamically
        if ti_array.type != .ARRAY {
            set_general_error(script, "%: Error: expected an array type.", get_location(for_loop.array_expression));
            return;
        }
        for_loop.it_decl = .{
            name       = "it",
            value_type = ti_array.element_type,
        };
        
      case .LIST;
        first_type := typecheck_node(script, for_loop.list[0], check_flags = .EXPECT_LVALUE);
        maybe_return_error(script);
        
        for 1..for_loop.list.count-1 {
            node := for_loop.list[it];
            type := typecheck_node(script, node, check_flags = .EXPECT_LVALUE);
            maybe_return_error(script);
            if type != first_type {
                set_general_error(script, "%: Error: all expressions in comma-separated iteration list must be of the same type. First type was %, type of element % was %", get_location(node), as_type(first_type), it + 2, as_type(type));
                return;
            }
        }
        
        for_loop.it_decl = .{
            name       = "it",
            value_type = first_type,
        };
    }
    
    typecheck_node(script, for_loop.statement, .IS_STATEMENT_ROOT);
    maybe_return_error(script);
    
    for_loop.value_type = type_info(void);
    return;
}
