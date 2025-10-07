
Node :: struct {
    node_type:              Type;
    scope:                  *Node;              // used for identifier resolution. maybe we can remove in the future
    flags:                  Flags;
    value_type:             *Type_Info;         // null if not yet typechecked
    
    // get copied from source token
    location:   Source_Code_Location;
    trivia:     string;
    
    Flags :: enum_flags { 
        TYPECHECKED;
        IS_ARROW;               // denotes that a Node_Dot is actually an arrow
        IS_LVALUE;
        IS_MALLEABLE;
        IS_CONSTANT;            // used to mark all nodes expressions that can be used as 'constants' (e.g. in a type slot of a declaration). that value may not ACTUALLY be a constant because I'm still figuring out how much that's even gonna be a thing in this language
        MACRO;                  // marks declarations with :: which act as simple AST node references to the init_expression
        OVERLOAD;
        IMPLICIT;               // used for casts
        PARENTHESIZED;          // explicitly 
        DISCARD_VALUE;
        MUST_BE_STATEMENT_ROOT;
        IS_STATEMENT_ROOT;
    };
}

/*
    Notes about trivia on nodes:
    
    Currently, we don't do a perfect job of preserving whitespace and comments on nodes (trivia), 
    since there is not a one-to-one correspondence between tokens and nodes.
    Many node types would need to store references to multiple tokens, and that's just not something I'm going to deal with at the moment.
    For this reason, one should not rely on comments to be preserved if it's in strange places, such as between the name and parameter list of a procedure call.
    In the future I may figure out some better solution, but for now one should be somewhat careful about where they stick comments if they plan to save over a script using the serialization functionality.
    
    The ultimate solution will probably just be to totally rewrite parsing so that we can do the pre-lexing thing and then storing token ranges on nodes.
    But this will require major refactoring for all of parsing since we will have to get tokens by reference instead of by value.
    And even then it will greatly complicate serialization, since we will have to consider both the source tokens and the current AST, which may have been altered or had nodes inserted procedurally.
    Basically, its doable, but we are lacking a lot of the necessary prerequisites at the moment.
*/

Node_Block :: struct {
    using #as node_base:    Node;
    name:                   string;
    statements:             [] *Node; 
}

// TODO: before continuing with whitespace/comments issue, refactor node creation so that we are passing in source token wherever possible.
//       then we can copy certain attributes from the token automatically, which will make it easier to try different methods of resolving whitespace issue, will improve debug abilities as well

Node_Cast :: struct {
    using #as base:     Node;
    value:              *Node;
    union {
        type_expression:  *Node;
        implicit_type:    *Type_Info;
    }
}

/*
    A note about identifiers and declarations:
    Because identifiers can refer to both internal declarations and external variables and procedures, 
        it is simpler to put the resolved type on the identifier node itself rather than 
        creating and maintaining declaration nodes for the external variables and procedures.
    The external variables in particular add complication because they can be subbed out, 
        so we need an additional indirection in accessing them from scripts.
    We could still make it work this way by holding a pointer to the corresponding declaration and then using that to update the declaration's value_pointer when the external variable is updated
    BUT this means we NEED to have these declaration nodes in order to even use the external variables in expressions
    and maybe we don't want that if we want the user to be able to purely use the script as a way of evaluating expressions whose nodes are just allocated in temp
    and that use case would be completely broken, becuase we'd be absolutely forcing the use of the pool allocator...
        but maybe we should just do that if it allows us to greatly simplify the identifier resolution code?
    
        
    NOTES:
        identifier.name is now only valid when unresolved
        after the identifier is resolved, the name must be retrieved indirectly, depending on the resolved type
        this indireciton means that we can change the identifier more dynamically now while the code is still in AST
        but, obviously we will want to add some way to check that these identifiers do not conflict when we reserialize...
        
        external variables, procedures, and types all now use the same index member in the union
        
        LITERAL has been added as an identifier type, to be used in place of ENUM
            this will allow us to implement things like use of constant struct members
    
        left side of node_dot can now be null since we actually go through node_dot now for enums
        
    
*/
Identifier_Type :: enum { UNRESOLVED; TYPE; LITERAL; STRUCT_MEMBER; DECLARATION; EXTERNAL_VARIABLE; EXTERNAL_PROCEDURE; }; 

Node_Identifier :: struct {
    using #as node_base:    Node;
    identifier_type:        Identifier_Type;
    union {
        name:           string;
        index:          int;    // external variable, procedure, or type
        declaration:    *Node_Declaration;
        member:         *Type_Info_Struct_Member;
        literal:        *Node_Literal;  // we may end up needing to also store a name for this, in case the literal was not an enum...
    }
}

get_identifier_name :: (script: *Script, identifier: *Node_Identifier) -> string {
    if #complete identifier.identifier_type == {
      case .UNRESOLVED;         return identifier.name;
      case .TYPE;               return script.type_table[identifier.index].name;
      case .EXTERNAL_VARIABLE;  return script.variables [identifier.index].name;
      case .EXTERNAL_PROCEDURE; return script.procedures[identifier.index].name;
      case .STRUCT_MEMBER;      return identifier.member.name;
      case .DECLARATION;        return get_declaration_name(script, identifier.declaration);
      case .LITERAL;
        assert(identifier.literal.value_type.type == .ENUM, "Invalid value type '%' for identifier.literal. Only enums are currently supported.", as_type(identifier.literal.value_type));
        return enum_value_to_name(xx identifier.literal.value_type, identifier.literal.number.as_s64);
    }
    assert(false, "unreachable");
}

Node_Declaration :: struct {
    using #as node_base:    Node;
    left:                   *Node;
    value_pointer:          *void;
    type_expression:        *Node;
    init_expression:        *Node;
}

get_declaration_name :: (script: *Script, declaration: *Node_Declaration) -> string {
    if !declaration.left  return "";
    
    if declaration.left.node_type == {
      case Node_Identifier;
        return get_identifier_name(script, xx declaration.left);
        
      case Node_Dot;
        
    }
    assert(false, "Unable to get declaration name!");
    return "";
}

is_virtual_member_declaration :: (declaration: *Node_Declaration) -> bool {
    return declaration.left.node_type == Node_Dot && (declaration.left.flags & .IS_ARROW);
}

// get_virtual_member_owner :: () {
    
// }

Node_Procedure_Call :: struct {
    using #as node_base:    Node;
    procedure_expression:   *Node;
    arguments:              [] *Node;
    return_ptr:             *void; // will point to some memory allocated in script pool if return type is an aggregate type
}

Node_Directive :: struct {
    using #as node_base:    Node;
    name:                   string;
    directive_index:        int;
    arguments:              [] *Node;
    runtime_node:           *Node;  // returned from directive as a replacement node. We still keep the directive itself around for re-serialization, though.
}

// TODO: figure out a better way to implement the ANY literal type
Node_Literal :: struct {
    using #as node_base: Node;
    literal_type: enum { BOOLEAN; STRING; NUMBER; STRUCT; ANY; };
    union {
        number:  Any_Number;
        text:    string;
        aggr: struct {
            value_pointer:      *void;      // once type is resolved, we store pointer to allocated value here
            expressions:        [] *Node;   // expressions to init struct members by. later maybe also accepting statements
        }
        any: Any;
    }
}

get_literal_value_as_any :: (literal: *Node_Literal) -> Any {
    assert(literal.value_type != null);
    if #complete literal.literal_type == {
      case .BOOLEAN; return to_any(*literal.number);
      case .STRING;  return literal.text;
      case .NUMBER;  return to_any(*literal.number);
      case .STRUCT;  return Any.{ literal.value_type, literal.aggr.value_pointer };
      case .ANY;     return literal.any;
    }
    unreachable();
}

can_be_malleable :: (literal: *Node_Literal) -> bool {
    return !(literal.flags & .IS_CONSTANT).(bool);
}

is_malleable :: (literal: *Node_Literal) -> bool {
    return (literal.flags & .IS_MALLEABLE).(bool);
}

/*
    Not sure where to put this for now, but here's some documentation about a cool feature with literals:
    
    Malleable Literals
    
    In Lead Sheets, you can mark literals with a `?` in order denote that the literal should be modifiable when the script is being executed as an AST.
    The literal can then be used as an lvalue (but again, only while in AST form), and the script can be serialized back to text from AST with the now modified value of the literal replacing the original value.
    Like any other literal though, when it comes time to lower the AST to bytecode, any statements that use the literal as an lvalue must be stripped, and the value will be baked as a constant at all sites of use.
    This may seem like an odd feature, but I think it will actually be extremely useful for certain applications.
    For example, I am working on a game where the movements of all platforms and many entities in a level are determined by functions written into the level's script (which is separate from the main level file).
    This means that a substantial and critical part of the level's design cannot be editted within the level editor itself, since we would have no means to save those changes back to the original text of the script file.
    But, if we have these sorts of malleable literals that can be modified in the editor and then written back, we can now visually modify what was previously only modifiable as text.
    I feel like I'm still struggling to explain how cool this is to me, but needless to say I'm very excited to implement it and try it out.
    For example, if you could display all of a script's mallebale constants in a menu for the user to edit, or draw them directly into the level as handles that are attached to tilemaps or entities.
    Then when the user saves the level, it will save both the basic layout and the script that governs that layout.
    With the use of an immediate-mode UI, the user can generate new UI elements directly from the level script, allowing them to create a visual means of editting bespoke level behaviours.
    And best of all, when it actually comes time to run the level and its accompanying script, the malleable literals get magically baked away, so there's no runtime cost.
*/

Node_Operation :: struct {
    using #as node_base:    Node;
    name:                   string;
    operator_index:         int;
    union { // check node.flag & .OVERLOAD
        builtin_operation_index:    Builtin_Operation_ID;
        overload_procedure:         *Node_Procedure_Call;
    }
    left, right:    *Node;      // only left is used if .UNARY flag is present
    return_ptr:     *Node;      // only if return type is aggregate
    directive:      *Node_Directive;
}

get_operator :: inline (script: *Script, node: *Node_Operation) -> *Operator { 
    return get_operator(script, node.operator_index); 
}

get_precedence :: inline (script: *Script, node: *Node_Operation) -> int {
    return get_operator(script, node).precedence;
}

get_operator_kind :: inline (script: *Script, node: *Node_Operation) -> Operator.Kind {
    return get_operator(script, node).kind;
}

// TODO: maybe we can shortcut evaluation of certain dot nodes when they're chained but only the last bit is relevant
//       if I'm thinking of the tree correctly, then because the dot is high prec and (i think it's considered left-associateive?) that it gets put further down the tree (left side gets extended and right side is always the terminal bit) 
//       need to verify this, but if this is the case then we actually get the last dot node first in tree walking order, so we can easily short-circuit evaluate it if left side is some constant expression
Node_Dot :: struct {
    using #as node_base:    Node;
    left, right:            *Node;
}

Node_Subscript :: struct {
    using #as node_base:    Node;
    base_expression:        *Node;
    indexing_expression:    *Node;
}

Node_If_Statement :: struct {
    using #as node_base:    Node;
    condition:              *Node;
    statement:              *Node;
}

Node_While_Loop :: struct {
    using #as node_base:    Node;
    condition:              *Node;
    statement:              *Node;
}

Node_For_Loop :: struct {
    using #as node_base: Node;
    control_type: enum { RANGE; ARRAY; LIST; };
    union {
        array_expression:   *Node;
        range:              struct { lower, upper: *Node; };
        list:               [] *Node;
    }
    statement:  *Node;
    
    // just storing these directly on the for loop itself
    it_decl, it_index_decl: Node_Declaration;
}





alloc_node :: (script: *Script, $T_Node: Type, scope: *Node = null, loc := #caller_location, trivia := "") -> *T_Node
#modify {
    return is_subclass_of(T_Node.(*Type_Info_Struct), "Node"), "Type passed to alloc_node must be a subclass of Node!";
} {
    node := New(T_Node,, get_pool_allocator(*script.pool));
    node.node_type = T_Node;
    node.scope     = ifx scope else script.current_scope;
    node.location  = loc;
    node.trivia    = trivia;
    return node;
}

replace_node :: inline (original: **Node, new: *Node) {
    new.scope  = original.*.scope;
    original.* = new;
}

get_location :: (node: Node) -> string {
    return format_location(node.location);
}

format_location :: (location: Source_Code_Location) -> string {
    return tprint("%:%,%", location.fully_pathed_filename, location.line_number, location.character_number);
}


// ===== Make Nodes From Tokens =====

make_number_literal :: (script: *Script, source_token: Token) -> *Node_Literal {
    value, ok := parse_number(source_token.text);
    if !ok  return null;
    
    literal := alloc_node(script, Node_Literal, loc = source_token.location, trivia = source_token.trivia);
    literal.literal_type = .NUMBER;
    literal.number       = value;
    return literal;
}

make_string_literal :: (script: *Script, source_token: Token) -> *Node_Literal {
    literal := alloc_node(script, Node_Literal, loc = source_token.location, trivia = source_token.trivia);
    literal.literal_type = .STRING;
    literal.text         = source_token.text;
    return literal;
}

// NOTE: source_token here should be the open brace token
make_struct_literal :: (script: *Script, source_token: Token, contents: [] *Node) -> *Node_Literal {
    literal := alloc_node(script, Node_Literal, loc = source_token.location, trivia = source_token.trivia);
    literal.literal_type     = .STRUCT;
    literal.aggr.expressions = contents;
    return literal;
}

make_identifier :: (script: *Script, source_token: Token) -> *Node_Identifier {
    node := alloc_node(script, Node_Identifier, loc = source_token.location, trivia = source_token.trivia);
    node.name = source_token.text;
    return node;
}

// NOTE: It is actually important that one call make_operation instead of making the node manually!
//       This is because we do the extra shenanigans for handling operator directives here.
make_operation :: (script: *Script, source_token: Token, operator_index: int, left: *Node, right: *Node) -> *Node_Operation {
    operation := alloc_node(script, Node_Operation, loc = source_token.location, trivia = source_token.trivia);
    operation.name           = source_token.text;
    operation.operator_index = operator_index;
    operation.left           = left;
    operation.right          = right;
    
    _operator := get_operator(script, operator_index);
    
    if _operator.kind == .ASSIGNMENT then operation.flags |= .MUST_BE_STATEMENT_ROOT;
    
    if _operator.directive_name {
        directive := alloc_node(script, Node_Directive, loc = source_token.location, trivia = source_token.trivia);
        directive.name = source_token.text;
        
        directive.directive_index = -1;
        for script.directives {
            if it.name == directive.name {
                directive.directive_index = it_index;
            }
        }
        if directive.directive_index == -1 {
            set_parse_error(script, "%: Unable to resolve directive '%'\n", directive.location, directive.name);
            return null;
        }
        
        directive.arguments = array_copy((*Node).[operation.left, operation.right],, get_pool_allocator(*script.pool));
        
        if !evaluate_directive(script, directive, .PARSE) {
            set_parse_error(script, "%: failed while trying to execute a directive during parsing.", source_token.location);
            return null;
        }
        
        operation.directive = directive;
    }
    
    return operation;
}

make_procedure_call :: (script: *Script, source_token: Token, proc_expr: *Node, arg_exprs: [] *Node) -> *Node_Procedure_Call {
    node := alloc_node(script, Node_Procedure_Call, loc = source_token.location, trivia = source_token.trivia);
    node.procedure_expression = proc_expr;
    node.arguments            = arg_exprs;
    return node;
}

make_subscript :: (script: *Script, source_token: Token, operand: *Node, indexing_expr: *Node) -> *Node_Subscript {
    node := alloc_node(script, Node_Subscript, loc = source_token.location, trivia = source_token.trivia);
    node.base_expression     = operand;
    node.indexing_expression = indexing_expr;
    return node;
}

make_dot :: (script: *Script, source_token: Token, left: *Node, right: *Node) -> *Node_Dot {
    node := alloc_node(script, Node_Dot, loc = source_token.location, trivia = source_token.trivia);
    node.left  = left;
    node.right = right;
    return node;
}

make_arrow :: inline (script: *Script, source_token: Token, left: *Node, right: *Node) -> *Node_Dot {
    node := make_dot(script, source_token, left, right);
    node.flags |= .IS_ARROW;
    return node;
}

make_cast :: (script: *Script, source_token: Token, value: *Node, type_expr: *Node) -> *Node_Cast {
    node := alloc_node(script, Node_Cast, loc = source_token.location, trivia = source_token.trivia);
    node.value           = value;
    node.type_expression = type_expr;
    return node;
}


// ===== Make Nodes Manually =====

// TODO: need to have some kind of flag on such a node so that we know whether we need to print the fully qualified enum name with type or not
//       and that will depend on whether this node is held by a node_identifier/node_dot or not
//       if this node is held by a node_identifier or node_dot, then it actually won't get directly called by print_node anyhow though
//       since the identifier will just print itself with get_identifier_name. so i guess it's actually just fine to always use the fully qualified enum name if we print an enum literal?
//       or maybe we use a flag like .IMPLICIT to signify that the enum literal was using a unary dot in the source text.

// NOTE: we can't use make_literal to make a STRUCT literal since that would require providing values as expressions
//       so this will only work for basically numbers, strings, and enums
make_literal :: (script: *Script, value: Any, loc := #caller_location) -> *Node_Literal {
    literal := alloc_node(script, Node_Literal);
    literal.value_type = value.type;
    if value.type.type == .STRING {
        literal.literal_type = .STRING;
        literal.text = value.value_pointer.(*string).*;
    }
    else if value.type.type == .ENUM || is_numeric_type(value.type) {
        literal.literal_type = .NUMBER;
        literal.number = Any_Number.from(value);
    } else {
        // @Hack: doing this temporarily so that we can at least return structs and 
        //        such from directives and still insert them as literals.
        literal.literal_type = .ANY;
        literal.any = copy_to_pool_if_needed(*script.pool, value);
    }
    return literal;
}

make_number_literal :: (script: *Script, number: $T, loc := #caller_location) -> *Node 
#modify {
    return is_numeric_type(T), "Type must be numeric (integer or float).";
} {
    literal := alloc_node(script, Node_Literal);
    literal.literal_type = .NUMBER;
    literal.number = Any_Number.from(number);
    return literal;
}

make_identifier :: (script: *Script, name: string, loc := #caller_location) -> *Node_Identifier {
    node := alloc_node(script, Node_Identifier);
    node.name = name;
    return node;
}
