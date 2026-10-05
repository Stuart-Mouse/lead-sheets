/*
    Notes on design of AST Nodes and user-extensibility:
    
    One of the desing goals of Lead Sheets is to make it relatively easy for the user to modify or extend the language in a variety of ways.
    
    Instead of using a superclass/subclass setup for to create interfaces to certain types of nodes,
    (where a pointer to a function table needs to be attached to each instances of a node)
    interface procedures are often placed directly on the script struct itself.
    
    This has a few benefits:
        - nodes stay slimmer because they don't need to individually hold procedure pointers or pointers to vtables
        - we can override the behaviour of existing nodes without needing to modify their internal data structure or content
        - the user can 
    
    If the user wants to add some marginal functionality on top of some interface, or add handling for new types, tey an just write a single procedure that handles those types in a switch statement,
    and then call back into the default procedure for the default node types.
*/

Node :: struct {
    node_type:              Type;
    value_type:             *Type_Info;
    scope:                  *Node;
    serial:                 u32;
    flags:                  Flags;
    
    // These get copied from source token.
    location:   Source_Code_Location;
    trivia:     string;
    Flags :: enum_flags u32 { 
        // lexical flags
        PARENTHESIZED;          // explicitly, textually
        IS_CONSTANT;            // used to mark all nodes expressions that can be used as 'constants' (e.g. in a type slot of a declaration). that value may not ACTUALLY be a constant because I'm still figuring out how much that's even gonna be a thing in this language
        
        STATIC;
        HAS_BEEN_EVALUATED;
        
        GENERATED;              // Indicates that a node was generated in Jai code, not parsed from some source file. Generally, this means that during serialization, some default formatting will be used.
        
        // typechecking flags
        OVERLOAD;               // gets set when operator resolves to some overloaded procedure
        IMPLICIT;               // used for casts and blocks
        
        MUST_BE_STATEMENT_ROOT; // TODO: try to remove this as well. can probably just make this a procedure 
        
        VALUE_TYPE_IS_NOMINAL;
        
        // evaluation flags
        PUSH_BY_POINTER;
        SWAP_OPERANDS;
    };
}


/*
    TODO: document this better so we understand its purpose more fully.
    
    In general, we want to avoid pushing aggregates to the stack by value, since this is slower than simply pushing a pointer.
    But in many cases, the aggregate is an intermediate value in some larger expression, and we can't avoid pushing the aggregate by value.
    
    TODO: document the exact cases where we need to push by value, and the exact cases where we don't. maybe there is a better solution to this whole situation than using this PUSH_BY_POINTER flag.
*/
should_push_by_pointer :: (node: *Node) -> bool {
    return node.flags & .PUSH_BY_POINTER != 0;
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

// TODO: introduce various block types so that we can use block for struct literals and the like. 
Node_Block :: struct {
    using #as node_base:    Node;
    name:                   string;
    statements:             [] *Node;
    
    stack_pointer:          *void;    // offset from stack frame
    stack_frame_size:       u32;
}

// TODO: Maybe replace this with Node_Namespace or something like that
Node_Dynamic_Block :: struct {
    using #as node_base:    Node;
    name:                   string;
    statements:             [..] *Node;
    storage_allocator:      Allocator;
}

/*
    This does 2 things:
        1. returns the current stack watermark and then increments it
            this gives the caller an absolute offset from the stack frame that they can use directly (no need to sum parent offsets)
        2. increments the local block's stack size
            honestly not sure what purpose this stack size serves for non-frame blocks atm
*/
get_stack_space_in_block :: (script: *Script, block: *Node_Block, requested_size: int) -> stack_offset: u64 {
    offset := block.stack_frame_size;
    
    // pad to 8 byte alignment
    size := (requested_size + 7) & ~7;
    block.stack_frame_size += xx size;
    
    return offset;
}

Node_Cast :: struct {
    using #as base:     Node;
    value:              *Node;
    union {
        type_expression:  *Node;
        implicit_type:    *Type_Info;
    }
}


/*
    NOTES:
        identifier.name is only valid when identifier_type is UNRESOLVED
            because of this, the identifer in Node_Declaration.left is always left as UNRESOLVED 
            this is fine in this case because we never execute this node, and it is only ever accessed through the declaration
        after the identifier is resolved, the name must be retrieved indirectly, depending on the resolved type
        this indireciton means that we can change the identifier more dynamically now while the code is still in AST
        but, obviously we will want to add some way to check that these identifiers do not conflict when we reserialize...
*/

// Referent_Type :: struct {
//     get_name:   (*Script, *void) -> bool, string;
//     get_type:   (*Script, *void) -> bool, *Type_Info;
//     get_value:  (*Script, *void) -> bool, *void; // How would this actually work without broader context?
//     compare:    (*Script, *void, *void) -> bool;
// }

Identifier_Type :: enum { UNRESOLVED; DECLARATION; LITERAL; STRUCT_MEMBER; EXTERNAL_TYPE; EXTERNAL_VARIABLE; EXTERNAL_PROCEDURE; }; 

Node_Identifier :: struct {
    using #as node_base:    Node;
    identifier_type:        Identifier_Type;
    union {
        name:               string;     // NOTE: This should only be read when referent is null. Otherwise, get the name from the referent.
        // referent:           *Referent;
        index:              int;    // external variable, procedure, or type
        declaration:        *Node_Declaration;
        
        // TODO: don't store offset here. We can store on the dot node or something.
        struct_member_info: *Type_Info_Struct_Member;
        literal:            *Node_Literal;
    }
}



#if false
is_unresolved_identifier :: (node: *Node) -> bool {
    return node 
        && node.node_type == Node_Identifier 
        && node.(*Node_Identifier).identifier_type == .UNRESOLVED;
}

get_name :: (script: *Script, identifier: *Node_Identifier) -> string {
    if #complete identifier.identifier_type == {
      case .UNRESOLVED;         return identifier.name;
      case .EXTERNAL_TYPE;      return script.type_table[identifier.index].name;
      case .EXTERNAL_VARIABLE;  return script.variables [identifier.index].name;
      case .EXTERNAL_PROCEDURE; return script.procedures[identifier.index].name;
      case .STRUCT_MEMBER;      return identifier.struct_member_info.name;
      case .DECLARATION;        return get_name(script, identifier.declaration);
      case .LITERAL;
        // TODO: should use this code path for true/false literals as well
        // TODO: we should probably have a context procedure for getting an identifier name from a literal value
        assert(identifier.literal.value_type.type == .ENUM, "Invalid value type '%' for identifier.literal. Only enums are currently supported.", as_type(identifier.literal.value_type));
        return enum_value_to_name(xx identifier.literal.value_type, identifier.literal.number.as_s64);
    }
    unreachable();
    
    // if identifier.referent {
    //     _, name := get_name(script, identifier.referent);
    //     return name;
    // }
    // return identifier.name;
}

compare_referents :: (script: *Script, i1: *Node_Identifier, i2: *Node_Identifier) -> bool {
    if !i1 || !i2 || i1.referent_type != i2.referent_type  return false;
    // return i1.referent_type.compare(script, i1.referent, i2.referent);
    if #complete i1.identifier_type == {
      case .UNRESOLVED;         return false;
      case .EXTERNAL_TYPE;      return i1.index == i2.index;
      case .EXTERNAL_VARIABLE;  return i1.index == i2.index;
      case .EXTERNAL_PROCEDURE; return i1.index == i2.index;
      case .STRUCT_MEMBER;      return i1.member == i2.member;
      case .DECLARATION;        return i1.declaration == i2.declaration;
      case .LITERAL;            return i1.literal.value_type == i2.literal.value_type && i1.literal.number.as_u64 == i2.literal.number.as_u64;
    }
}


// TODO: We may want to find some way of attaching a named block to a declaration so that it can be resolved in the same way.
//       This will be necessary in order to prevent name conflicts between declarations and named blocks.

Node_Declaration :: struct {
    using #as node_base:    Node;
    left:                   *Node;
    value_locator:          u64;
    type_expression:        *Node;
    init_expression:        *Node;
}

get_value_pointer :: (script: *Script, declaration: *Node_Declaration) -> *void {
    if declaration.left.node_type == Node_Dot {
        return null;
    }
    if declaration.flags & .STATIC {
        return declaration.value_locator.(*void);
    }
    base_pointer := script.get_storage_base_pointer_for_scope(script, declaration.scope);
    return base_pointer + declaration.value_locator;
}

get_name :: (script: *Script, declaration: *Node_Declaration) -> string {
    if declaration == null  return "";
    
    if declaration.left.node_type == {
      case Node_Identifier;
        return get_name(script, declaration.left.(*Node_Identifier));
    }
    
    // TODO: figure out what to do about virtual member declarations
    return "";
}



// ===== Procedures and Directives =====

Node_Procedure_Call :: struct {
    using #as node_base:    Node;
    procedure_expression:   *Node;
    arguments:              [] *Node;
}

Node_Directive :: struct {
    using #as node_base:    Node;
    name:                   string;
    directive_index:        int;
    arguments:              [] *Node;
    runtime_node:           *Node;  // returned from directive as a replacement node. We still keep the directive itself around for re-serialization, though.
}



// ===== Literals =====

// TODO: figure out a better way to implement the ANY literal type
Node_Literal :: struct {
    using #as node_base: Node;
    literal_type: enum { BOOLEAN; STRING; NUMBER; STRUCT; ANY; };
    union {
        number:  Any_Number;
        text:    string;
        aggregate: struct {
            value_pointer:      *void;      // will only be used if literal has static storage
            expressions:        [] *Node;   // expressions to init struct members by. later maybe also accepting assignment statements
        }
        any: Any;
    }
}

is_number_literal :: (node: *Node) -> bool {
    return node.node_type == Node_Literal 
        && node.(*Node_Literal).literal_type == .NUMBER;
}

get_literal_value_as_any :: (literal: *Node_Literal) -> Any {
    assert(literal.value_type != null);
    if #complete literal.literal_type == {
      case .BOOLEAN; return to_any(*literal.number);
      case .STRING;  return literal.text;
      case .NUMBER;  return to_any(*literal.number);
      case .STRUCT;  return Any.{ literal.value_type, literal.aggregate.value_pointer };
      case .ANY;     return literal.any;
    }
    unreachable();
}


// ===== Operators =====

Node_Operator :: struct {
    using #as node_base:    Node;
    token:                  string;
    operator_id:            Operator_ID;
    
    union { // check node.flag & .OVERLOAD
        builtin_operation_id:   Builtin_Operation_ID;
        overload_procedure:     *Node_Procedure_Call;
    }
    directive: *Node_Directive;
}

Node_Binary_Operator :: struct {
    using #as operator_base: Node_Operator;
    left, right: *Node;
}

Node_Unary_Operator :: struct {
    using #as operator_base: Node_Operator;
    expression: *Node;
}

get_operator_info :: inline (script: *Script, node: *Node_Operator) -> *Operator_Info { 
    return get_operator_info(script, node.operator_id); 
}

get_precedence :: inline (script: *Script, node: *Node_Operator) -> int {
    return get_operator_info(script, node).precedence;
}

get_operator_lexical_type :: inline (script: *Script, node: *Node_Operator) -> Type {
    return get_operator_node_type(script, node).kind;
}


// ===== Indexing Expresions =====

/*
    These are not implemented as Operators because they really need entirely different logic.
    We also don't need to consider operator precedence for these nodes because they always have an essentially infinite binding power.
*/

Node_Dot :: struct {
    using #as node_base:        Node;
    left, right:                *Node;
    member_offset:              int;
}

Node_Subscript :: struct {
    using #as node_base:    Node;
    base_expression:        *Node;
    indexing_expression:    *Node;
}

Node_Arrow :: struct {
    using #as node_base:        Node;
    left, right:                *Node;
}



// ===== Flow Control Statements =====

Node_If_Statement :: struct {
    using #as node_base: Node;
    then_block:     *Node_Block;
    else_block:     *Node_Block;
    condition:      *Node;
    subcondition:   *Node;
}

Node_If_Expression :: struct {
    using #as node_base: Node;
    then_expression:    *Node;
    else_expression:    *Node;
    condition:          *Node;
}

Node_While_Loop :: struct {
    using #as node_base: Node;
    block:      *Node_Block;
    condition:  *Node;
}

Node_For_Loop :: struct {
    using #as node_base: Node;
    block:        *Node_Block;
    
    // TODO: should just accept a `control_expression: *Node` here and make range/list part of the type system
    control_type: enum { RANGE; ARRAY; LIST; };
    union {
        array_expression:   *Node;
        range:              struct { lower, upper: *Node; };
        list:               [] *Node;
    }
    
    declarations: [2] *Node_Declaration;
    #overlay(declarations[0]) it_decl:        *Node_Declaration;
    #overlay(declarations[1]) it_index_decl:  *Node_Declaration;
}

Node_Break :: struct {
    using #as node_base: Node;
    target_identifier:  *Node_Identifier;
    resolved_target:    *Node;    // TODO: maybe try to remove later on, would require changes to Node_Identifier
    value_expression:   *Node;
}


alloc_node :: (script: *Script, $Node_Type: Type, scope: *Node = null, loc := #caller_location, trivia := "") -> *Node_Type
#modify {
    return is_subclass_of(Node_Type.(*Type_Info_Struct), "Node"), "Type passed to alloc_node must be a subclass of Node!";
} {
    node := New(Node_Type,, get_allocator(script));
    node.node_type = Node_Type;
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
    ok, value := Utils.parse_any_number(source_token.text);
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
    literal.aggregate.expressions = contents;
    return literal;
}

make_identifier :: (script: *Script, source_token: Token) -> *Node_Identifier {
    node := alloc_node(script, Node_Identifier, loc = source_token.location, trivia = source_token.trivia);
    node.name = source_token.text;
    return node;
}

// NOTE: It is actually important that one call make_operation instead of making the node manually!
//       This is because we do the extra shenanigans for handling operator directives here.
make_operator :: (script: *Script, source_token: Token, operator_id: int, left: *Node, right: *Node, loc := #caller_location) -> *Node_Operator {
    operator_info := get_operator_info(script, operator_id);
    
    base_operator: *Node_Operator;
    if #complete operator_info.lexical_type == {
      case .PREFIX; #through;
      case .POSTFIX;
        unary_operator := alloc_node(script, Node_Unary_Operator, loc = source_token.location, trivia = source_token.trivia);
        unary_operator.token       = source_token.text;
        unary_operator.operator_id = operator_id;
        unary_operator.expression  = left;
        
        base_operator = unary_operator;
        
      case .BINARY; #through;
      case .ASSIGNMENT;
        binary_operator := alloc_node(script, Node_Binary_Operator, loc = source_token.location, trivia = source_token.trivia);
        binary_operator.token       = source_token.text;
        binary_operator.operator_id = operator_id;
        binary_operator.left        = left;
        binary_operator.right       = right;
        
        // if operator_info.lexical_type == .ASSIGNMENT then binary_operator.flags |= .MUST_BE_STATEMENT_ROOT;
        
        base_operator = binary_operator;
        
      case .UNINITIALIZED;
      case;
        assert(false, );
        set_parse_error(script, "Invalid operator lexical type in make_operator: %", operator_info.lexical_type, loc = source_token.location);
        return null;
    }
    
    if operator_info.directive_name {
        directive := alloc_node(script, Node_Directive, loc = source_token.location, trivia = source_token.trivia);
        directive.name = source_token.text;
        
        directive.directive_index = -1;
        for script.directives {
            if it.name == directive.name {
                directive.directive_index = it_index;
            }
        }
        if directive.directive_index == -1 {
            set_parse_error(script, "Unable to resolve directive '%'\n", directive.name, loc = directive.location);
            return null;
        }
        
        // TODO: could add some get_operands(*Node_Operator) -> [] *Node
        if base_operator.node_type == {
          case Node_Unary_Operator;
            unary_operator := base_operator.(*Node_Unary_Operator);
            directive.arguments = array_copy((*Node).[unary_operator.expression],, get_allocator(script));
            
          case Node_Binary_Operator;
            binary_operator := base_operator.(*Node_Binary_Operator);
            directive.arguments = array_copy((*Node).[binary_operator.left, binary_operator.right],, get_allocator(script));
        }
        
        if !evaluate_directive(script, directive, .PARSE) {
            set_parse_error(script, "Failed while trying to execute a directive during parsing.", loc = source_token.location);
            return null;
        }
        
        base_operator.directive = directive;
    }
    
    return base_operator;
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

make_arrow :: inline (script: *Script, source_token: Token, left: *Node, right: *Node) -> *Node_Arrow {
    node := alloc_node(script, Node_Arrow, loc = source_token.location, trivia = source_token.trivia);
    node.left  = left;
    node.right = right;
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
//       since the identifier will just print itself with get_name. so i guess it's actually just fine to always use the fully qualified enum name if we print an enum literal?
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
    else if value.type.type == .ENUM || is_numeric(value.type) {
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
    return is_numeric(T), "Type must be numeric (integer or float).";
} {
    literal := alloc_node(script, Node_Literal);
    literal.literal_type = .NUMBER;
    literal.number = Any_Number.from(number);
    return literal;
}

make_identifier :: (script: *Script, name: string, loc := #caller_location) -> *Node_Identifier {
    node := alloc_node(script, Node_Identifier, loc = loc);
    node.name = name;
    return node;
}
