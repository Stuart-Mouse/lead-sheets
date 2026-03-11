
Node :: struct {
    node_type:              Type;
    scope:                  *Node;              // used for identifier resolution. maybe we can remove in the future. TODO: should be a *Node_Block
    flags:                  Flags;
    value_type:             *Type_Info;         // null if not yet typechecked
    
    // TODO: consider keeping all source tokens, store pointer to token on node
    //       or store tokens based on node type, as maybe some will use multiple tokens.
    // TODO: should have some source code range rather than just a location
    
    // These get copied from source token.
    location:   Source_Code_Location;
    trivia:     string;
    
    Flags :: enum_flags { 
        // lexical flags
        PARENTHESIZED;          // explicitly
        IS_ARROW;               // denotes that a Node_Dot is actually an arrow
        IS_MALLEABLE;
        IS_CONSTANT;            // used to mark all nodes expressions that can be used as 'constants' (e.g. in a type slot of a declaration). that value may not ACTUALLY be a constant because I'm still figuring out how much that's even gonna be a thing in this language
        MACRO;                  // marks declarations with :: which act as simple AST node references to the init_expression
        
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
    
    // TODO: figure out how to not have to store this on every block...?
    stack_frame_size:       int;
}

// TODO: push/pop stack subframes per every block, even though the actual stack frame is only per named block
get_stack_space :: (script: *Script, requested_size: int, scope: *Node) -> stack_offset: int {
    block := get_stack_frame_block(script, scope);
    
    current_offset := block.stack_frame_size;
    
    // pad to 8 byte alignment
    size := (requested_size + 7) & ~7;
    block.stack_frame_size += size;
    
    return current_offset;
}

// TODO: resolve to closest named block or ast root
get_stack_frame_block :: (script: *Script, node: *Node) -> *Node_Block {
    scope := node;
    while scope {
        if is_stack_frame_block(script, scope) {
            return xx scope;
        }
        scope = scope.scope;
    }
    return null;
}

is_stack_frame_block :: (script: *Script, node: *Node) -> bool {
    return node.node_type == Node_Block && (node.(*Node_Block).name || node == script.ast_root);
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
    TODO: maybe we can generalize identifier reference such that we just have an interface defining things like get_name, get_type, get_value, etc.
    
    Referent :: struct {
        meta:   *Meta;
        handle: Handle;
        
        Handle :: struct {
            pointer:    *void;
            index:      int;
        }
        
        Meta :: struct {
            get_name:   (Referent) -> bool, string;
            get_type:   (Referent) -> bool, *Type_Info;
            get_value:  (Referent) -> bool, Any; // How would this actually work without broader context?
        }
    }
    
    NOTES:
        identifier.name is only valid when identifier_type is UNRESOLVED
            because of this, the identifer in Node_Declaration.left is always left as UNRESOLVED 
            this is fine in this case because we never execute this node, and it is only ever accessed through the declaration
        after the identifier is resolved, the name must be retrieved indirectly, depending on the resolved type
        this indireciton means that we can change the identifier more dynamically now while the code is still in AST
        but, obviously we will want to add some way to check that these identifiers do not conflict when we reserialize...
*/
Identifier_Type :: enum { UNRESOLVED; TYPE; LITERAL; STRUCT_MEMBER; DECLARATION; EXTERNAL_VARIABLE; EXTERNAL_PROCEDURE; }; 

Node_Identifier :: struct {
    using #as node_base:    Node;
    identifier_type:        Identifier_Type;
    union {
        name:           string;
        index:          int;    // external variable, procedure, or type
        declaration:    *Node_Declaration;
        member:         struct { info: *Type_Info_Struct_Member; offset: int; };
        
        // NOTE: This exists only for enum literals. We should probably find another way to handle this case.
        literal:        *Node_Literal;  // we may end up needing to also store a name for this, in case the literal was not an enum...
    }
}

is_unresolved_identifier :: (node: *Node) -> bool {
    return node 
        && node.node_type == Node_Identifier 
        && node.(*Node_Identifier).identifier_type == .UNRESOLVED;
}

get_identifier_name :: (script: *Script, identifier: *Node_Identifier) -> string {
    if #complete identifier.identifier_type == {
      case .UNRESOLVED;         return identifier.name;
      case .TYPE;               return script.type_table[identifier.index].name;
      case .EXTERNAL_VARIABLE;  return script.variables [identifier.index].name;
      case .EXTERNAL_PROCEDURE; return script.procedures[identifier.index].name;
      case .STRUCT_MEMBER;      return identifier.member.info.name;
      case .DECLARATION;        return get_declaration_name(script, identifier.declaration);
      case .LITERAL;
        assert(identifier.literal.value_type.type == .ENUM, "Invalid value type '%' for identifier.literal. Only enums are currently supported.", as_type(identifier.literal.value_type));
        return enum_value_to_name(xx identifier.literal.value_type, identifier.literal.number.as_s64);
    }
    unreachable();
}

identifiers_refer_to_the_same_thing :: (script: *Script, i1: *Node_Identifier, i2: *Node_Identifier) -> bool {
    if !i1 || !i2 || i1.identifier_type != i2.identifier_type  return false;
    if #complete i1.identifier_type == {
      case .UNRESOLVED;         return false;
      case .TYPE;               return i1.index == i2.index;
      case .EXTERNAL_VARIABLE;  return i1.index == i2.index;
      case .EXTERNAL_PROCEDURE; return i1.index == i2.index;
      case .STRUCT_MEMBER;      return i1.member == i2.member;
      case .DECLARATION;        return i1.declaration == i2.declaration;
      case .LITERAL;            return i1.literal.value_type == i2.literal.value_type && i1.literal.number.as_u64 == i2.literal.number.as_u64;
    }
}

Node_Declaration :: struct {
    using #as node_base:    Node;
    left:                   *Node;
    union { // use value_pointer if declaration is static, else 
        value_pointer:      *void;
        stack_offset:       int;
    }
    type_expression:        *Node;
    init_expression:        *Node;
}

get_value_pointer :: (script: *Script, declaration: *Node_Declaration) -> *void {
    if declaration.left.node_type == Node_Dot {
        return null;
    }
    if declaration.flags & .MACRO {
        return declaration.value_pointer;
    }
    return script.stack.frame + declaration.stack_offset;
}

get_declaration_name :: (script: *Script, declaration: *Node_Declaration) -> string {
    if is_unresolved_identifier(declaration.left) {
        return declaration.left.(*Node_Identifier).name;
    }
    // TODO: figure out what to do about virtual member declarations
    return ""; // declaration is either malformed or has not yet been typechecked, or it is a declaration for a virtual member
}

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

// TODO: probably make unary dot enum literal an actual Node_Literal instead of an identifier
// TODO: figure out a better way to implement the ANY literal type
Node_Literal :: struct {
    using #as node_base: Node;
    literal_type: enum { BOOLEAN; STRING; NUMBER; STRUCT; ANY; };
    union {
        number:  Any_Number;
        text:    string;
        aggregate: struct {
            value_pointer:      *void;      // once type is resolved, we store pointer to allocated value here
            expressions:        [] *Node;   // expressions to init struct members by. later maybe also accepting statements
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

can_be_malleable :: (literal: *Node_Literal) -> bool {
    return !(literal.flags & .IS_CONSTANT).(bool);
}

is_malleable :: (literal: *Node_Literal) -> bool {
    return (literal.flags & .IS_MALLEABLE).(bool);
}



Node_Operation :: struct {
    using #as node_base:    Node;
    name:                   string;
    operator_index:         int;
    union { // check node.flag & .OVERLOAD
        builtin_operation_index:    Builtin_Operation_ID;
        overload_procedure:         *Node_Procedure_Call;
    }
    left, right:    *Node;      // only left is used if .UNARY flag is present
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

// TODO: maybe remove Node_Dot and Node_Subscript in favor of just using a general Binary Operator Node
//       then separate binary and unary operators since they are lexically and semantially different
// TODO: maybe we can shortcut evaluation of certain dot nodes when they're chained but only the last bit is relevant
//       if I'm thinking of the tree correctly, then because the dot is high prec and (i think it's considered left-associateive?) that it gets put further down the tree (left side gets extended and right side is always the terminal bit) 
//       need to verify this, but if this is the case then we actually get the last dot node first in tree walking order, so we can easily short-circuit evaluate it if left side is some constant expression
Node_Dot :: struct {
    using #as node_base:        Node;
    left, right:                *Node;
}

Node_Subscript :: struct {
    using #as node_base:    Node;
    base_expression:        *Node;
    indexing_expression:    *Node;
}


// ===== Flow Control Statements =====

/*
    We now have two possible paths ahead here:
    1. Do this Node_Flow_Control thing, and try to generalize all flow control constructs so that user can implement their own more easily.
    2. Just attach an owner node pointer to Code_Block and put some overloadable procedure in the script context so that code block can query its owning node for additional declarations that will go into that block's scope.
        This may be a more attractive option because we will probably want to refactor blocks to work as data scopes as well (so the owner could be a struct literal)
*/

// Node_Flow_Control :: struct {
//     using #as node_base: Node;
//     block:          *Node_Block;
//     declarations:   [] *Node_Declaration;
// }

Node_If_Statement :: struct {
    using #as node_base:    Node;
    condition:              *Node;
    block:                  *Node_Block;
}

Node_While_Loop :: struct {
    using #as node_base:    Node;
    condition:              *Node;
    block:                  *Node_Block;
}

Node_For_Loop :: struct {
    using #as node_base: Node;
    control_type: enum { RANGE; ARRAY; LIST; };
    union {
        array_expression:   *Node;
        range:              struct { lower, upper: *Node; };
        list:               [] *Node;
    }
    block:  *Node_Block;
    
    it_decl, it_index_decl:     *Node_Declaration;
}



alloc_node :: (script: *Script, $Node_Type: Type, scope: *Node = null, loc := #caller_location, trivia := "") -> *Node_Type
#modify {
    return is_subclass_of(Node_Type.(*Type_Info_Struct), "Node"), "Type passed to alloc_node must be a subclass of Node!";
} {
    node := New(Node_Type,, get_pool_allocator(*script.pool));
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
    value, ok := Utils.parse_number(source_token.text);
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
