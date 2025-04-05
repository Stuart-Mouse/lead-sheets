
Node :: struct {
    node_type:              Type;
    scope:                  *Node;              // used for identifier resolution. maybe we can remove in the future
    flags:                  Flags;
    value_type:             *Type_Info;         // null if not yet typechecked
    source_token:           *Token;
    
    Flags :: enum_flags { 
        TYPECHECKED;
        IS_LVALUE;
        IS_MALLEABLE;
        IS_CONSTANT;            // used to mark all nodes expressions that can be used as 'constants' (e.g. in a type slot of a declaration). that value may not ACTUALLY be a constant because I'm still figuring out how much that's even gonna be a thing in this language
        MACRO;                  // marks declarations with :: which act as simple AST node references to the init_expression
        OVERLOAD;
        IMPLICIT;               // used for casts
        PARENTHESIZED;          // explicitly 
        DISCARD_VALUE;
        MUST_BE_STATEMENT_ROOT;
    };
}

/*
    Notes about source_token on nodes:
    
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
      case .DECLARATION;        return identifier.declaration.name;
      case .LITERAL;
        assert(identifier.literal.value_type.type == .ENUM, "Invalid value type '%' for identifier.literal. Only enums are currently supported.", as_type(identifier.literal.value_type));
        return enum_value_to_name(xx identifier.literal.value_type, identifier.literal.number.as_s64);
    }
    assert(false, "unreachable");
}

Node_Declaration :: struct {
    using #as node_base:    Node;
    name:                   string;
    value_pointer:          *void;
    type_expression:        *Node;
    init_expression:        *Node;
}

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
    literal_type: enum { STRING; NUMBER; STRUCT; ANY; };
    union {
        number:  Small_Any;
        text:    string;
        aggr: struct {
            value_pointer:      *void;      // once type is resolved, we store pointer to allocated value here
            expressions:        [] *Node;   // expressions to init struct members by. later maybe also accepting statements
        }
        any: Any;
    }
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
    operator_type:          Operator_Type;
    union { // check node.flag & .OVERLOAD
        operator_index:     int;
        overload_procedure: *Node_Procedure_Call;
    }
    left, right:            *Node;      // only left is used if .UNARY flag is present
    return_ptr:             *Node;      // only if return type is aggregate
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

alloc_node :: inline (script: *Script, $T_Node: Type, scope: *Node = null, source_token: *Token = null) -> *T_Node
#modify {
    return is_subclass_of(T_Node.(*Type_Info_Struct), "Node"), "Type passed to alloc_node must be a subclass of Node!";
} {
    node := New(T_Node,, script.allocator);
    node.node_type = T_Node;
    node.scope     = ifx scope else script.current_scope;
    if source_token {
        node.source_token = copy_to_pool_if_needed(*script.pool, source_token).value_pointer;
    }
    return node;
}

replace_node :: inline (original: **Node, new: *Node) {
    new.scope  = original.*.scope;
    original.* = new;
}

get_location :: (node: Node) -> Source_Code_Location {
    if node.source_token  return node.source_token.location;
    return .{};
}

