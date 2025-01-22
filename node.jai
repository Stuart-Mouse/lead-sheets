
Node :: struct {
    node_type:              Type;
    parent, next:           *Node;              // used for argument lists or connecting statements in a block
    flags:                  Flags;
    value_type:             *Type_Info;         // null if not yet typechecked
    whitespace_before:      string;             // can include comments, gets pasted back when serializing
    
    Flags :: enum_flags { 
        IS_LVALUE; 
        IS_MALLEABLE;
        IS_CONSTANT;
        UNARY; 
        OVERLOAD; 
        DISCARD_VALUE; 
    };
}

Node_Block :: struct {
    using #as node_base:    Node;
    statements:             *Node; 
}

// TODO: before continuing with whitespace/comments issue, refactor node creation so that we are passing in source token wherever possible.
//       then we can copy certain attributes from the token automatically, which will make it easier to try different methods of resolving whitespace issue, will improve debug abilities as well

// TOOD: storing comments on AST will be tricky, since they may be be in the middle of statements, not just between statements
//       also need to consider this with whitespace, where there may be newlines in between AST subexpressions
//       in the short term I may just make a rule that all comments will get hoisted up a line or to end of line when they're in the middle of an expression/statement
// maybe we could just hack it so that comments are considered as part of the whitespace before a node, and we just hold a string the the whitespace before each node and paste it back on serialize
// Node_Comment :: struct {
//     using #as node_base:    Node;
//     comment:                string; 
//     comment_type:           enum { LINE; BLOCK; }
// }

Node_Cast :: struct {
    using #as base:         Node;
    left:                   *Node;
    right:                  *Node_Identifier;
}

/*
    A note about identifiers and declarations:
    Because identifiers can refer to both internal declarations and external variables and procedures, 
        it is simpler to put the resolved type on the identifier node itself rather than 
        creating and maintaining declaration nodes for the external variables and procedures.
    The external variables in particular add complication because they can be subbed out, 
        so we need an additional indirection in accessing them from scripts.
*/
Node_Identifier :: struct {
    using #as node_base:    Node;
    name:                   string;
    resolved_type:          enum { UNRESOLVED; TYPE; STRUCT_MEMBER; DECLARATION; EXTERNAL_VARIABLE; EXTERNAL_PROCEDURE; }; 
    union {
        variable_index:     int; // external variable
        procedure_index:    int; // external procedure
        declaration:        *Node_Declaration;
        member_type:        *Type_Info_Struct_Member;
        type_info_pointer:  *Type_Info;
    }
}

Node_Declaration :: struct {
    using #as node_base:    Node;
    name:                   string;
    value_pointer:          *void;
    type_expression:        *Node_Identifier; // must resolve to a type declaration
    init_expression:        *Node;    
}

Node_Procedure_Call :: struct {
    using #as node_base:    Node;
    procedure_expression:   *Node;
    arguments:              *Node;
    arguments_count:        int;
    return_ptr:             *void; // will point to some memory allocated in script pool if return type is an aggregate type
}

Node_Directive :: struct {
    using #as node_base:    Node;
    name:                   string;
    directive_index:        int;
    arguments:              *Node;
    arguments_count:        int;
}

// NOTE: may need something like the Many_Any for doing array literals in the future
//       this is because we can't construct a type info for an array literal
//       due to this and other reasons, we probably need our own internal version of a type_info
//       theoretically we could just construct type_infos for the array literals dynamically, but not sure if that's a great idea
//       also we could do some kind of modified Any which also captures the semantics we need around additional indirections to value_pointers
Node_Literal :: struct {
    using #as node_base: Node;
    literal_type: enum { STRING; NUMBER; STRUCT; ARRAY; };
    using value_union: union {
        number:  Small_Any; // Q: couldn't we just use value_type? A: only issue is that if value_type is preset, that node will skip typechecking...
        text:    string;
        aggr: struct {
            expressions:    *Node; // expressions to init struct members or array values by. later maybe also accepting statements
            value_pointer:  *void; // once type is resolved, we store pointer to allocated value here
        }
        
        // used to store backing value for numeric values 
        // once a concrete type has been determined
        // in theory we could also store small structs here (but that's probably ill-informed...)
        bytes:   [16] u8; 
    }
}

get_literal_value_as_any :: (literal: Node_Literal) -> Any {
    assert(literal.value_type != null);
    if literal.literal_type == {
      case .STRING;
        return Any.{ type_info(string), *literal.text };
        
      case .NUMBER;
        return to_any(literal.number);
        
      case .STRUCT;
        return Any.{ literal.value_type, literal.aggr.value_pointer };
        
      case .ARRAY;
        log("Error: array literals are not yet implemented in get_value_pointer_and_size().");
        return Any.{};
    }
    
    assert(false);
    return Any.{};
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
    operator_index:         int;        // TODO: maybe put into union with overload_procedure, then use .OVERLOAD flag to disambiguate
    left, right:            *Node;      // only left is used if .UNARY
    return_ptr:             *Node;      // only if return type is aggregate
    overload_procedure:     *Node_Procedure_Call;
}

// can be either a struct member access or a struct / array literal with explicit type
Node_Dot :: struct {
    using #as node_base:    Node;
    dot_type:               enum { UNRESOLVED; STRUCT_MEMBER; LITERAL; };
    left, right:            *Node;
    union {
        member_offset:      int;
    }
}

Node_Subscript :: struct {
    using #as node_base:    Node;
    base_expression:        *Node;
    indexing_expression:    *Node;
}

Node_Assignment :: struct {
    using #as node_base:    Node;
    left, right:            *Node; 
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
    control_type: enum { RANGE; ARRAY; };
    union {
        array_expression:  *Node;
        range:  struct { lower, upper: *Node; };
    }
    statement:  *Node;
    
    // just storing these directly on the for loop itself
    it_decl, it_index_decl: Node_Declaration;
}

for_expansion :: (node: *Node, body: Code, flags: For_Flags) #expand {
    #if flags & .REVERSE {
        // required to define it index here it seems? but it doesn't make any sense to use...
        `it := node;
        `it_index := 0;
        while it != null {
            #insert body;
            it = it.prev;
            it_index -= 1;
        }
    } else {
        `it := node;
        `it_index := 0;
        while it != null {
            #insert body;
            it = it.next;
            it_index += 1;
        }
    }
}


alloc_node :: (ctxt: *Script, $T_Node: Type, parent_node: *Node = null, source_token := Token.{}, loc := #caller_location) -> *T_Node #expand
#modify {
    return is_subclass_of(T_Node.(*Type_Info_Struct), "Node");
} {
    node := New(T_Node,, ctxt.allocator);
    node.node_type = T_Node;
    node.parent    = ifx parent_node else ctxt.current_parent_node;
    // dbg_print("allocated % at %\n", T_Node, loc);
    
    // TODO: copy stuff from given token
    return node;
}
