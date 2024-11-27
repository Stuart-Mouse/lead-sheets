
// TODO: what if we just use the actual Type value as the tag for node type?
//       this would allow use to simplify alloc_node to just take the type
//       and then we do not need to maintain alloc_node as we add new node types
//       only reason to keep enum around would be to serialize, but I don't think we would never want to serialize the AST instead of just using the source

// TODO: reimplement something like sprint_node 

// TODO: probably remove prev pointer from node, I don't think we need it after all

Node_Type :: enum {
    INVALID :: 0;
    
    // expressions
    LITERAL;
    IDENTIFIER;
    OPERATION;
    PROCEDURE_CALL;
    DOT;
    SUBSCRIPT;
    
    // statements
    DECLARATION;
    BLOCK;
    ASSIGNMENT;
    IF_STATEMENT;
    WHILE_LOOP;
    FOR_LOOP;
    
    // special
    DIRECTIVE;
}

Node :: struct {
    node_type:            Node_Type;
    parent, prev, next:   *Node;              // used for argument lists or connecting statements in a block
    flags:                Flags;
    value_type:           *Type_Info;         // null if not yet typechecked
    
    Flags :: enum_flags { 
        IS_LVALUE; 
        UNARY_OP; 
        OVERLOAD_OP; 
        DISCARD_VALUE; 
    };
}

Node_Block :: struct {
    using #as node_base: Node;
    statements: *Node; 
}

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
    using #as node_base:  Node;
    name:                 string;
    value_pointer:        *void;
    type_expression:      *Node_Identifier; // must resolve to a type declaration
    init_expression:      *Node;    
}

Node_Procedure_Call :: struct {
    using #as node_base:    Node;
    procedure_expression:   *Node;
    arguments:              *Node;
    arguments_count:        int;
    return_ptr:             *void; // will point to some memory allocated in script pool if return type is an aggregate type
}

Node_Directive :: struct {
    using #as node_base: Node;
    name:                string;
    directive_index:     int;
    arguments:           *Node;
    arguments_count:     int;
}

// NOTE: may need something like the Many_Any for doing arrays in the future
//       this is because we can't construct a type info for an array literal
//       bue to this and other reasons, we probably need our own internal version of a type_info
//       theoretically we could just construct type_infos for the array literals dynamically, but not sure if that's a great idea
//       also we could do some kind of modified Any which also captures the semantics we need around additional indirections to value_pointers
Node_Literal :: struct {
    using #as node_base: Node;
    literal_type: enum { STRING; NUMBER; STRUCT; ARRAY; };
    using value_union: union {
        number:  float;
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

Node_Operation :: struct {
    using #as node_base: Node;
    name:            string;     // TODO: hmmm...
    operator_index:  int;
    left, right:     *Node;
    return_ptr:      *Node;      // only if return type is aggregate
    // TODO: just point to procedure for overloaded operators?
}


// can be either a struct member access or a struct / array literal with explicit type
Node_Dot :: struct {
    using #as node_base:  Node;
    dot_type:             enum { UNRESOLVED; STRUCT_MEMBER; LITERAL; };
    left, right:          *Node;
    union {
        member_offset: int;
    }
}

Node_Subscript :: struct {
    using #as node_base: Node;
    base_expression:       *Node;
    indexing_expression:   *Node;
}

Node_Assignment :: struct {
    using #as node_base: Node;
    left, right: *Node; 
}

Node_If_Statement :: struct {
    using #as node_base: Node;
    condition: *Node;
    statement: *Node;
}

Node_While_Loop :: struct {
    using #as node_base: Node;
    condition: *Node;
    statement: *Node;
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



// Two allocate_node functions currently. 
// May possibly remove the first if we get rid of need for Node_Type enum.

alloc_node :: (ctxt: *Script, node_type: Node_Type, parent_node: *Node = null, loc := #caller_location) -> *Node {
    node: *Node;
    
    // have to allocate the proper amount of space for the node
    if #complete node_type == {
      case .LITERAL;         node = New(Node_Literal,,        ctxt.allocator);
      case .IDENTIFIER;      node = New(Node_Identifier,,     ctxt.allocator);
      case .DECLARATION;     node = New(Node_Declaration,,    ctxt.allocator);
      case .OPERATION;       node = New(Node_Operation,,      ctxt.allocator);
      case .PROCEDURE_CALL;  node = New(Node_Procedure_Call,, ctxt.allocator);
      case .DIRECTIVE;       node = New(Node_Directive,,      ctxt.allocator);
      case .DOT;             node = New(Node_Dot,,            ctxt.allocator);
      case .SUBSCRIPT;       node = New(Node_Subscript,,      ctxt.allocator);
      case .BLOCK;           node = New(Node_Block,,          ctxt.allocator);
      case .ASSIGNMENT;      node = New(Node_Assignment,,     ctxt.allocator);
      case .IF_STATEMENT;    node = New(Node_If_Statement,,   ctxt.allocator);
      case .WHILE_LOOP;      node = New(Node_While_Loop,,     ctxt.allocator);
      case .FOR_LOOP;        node = New(Node_For_Loop,,       ctxt.allocator);
      case;
        assert(false, "Invalid node type!");    
    }
    
    node.type   = node_type;
    node.parent = ifx parent_node else ctxt.current_parent_node;
    return node;
}

alloc_node :: (ctxt: *Script, $T_Node: Type, parent_node: *Node = null, loc := #caller_location) -> *T_Node #expand
#modify {
    ti := cast(*Type_Info_Struct) T_Node;
    return is_subclass_of(ti, "Node");
} {
    node := New(T_Node,, ctxt.allocator);
    node.node_type = #run type_to_enum(T_Node);
    node.parent    = ifx parent_node else ctxt.current_parent_node;
    dbg_print("allocated % at %\n", T_Node, loc);
    return node;
}


// Conversion functions to go back and forth between Node_Type and corresponding Type

enum_to_type :: (node_type: Node_Type) -> Type {
    if #complete node_type == {
      case .LITERAL;         return Node_Literal;
      case .IDENTIFIER;      return Node_Identifier;
      case .OPERATION;       return Node_Operation;
      case .PROCEDURE_CALL;  return Node_Procedure_Call;
      case .DIRECTIVE;       return Node_Directive;
      case .DOT;             return Node_Dot;   
      case .SUBSCRIPT;       return Node_Subscript;
      case .BLOCK;           return Node_Block;
      case .DECLARATION;     return Node_Declaration;
      case .ASSIGNMENT;      return Node_Assignment;
      case .IF_STATEMENT;    return Node_If_Statement;
      case .WHILE_LOOP;      return Node_While_Loop;
      case .FOR_LOOP;        return Node_For_Loop;
      case;
        assert(false, "Invalid node type!");    
    }
    return null;
}

type_to_enum :: (type: Type) -> Node_Type {
    if type == {
      case Node_Literal;         return .LITERAL;
      case Node_Identifier;      return .IDENTIFIER;
      case Node_Operation;       return .OPERATION;
      case Node_Procedure_Call;  return .PROCEDURE_CALL;
      case Node_Directive;       return .DIRECTIVE;
      case Node_Dot;             return .DOT;   
      case Node_Subscript;       return .SUBSCRIPT;
      case Node_Block;           return .BLOCK;
      case Node_Declaration;     return .DECLARATION;
      case Node_Assignment;      return .ASSIGNMENT;
      case Node_If_Statement;    return .IF_STATEMENT;
      case Node_While_Loop;      return .WHILE_LOOP;
      case Node_For_Loop;        return .FOR_LOOP;
      case;
        assert(false, "Invalid node type!");    
    }
    return xx 0;
}
