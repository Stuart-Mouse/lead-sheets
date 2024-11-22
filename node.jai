
// TODO: what if we just use the actual Type value as the tag for node type?
//       this would allow use to simplify alloc_node to just take the type
//       and then we do not need to maintain alloc_node as we add new node types
//       only reason to keep enum around would be to serialize, but I don't think we would never want to serialize the AST instead of just using the source

Node_Type :: enum {
    // expressions
    NUMBER;
    // LITERAL; // TODO
    
    IDENTIFIER;
    // SUBEXPRESSION;  // basically a 'local variable', but less opaque at AST level
    VARIABLE;       // externally declared variable
    PROCEDURE;
    
    OPERATION;
    PROCEDURE_CALL;
    MEMBER;
    SUBSCRIPT;
    
    ITERATOR;
    ITERATOR_INDEX;
    
    RANGE;
    
    // statements
    BLOCK;
    ASSIGNMENT;
    DECLARATION;
    IF_STATEMENT;
    WHILE_LOOP;
    FOR_LOOP;
}

Node :: struct {
    node_type:            Node_Type;
    // src_loc:              Source_Location;    // -1 for declared externally?
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
    using #as node_base: Node;
    name: string;
    resolved_type: enum { UNRESOLVED; EXTERNAL_VARIABLE; EXTERNAL_PROCEDURE; DECLARATION; }; 
    union {
        variable_index:   int;
        procedure_index:  int;
        declaration:      *Node;
    }
}

// Maybe we should just insert Node_Declaration's when we call set_variable or add_procedure?
// or maybe this should just be a flag on Node_Assignment?
Node_Declaration :: struct {
    left:   *Node_Identifier;
    right:  *Node;
}

Node_Procedure_Call :: struct {
    using #as node_base: Node;
    procedure_expression: *Node;
    arguments:            *Node;
    arguments_count:      int;
    return_ptr:           *void; // will point to some memory allocated in script pool if return type is an aggregate type
}

Node_Number :: struct {
    using #as node_base: Node;
    float_value: float;
}

Node_Range :: struct {
    using #as node_base: Node;
    lower: *Node;
    upper: *Node;
}

Node_Operation :: struct {
    using #as node_base: Node;
    name:            string;     // TODO: hmmm...
    operator_index:  int;
    left, right:     *Node;
    return_ptr:      *Node;      // only if return type is aggregate
    // TODO: just point to procedure for overloaded operators?
}

Node_Member :: struct {
    using #as node_base: Node;
    base_expression:    *Node;
    identifier:         string;
    offset:             int;
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
    control_expression: *Node;
    statement: *Node;
    
    iterator_ptr: *void;       // points to some value allocated in pool, iterator node will get a copy of this during typecheck 
    iterator_index: int; // backing value. iterator_index node inside loop will get pointer to this.
}

for_expansion :: (node: *Node, body: Code, flags: For_Flags) #expand {
    #if flags & .REVERSE {
        // no it_index when iterating in reverse. not sure how that would really work
        `it := node;
        while it != null {
            #insert body;
            it = it.prev;
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

// TODO
// sprint_ast_node :: (script: *Script, node: Node) -> string {
//     if node.node_type == {
//       case .NUMBER;
//         return tprint(".NUMBER: %", node.number);
        
//       case .VARIABLE;
//         return tprint(".VARIABLE: %", node.src_text);
        
//       case .OPERATION;
//         if script.flags & .TYPECHECKED {
//             return tprint(".OPERATION: %", BUILTIN_OPERATIONS[node.operation.operator_index].operator_ident);
//         } else {
//             return node.src_text;
//         }
        
//       case .PROCEDURE_CALL;
//         if script.flags & .TYPECHECKED {
//             // return tprint(".PROCEDURE_CALL: %", node_get_procedure(script, node).name);
//             return tprint(".PROCEDURE_CALL: %", as_type(node.procedure_call.procedure_expression.value_type));
//         } else {
//             return node.src_text;
//         }
        
//       case .PROCEDURE;
//         return tprint(".PROCEDURE: %", node.src_text);
        
//       case .IF_STATEMENT;
//         return tprint(".IF_STATEMENT: if %  %", sprint_ast_node(script, node.if_statement.condition), sprint_ast_node(script, node.if_statement.statement));
        
//       case .WHILE_LOOP;
//         return tprint(".WHILE_LOOP: while %  %", sprint_ast_node(script, node.while_loop.condition), sprint_ast_node(script, node.while_loop.statement));
        
//       case .FOR_LOOP;
//         return tprint(".FOR_LOOP: for %  %", sprint_ast_node(script, node.while_loop.condition), sprint_ast_node(script, node.while_loop.statement));
        
//       case .BLOCK;
//         return "{ ... }";
        
//       case;
//         return tprint("%: ??? ", node.type);
//     }
//     return "";
// }


// Two allocate_node functions currently. 
// May possibly remove the first if we get rid of need for Node_Type enum.

alloc_node :: (ctxt: *Script_Constructor, node_type: Node_Type, parent_node: *Node = null, loc := #caller_location) -> *Node {
    node: *Node;
    
    // have to allocate the proper amount of space for the node
    if node_type == {
      case .NUMBER;          node = New(Node_Number,,         ctxt.allocator);
      case .IDENTIFIER;      node = New(Node_Identifier,,     ctxt.allocator);
      case .DECLARATION;     node = New(Node_Declaration,,    ctxt.allocator);
      case .VARIABLE;        node = New(Node_Variable,,       ctxt.allocator);
      case .OPERATION;       node = New(Node_Operation,,      ctxt.allocator);
      case .PROCEDURE_CALL;  node = New(Node_Procedure_Call,, ctxt.allocator);
      case .MEMBER;          node = New(Node_Member,,         ctxt.allocator);
      case .SUBSCRIPT;       node = New(Node_Subscript,,      ctxt.allocator);
      case .RANGE;           node = New(Node_Range,,          ctxt.allocator);
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

alloc_node :: (ctxt: *Script_Constructor, $T_Node: Type, parent_node: *Node = null, loc := #caller_location) -> *T_Node #expand
#modify {
    ti := cast(*Type_Info_Struct) T_Node;
    return ti.type == .STRUCT && ti.members[0].type == type_info(Node) && (ti.members[0].flags & (.USING | .AS)) == (.USING | .AS);
} {
    node := New(T_Node,, ctxt.allocator);
    node.node_type = #run type_to_enum(T_Node);
    node.parent    = ifx parent_node else ctxt.current_parent_node;
    return node;
}


// Conversion functions to go back and forth between Node_Type and corresponding Type

enum_to_type :: (node_type: Node_Type) -> Type {
    if #complete node_type == {
      case .NUMBER;          return Node_Number;
      case .IDENTIFIER;      return Node_Identifier;
      case .DECLARATION;     return Node_Declaration;
      case .OPERATION;       return Node_Operation;
      case .PROCEDURE_CALL;  return Node_Procedure_Call;
      case .MEMBER;          return Node_Member;
      case .SUBSCRIPT;       return Node_Subscript;
      case .RANGE;           return Node_Range;
      case .BLOCK;           return Node_Block;
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
      case Node_Number;          return .NUMBER;
      case Node_Identifier;      return .IDENTIFIER;
      case Node_Declaration;     return .DECLARATION;
      case Node_Operation;       return .OPERATION;
      case Node_Procedure_Call;  return .PROCEDURE_CALL;
      case Node_Member;          return .MEMBER;
      case Node_Subscript;       return .SUBSCRIPT;
      case Node_Range;           return .RANGE;
      case Node_Block;           return .BLOCK;
      case Node_Assignment;      return .ASSIGNMENT;
      case Node_If_Statement;    return .IF_STATEMENT;
      case Node_While_Loop;      return .WHILE_LOOP;
      case Node_For_Loop;        return .FOR_LOOP;
      case;
        assert(false, "Invalid node type!");    
    }
    return xx 0;
}

// replace_node :: (old: *Node, new: *Node) {
//     new.prev   = old.prev;
//     new.next   = old.next;
//     new.parent = old.parent;
    
//     old.prev.next = new;
//     old.next.prev = new;
    
//     // TODO: if we actually end up needing to use this proceudre, then figure out what to do about parent/child references to node
//     // frankly this seems like something we just should not do...
    
//     // // the lame part, we need to maintain logic here for fixing up all other places where old may be referenced
//     // // perhaps it would still be better to use some union for nodes so that we can do this more easily...
//     // if old.parent {
//     //     if old.parent.type == {
//     //       case .BLOCK;
//     //         block := cast(*Node_Block) old.parent;
//     //         if block.statements == old  block.statements = new;
            
            
//     //       case .IF_STATMENT;
//     //         if_statement := cast(*Node_If_Statement) old.parent;
//     //         if if_statement.condition == old  if_statement.condition = new;
//     //         if if_statement.statement == old  if_statement.statement = new;
            
//     //       case .WHILE_LOOP;
//     //         while_loop := cast(*Node_If_Statement) old.parent;
//     //         if while_loop.condition == old  while_loop.condition = new;
//     //         if while_loop.statement == old  while_loop.statement = new;
            
//     //     }
//     // } 
    
// }
