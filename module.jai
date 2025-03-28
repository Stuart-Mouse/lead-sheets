
#import "Basic";
#import "Math";
#import "String";
#import "Hash_Table";
#import "Flat_Pool";
#import "Reflection";
#import "Compiler";     // for 'is_subclass_of'

#import "Data_Packer";  // for 'remap_data'
#import "dyncall";

#load "lexer.jai";
#load "parse.jai";
#load "node.jai";
#load "typecheck.jai";
#load "execute.jai";
#load "procedure.jai";
#load "binary_op.jai";
#load "serialize.jai";
#load "evaluate.jai";


#module_parameters (DEBUG := false);


External_Variable :: struct {
    name:       string;
    binding:    Any;
}

Name_And_Type :: struct {
    name:   string; 
    type:   *Type_Info;
}

Script :: struct {
    ast_root:               *Node_Block;
    flags:                  Flags;
    
    variables:              [..] External_Variable;
    procedures:             [..] External_Procedure;
    directives:             [..] Directive;
    named_blocks:           [..] *Node_Block;
    type_table:             [..] Name_And_Type;
    
    // added for the sake of convenience and use in repl
    global_declarations:    [..] *Node_Declaration;
    
    pool:                   Flat_Pool;
    allocator:              Allocator;
    
    stack_base:             *void;
    stack_size:             int;
    stack_ptr:              *void;
    
    dyncall_vm:             *DCCallVM;
    
    lexer:                  Lexer;
    current_scope:          *Node;    // perhaps this should be passed on stack, only used during parsing
    
    
    Flags :: enum_flags {
        INITIALIZED;
        PARSED;
        TYPECHECKED;
    }
}

get_pool_allocator :: (script: *Script) -> Allocator {
    return .{ flat_pool_allocator_proc, *script.pool };
}

free_script :: (using script: *Script) {
    array_free(variables);
    array_free(procedures);
    array_free(directives);
    array_free(type_table);
    fini(*pool);
    if script.dyncall_vm  dcFree(script.dyncall_vm);
    
    script.* = .{};
}

init_script :: (script: *Script, stack_size := 1024) {
    script.allocator = get_pool_allocator(script);
    
    script.stack_size = stack_size;
    script.stack_base = get(*script.pool, script.stack_size + size_of(type_of(STACK_SENTINEL_VALUE)));
    script.stack_ptr  = script.stack_base;
    sentinel := (script.stack_base + script.stack_size).(*type_of(STACK_SENTINEL_VALUE));
    sentinel.* = STACK_SENTINEL_VALUE;
    
    // allocate dyncall vm that will be reused
    script.dyncall_vm = dcNewCallVM(2048);
    dcMode(script.dyncall_vm, DC_CALL_C_DEFAULT);
    dcReset(script.dyncall_vm);
    
    script.ast_root = alloc_node(script, Node_Block);
    script.current_scope = script.ast_root;
    
    script.flags |= .INITIALIZED;
}

// ensures no name conflicts
set_variable :: (script: *Script, name: string, any: Any) {
    for *script.variables {
        if it.name == name {
            it.binding = any;
            return;
        }
    }
    array_add(*script.variables, .{ name, any });
}

// ensures no name conflicts
register_type :: (script: *Script, name: string, type: *Type_Info) {
    for *script.type_table {
        if it.name == name {
            log("Warning: attempt register a type with same name as previously registered type.");
            return;
        }
    }
    array_add(*script.type_table, .{ name, type });
}

get_named_block :: (script: *Script, name: string) -> *Node_Block {
    for script.named_blocks
        if it.name == name
            return it;
    return null;
}


// STACK STUFF

// TODO: instead of having this goofy sentinel value thing on the stack, we should just assert that stack_ptr < stack_base + stack_size after pushing to stack
STACK_SENTINEL_VALUE: u64 : 0x0123_4567_89AB_CDEF;

PTR_SIZE :: size_of(*void);

stack_pop :: inline (script: *Script, $$is_lvalue := false) -> *void {
    script.stack_ptr -= PTR_SIZE;
    assert(script.stack_ptr >= script.stack_base, "stack_ptr offset: %", script.stack_ptr - script.stack_base);
    if is_lvalue {
        return script.stack_ptr.(**void).*;
    } else {
        return script.stack_ptr;
    }
}

stack_peek :: inline (script: *Script, $$is_lvalue := false) -> *void {
    stack_ptr := script.stack_ptr - PTR_SIZE;
    assert(stack_ptr >= script.stack_base);
    if is_lvalue {
        return stack_ptr.(**void).*;
    } else {
        return stack_ptr;
    }
}

stack_push_any :: (script: *Script, any: Any, is_lvalue := false) -> bool {
    if script.stack_ptr >= script.stack_base + script.stack_size {
        log("Runtime Error: ran out of space on stack!\n");
        return false;
    }
    defer script.stack_ptr += PTR_SIZE;
    
    if is_aggr(any.type) || is_lvalue {
        script.stack_ptr.(**void).* = any.value_pointer; // store the pointer itself instead of the value
    } else {
        assert(any.type.runtime_size <= PTR_SIZE);
        memcpy(script.stack_ptr, any.value_pointer, PTR_SIZE); // copy the value being pointed at for register-sized values (INT, FLOAT, ENUM)
    }
    // dprint("wrote % to stack at %\n", << cast(**void)script.stack_ptr, script.stack_ptr);
    return true;
}

stack_push_zeroes :: (script: *Script, size: int) -> bool {
    if script.stack_ptr + size >= script.stack_base + script.stack_size {
        log("Runtime Error: ran out of space on stack!\n");
        return false;
    }
    
    dprint("stack ptr: %\n", script.stack_ptr);
    memset(script.stack_ptr, 0, size);
    script.stack_ptr += size;
    return true;
}

stack_pop_any :: inline (script: *Script, ti: *Type_Info, is_lvalue := false) -> Any {
    assert(script.stack_ptr >= script.stack_base);
    
    script.stack_ptr -= PTR_SIZE;
    if is_aggr(ti) || is_lvalue {
        return Any.{ ti, script.stack_ptr.(**void).* }; // additional dereference to get pointer to value
    } else {
        assert(ti.runtime_size <= PTR_SIZE);
        return Any.{ ti, script.stack_ptr };
    }
}

stack_peek_any :: inline (script: *Script, ti: *Type_Info, is_lvalue := false) -> Any {
    assert(script.stack_ptr >= script.stack_base);
    ptr := script.stack_ptr - PTR_SIZE;
    
    if is_aggr(ti) || is_lvalue {
        return Any.{ ti, ptr.(**void).* }; // additional dereference to get pointer to value
    } else {
        assert(ti.runtime_size <= PTR_SIZE);
        return Any.{ ti, ptr };
    }
}

make_number_literal :: (script: *Script, token: Token) -> *Node {
    value, ok := parse_number(token.text);
    if !ok  return null;
    
    // TODO: copy extra token stuff onto node as well
    literal := alloc_node(script, Node_Literal);
    literal.literal_type = .NUMBER;
    literal.number = value;
    return literal;
}

// TODO: need to have some kind of flag on such a node so that we know whether we need to print the fully qualified enum name with type or not
//       adn that will depend on whether this node is held by a node_identifier/node_dot or not
//       if this nod eis held by a node_identifier or node_dot, then it actually won't get directly called by print_node anyhow though
//       since the identifier will just print itself with get_identifier_name. so i guess it's actually just fine to always use the fully qualified enum name if we print an enum literal?
//       or maybe we use a flag like .IMPLICIT to signify that the enum literal was using a unary dot in the source text.
// TODO: we can't use make_literal to make a STRUCT literal since that would require providing values as expressions
//       so this will only work for basically numbers, strings, and enums
make_literal :: (script: *Script, value: Any) -> *Node_Literal {
    literal := alloc_node(script, Node_Literal);
    if value.type.type == .STRING {
        literal.literal_type = .STRING;
        literal.text = value.value_pointer.(*string).*;
    }
    else if value.type.type == .ENUM || is_numeric_type(value.type) {
        literal.literal_type = .NUMBER;
        literal.number = Small_Any.{ value.type, value.value_pointer.(**void).* };
    }
    literal.value_type = value.type;
    return literal;
}

get_literal_value_as_any :: (literal: Node_Literal) -> Any {
    assert(literal.value_type != null);
    if #complete literal.literal_type == {
      case .STRING; return Any.{ type_info(string), *literal.text };
      case .NUMBER; return to_any(literal.number);
      case .STRUCT; return Any.{ literal.value_type, literal.aggr.value_pointer };
      case .ANY;    return literal.any;
    }
    assert(false, "unreachable");
}


make_number_literal :: (script: *Script, number: float64) -> *Node {
    literal := alloc_node(script, Node_Literal);
    literal.literal_type = .NUMBER;
    literal.number = make_small_any(number);
    return literal;
}

// TODO: add source location parameter. we should actually do this for all node types
//       actually make versions of make_xxx procs that just take token for simplicity sake
make_identifier :: (script: *Script, name: string) -> *Node_Identifier {
    node := alloc_node(script, Node_Identifier);
    node.name = name;
    return node;
}



// uses an extra level of indirection so that we can replace any pointer to a node
// I removed this funcitonality from the typecheking step, but it should work better here
// walk_nodes :: (script: *Script, node: **Node, body: Code) -> bool #expand {
//     recurse :: (script: *Script, _node: **Node) -> bool {
//         node := _node.*;
//         if node == null {
//             dprint("Warning: node was null in walk_nodes.\n");
//             return true; // TODO: make false later probably...
//         }
        
//         replace_node :: (node: *Node) #expand { replace_node(`_node, node); };
        
//         #insert,scope() body;
        
//         // recurse for certain node types
//         if node.node_type == {
//           case Node_Operation;
//             operation := node.(*Node_Operation);
//             return recurse(script, xx *operation.left) 
//                 && recurse(script, xx *operation.right);
            
//           case Node_Procedure_Call;
//             procedure_call := node.(*Node_Procedure_Call);
//             iterate(*procedure_call.arguments, #code {
//                 if !recurse(script, it)  return false;
//             });
//             return true;
            
//           case Node_Dot;
//             dot := node.(*Node_Dot);
//             return recurse(script, xx *dot.left) 
//                 && recurse(script, xx *dot.right);
            
//           case Node_Subscript;
//             subscript := node.(*Node_Subscript);
//             return recurse(script, xx *subscript.base_expression)
//                 && recurse(script, xx *subscript.indexing_expression);
            
//           case Node_Declaration;
//             declaration := node.(*Node_Declaration);
//             return recurse(script, xx *declaration.type_expression)
//                 && recurse(script, xx *declaration.init_expression);
            
//           case Node_Block;
//             block := node.(*Node_Block);
//             iterate(*block.statements, #code {
//                 if !recurse(script, it)  return false;
//             });
//             return true;
            
//           case Node_If_Statement;
//             if_statement := node.(*Node_If_Statement);
//             return recurse(script, xx *if_statement.condition)
//                 && recurse(script, xx *if_statement.statement);
            
//           case Node_While_Loop;
//             while_loop := node.(*Node_While_Loop);
//             return recurse(script, xx *while_loop.condition)
//                 && recurse(script, xx *while_loop.statement);
            
//           case Node_For_Loop;
//             for_loop := node.(*Node_For_Loop);
//             if for_loop.control_type == {
//               case .RANGE;
//                 if !recurse(script, xx *for_loop.range.lower)  return false;
//                 if !recurse(script, xx *for_loop.range.upper)  return false;
                
//               case .ARRAY;
//                 if !recurse(script, xx *for_loop.array_expression)  return false;
//             }
//             if !recurse(script, xx *for_loop.statement)  return false;
//             return true;
//         }
        
//         return true;
//     }
    
//     return recurse(script, node);
// }

Walk_Nodes_Callback :: #type (script: *Script, node: *Node, data: *void) -> (bool, *Node);
walk_nodes :: (script: *Script, _node: **Node, callback: Walk_Nodes_Callback, data: *void) -> bool {
    node := _node.*;
    if node == null {
        dprint("Warning: node was null in walk_nodes.\n");
        return true; // TODO: make false later probably...
    }
    
    ok, replacement := callback(script, node, data);
    if !ok  return false;
    if replacement  replace_node(_node, replacement);
    
    // recurse for certain node types
    if node.node_type == {
      case Node_Operation;
        operation := node.(*Node_Operation);
        return walk_nodes(script, xx *operation.left, callback, data) 
            && walk_nodes(script, xx *operation.right, callback, data);
        
      case Node_Procedure_Call;
        procedure_call := node.(*Node_Procedure_Call);
        for 0..procedure_call.arguments.count-1 {
            node_ptr_ptr := *procedure_call.arguments[it];
            if !walk_nodes(script, node_ptr_ptr, callback, data)  return false;
        }
        return true;
        
      case Node_Dot;
        dot := node.(*Node_Dot);
        return walk_nodes(script, xx *dot.left, callback, data) 
            && walk_nodes(script, xx *dot.right, callback, data);
        
      case Node_Cast;
        node_cast := node.(*Node_Cast);
        if !walk_nodes(script, xx *node_cast.value, callback, data)  return false;
        if !(node_cast.flags & .IMPLICIT) {
            if !walk_nodes(script, xx *node_cast.type_expression, callback, data)  return false;
        }
        
      case Node_Subscript;
        subscript := node.(*Node_Subscript);
        return walk_nodes(script, xx *subscript.base_expression, callback, data)
            && walk_nodes(script, xx *subscript.indexing_expression, callback, data);
        
      case Node_Declaration;
        declaration := node.(*Node_Declaration);
        return walk_nodes(script, xx *declaration.type_expression, callback, data)
            && walk_nodes(script, xx *declaration.init_expression, callback, data);
        
      case Node_Block;
        block := node.(*Node_Block);
        for 0..block.statements.count-1 {
            node_ptr_ptr := *block.statements[it];
            if !walk_nodes(script, node_ptr_ptr, callback, data)  return false;
        }
        return true;
        
      case Node_If_Statement;
        if_statement := node.(*Node_If_Statement);
        return walk_nodes(script, xx *if_statement.condition, callback, data)
            && walk_nodes(script, xx *if_statement.statement, callback, data);
        
      case Node_While_Loop;
        while_loop := node.(*Node_While_Loop);
        return walk_nodes(script, xx *while_loop.condition, callback, data)
            && walk_nodes(script, xx *while_loop.statement, callback, data);
        
      case Node_For_Loop;
        for_loop := node.(*Node_For_Loop);
        if for_loop.control_type == {
          case .RANGE;
            if !walk_nodes(script, xx *for_loop.range.lower, callback, data)  return false;
            if !walk_nodes(script, xx *for_loop.range.upper, callback, data)  return false;
            
          case .ARRAY;
            if !walk_nodes(script, xx *for_loop.array_expression, callback, data)  return false;
        }
        if !walk_nodes(script, xx *for_loop.statement, callback, data)  return false;
        return true;
    }
    
    return true;
}



// CONVENIENCE STUFF

// user can append to this as they wish, or just change it here
common_types: [..] struct { name: string; type: *Type_Info; } = .[
    .{ "u8",      type_info(u8)      }, 
    .{ "u16",     type_info(u16)     }, 
    .{ "u32",     type_info(u32)     }, 
    .{ "u64",     type_info(u64)     },
    .{ "s8",      type_info(s8)      }, 
    .{ "s16",     type_info(s16)     }, 
    .{ "s32",     type_info(s32)     }, 
    .{ "s64",     type_info(s64)     },
    .{ "int",     type_info(int)     }, 
    .{ "float",   type_info(float)   }, 
    .{ "float32", type_info(float32) }, 
    .{ "float64", type_info(float64) },
];

register_common_types :: (script: *Script) {
    for common_types  register_type(script, it.name, it.type);
}






#scope_module

memzero :: (t: *$T) { memset(t, 0, size_of(T)); };

is_numeric_type :: (t: Type) -> bool {
    ti := t.(*Type_Info);
    return ti.type == .INTEGER || ti.type == .FLOAT;
}

is_numeric_type :: (ti: *Type_Info) -> bool {
    return ti.type == .INTEGER || ti.type == .FLOAT;
}


/* 
    Small_Any is an Any, but we only use it for small values (<= 8 bytes), 
        which get packed into the space of the value_pointer.
    Maybe in the future I will also use this to double as a standard any, when we need to point to larger values.
*/
Small_Any :: #type,distinct Any; 

// kind of feels like this should have ot be passed by pointer, but it works?
// and there's a strange error when I do it that way, so...
to_any :: inline (any: Small_Any) -> Any { 
    return Any.{ any.type, *any.value_pointer }; 
};

get_as_type :: inline (any: Small_Any, $T: Type) -> T, bool #modify {
    return T.(*Type_Info).runtime_size <= 8;
} { 
    return (*any.value_pointer).(*T).*, any.type == type_info(T);
}

make_small_any :: inline (value: $T) -> Small_Any #modify {
    return T.(*Type_Info).runtime_size <= 8;
} { 
    // init with zerp and then copy value just so we don't get some weird random bytes copied over somehow
    ret := Small_Any.{ type_info(T), null }; 
    (*ret.value_pointer).(*T).* = value;
    return ret;
}

// NOTE: this would not work with dynamic type infos, 
//       as that would require a more complex switch case
//       on type of type, runtime size, and signedness
negate_number :: inline (any: *Small_Any) {
    if as_type(any.type) == {
      // case u\8;     val := (*any.value).(*u\8);     val.* = -val.*;
      // case u16;     val := (*any.value).(*u16);     val.* = -val.*;
      // case u32;     val := (*any.value).(*u32);     val.* = -val.*;
      // case u64;     val := (*any.value).(*u64);     val.* = -val.*;
      case s\8;     val := (*any.value).(*s\8);     val.* = -val.*;
      case s16;     val := (*any.value).(*s16);     val.* = -val.*;
      case s32;     val := (*any.value).(*s32);     val.* = -val.*;
      case s64;     val := (*any.value).(*s64);     val.* = -val.*;
      case float32; val := (*any.value).(*float32); val.* = -val.*;
      case float64; val := (*any.value).(*float64); val.* = -val.*;
      
      case; assert(false, "Small_Any passed to negate_number was not a valid numeric type! (was %)", as_type(any.type));
    }
}



// copied from how_to/170_modify.jai
Dynamic_New :: (ti: *Type_Info, initialized := true) -> *void {
    size := ti.runtime_size;
    if size < 0 return null;   // Maybe they gave us a weird polymorphic type for some reason.

    memory := alloc(size);

    if initialized {
        if ti.type == .STRUCT {
            // Structs can have initializers. If the initializer is null, the struct is to be zeroed.
            tis := ti.(*Type_Info_Struct);
            if tis.initializer  tis.initializer(memory);
            else                memset(memory, 0, size);
        } else {
            // Non-structs always default to 0.
            // @Incomplete: Here we are not handling variants, or arrays of things that might have initializers.
            memset(memory, 0, size);
        }
    }

    return memory;
}

Dynamic_New_Any :: inline (ti: *Type_Info, initialized := true) -> Any {
    memory := Dynamic_New(ti, initialized);
    return ifx memory then Any.{ ti, memory } else Any.{};
}


as_type :: inline (ti: *Type_Info) -> Type {
    return (*ti).(*Type).*;
}

enum_name_to_value :: (ti: *Type_Info_Enum, name: string) -> s64, bool {
    assert(ti.type == .ENUM);
    for ti.names  if name == it  return ti.values[it_index], true;
    return 0, false;
}

enum_value_to_name :: (ti: *Type_Info_Enum, value: s64) -> string {
    assert(ti.type == .ENUM);
    for ti.values  if value == it  return ti.names[it_index];
    return "";
}



#if DEBUG {#add_context dprint_indent: int;}

dprint :: (format_string: string, args: .. Any) {
    #if DEBUG {
        for 0..context.dprint_indent  print("  ");
        print(format_string, ..args);
    }
}
dprint_push_indent :: () #expand {
    #if DEBUG {
        context.dprint_indent += 1;
        `defer context.dprint_indent -= 1;
    }
}


// implement is_this_yours for flat pool allocator
is_this_yours :: (pool: *Flat_Pool, memory: *void) -> bool {
    return flat_pool_allocator_proc(.IS_THIS_YOURS, 0, 0, memory, pool).(bool);
}

