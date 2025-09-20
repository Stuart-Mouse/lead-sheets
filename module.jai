
External_Variable :: struct {
    name:       string;
    binding:    Any;
}

Name_And_Type :: struct {
    name:   string; 
    type:   *Type_Info;
}

// typechecking callbcak allows additional return value for replacement node
Typecheck_Callback :: type_of(typecheck_node);
Execute\ _Callback :: type_of(execute_node);
Evaluate\_Callback :: type_of(evaluate_node);
Serialize_Callback :: type_of(sprint_node);

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
    
    error:                  Error;
    
    stack_base:             *void;
    stack_size:             int;
    stack_ptr:              *void;
    
    #if USING_DYNCALL {
        dyncall_vm:             *DCCallVM;
    }
    
    lexer:                  Lexer;
    current_scope:          *Node;    // perhaps this should be passed on stack, only used during parsing
    
    // callbacks for handling custom node types
    typecheck_callback:     Typecheck_Callback;
    execute\ _callback:     Execute\ _Callback;
    evaluate\_callback:     Evaluate\_Callback;
    serialize_callback:     Serialize_Callback;
    
    // to make it easy to override parsing procedures individually, while still being able to make use of many of the convenient higher-order procs like parse_comma_separated_expressions.
    my_parse_expression := parse_expression;
    my_parse_binary     := parse_binary;
    my_parse_leaf       := parse_leaf;
    
    Flags :: enum_flags {
        INITIALIZED;
        PARSED;
        TYPECHECKED;
    }
}

free_script :: (using script: *Script) {
    array_reset(*variables);
    array_reset(*procedures);
    array_reset(*named_blocks);
    array_reset(*directives);
    array_reset(*type_table);
    array_reset(*global_declarations);
    fini(*pool);
    #if USING_DYNCALL {
        if script.dyncall_vm {
            dcFree(script.dyncall_vm);
            script.dyncall_vm = null;
        }
    }
}

init_script :: (script: *Script, stack_size := 1024) {
    script.* = .{};
    init(*script.pool);
    
    script.named_blocks.allocator = context.allocator;
    
    // TODO: move elsewhere. This is a hack to prevent the lookup table from getting initialized with the temporary allocator.
    // if !DCaggr_lookup_table {
    //     DCaggr_lookup_table.allocator = context.allocator;
    // }
    
    script.stack_size = stack_size;
    script.stack_base = get(*script.pool, script.stack_size);
    script.stack_ptr  = script.stack_base;
    
    // allocate dyncall vm that will be reused
    #if USING_DYNCALL {
        script.dyncall_vm = dcNewCallVM(2048);
        dcMode(script.dyncall_vm, DC_CALL_C_DEFAULT);
        dcReset(script.dyncall_vm);
    }
    
    script.ast_root = alloc_node(script, Node_Block);
    script.current_scope = script.ast_root;
    
    script.flags |= .INITIALIZED;
}

// copy_script_static_variables :: (dst: *Script, src: *Script) {
//     free_script(dst); // just in case data is already there...
//     dst.* = .{};
    
//     init(*dst.pool);
    
//     // TODO: how to copy all of syntax tree...
//     // this is annoying because we will have to clone the whole tree recursively and 
    
// }

// ensures no name conflicts
set_variable :: (script: *Script, name: string, any: Any, replacement_value := false) {
    for *script.variables {
        if it.name == name {
            it.binding = any;
            return;
        }
    }
    if replacement_value {
        log("Called set_variable with replacement_value flag, but no such pre-existing variable '%'.", name);
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

PTR_SIZE :: size_of(*void);

stack_pop :: inline (script: *Script, $$is_lvalue := false) -> *void {
    script.stack_ptr -= PTR_SIZE;
    assert(script.stack_ptr >= script.stack_base, "script stack pointer was below stack base!");
    if is_lvalue {
        return script.stack_ptr.(**void).*;
    } else {
        return script.stack_ptr;
    }
}

stack_peek :: inline (script: *Script, $$is_lvalue := false) -> *void {
    stack_ptr := script.stack_ptr - PTR_SIZE;
    assert(stack_ptr >= script.stack_base, "script stack pointer was below stack base!");
    if is_lvalue {
        return stack_ptr.(**void).*;
    } else {
        return stack_ptr;
    }
}

stack_push_any :: (script: *Script, any: Any, is_lvalue := false) -> bool {
    if script.stack_ptr >= script.stack_base + script.stack_size {
        set_exec_error(script, "Ran out of space on stack!", node = null);
        return false;
    }
    defer script.stack_ptr += PTR_SIZE;
    
    if is_aggr(any.type) || is_lvalue {
        script.stack_ptr.(**void).* = any.value_pointer; // store the pointer itself instead of the value
    } else {
        assert(any.type.runtime_size <= PTR_SIZE);
        memcpy(script.stack_ptr, any.value_pointer, PTR_SIZE); // copy the value being pointed at for register-sized values (INT, FLOAT, ENUM)
    }
    return true;
}

stack_push_zeroes :: (script: *Script, size: int) -> bool {
    if script.stack_ptr + size >= script.stack_base + script.stack_size {
        set_exec_error(script, "Ran out of space on stack!", node = null);
        return false;
    }
    
    memset(script.stack_ptr, 0, size);
    script.stack_ptr += size;
    return true;
}

stack_pop_any :: inline (script: *Script, ti: *Type_Info, is_lvalue := false) -> Any {
    script.stack_ptr -= PTR_SIZE;
    assert(script.stack_ptr >= script.stack_base, "script stack pointer was below stack base!");
    if is_aggr(ti) || is_lvalue {
        return Any.{ ti, script.stack_ptr.(**void).* }; // additional dereference to get pointer to value
    } else {
        assert(ti.runtime_size <= PTR_SIZE);
        return Any.{ ti, script.stack_ptr };
    }
}

stack_peek_any :: inline (script: *Script, ti: *Type_Info, is_lvalue := false) -> Any {
    ptr := script.stack_ptr - PTR_SIZE;
    assert(script.stack_ptr >= script.stack_base, "script stack pointer was below stack base!");
    if is_aggr(ti) || is_lvalue {
        return Any.{ ti, ptr.(**void).* }; // additional dereference to get pointer to value
    } else {
        assert(ti.runtime_size <= PTR_SIZE);
        return Any.{ ti, ptr };
    }
}



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


#module_parameters ()(DEBUG := false, USING_DYNCALL := true);

#load "node.jai";
#load "lexer.jai";
#load "parse.jai";
#load "typecheck.jai";
#load "execute.jai";
#load "evaluate.jai";
#load "serialize.jai";

#load "procedure.jai";
#load "directive.jai";
#load "operator.jai";

#load "result.jai";


#scope_module

#import "Basic";
#import "Math";
#import "String";
using,except(next_power_of_two) Hash_Table :: #import "Hash_Table";
#import "Flat_Pool";
#import "Reflection";
#import "Compiler";     // for 'is_subclass_of'

#import "Utils";
Convert :: #import "Convert";

#if USING_DYNCALL {
    #import "dyncall";
}

#if DEBUG { #add_context dprint_indent: int; }

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


// ========== Flat Pool ==========

// Moved these helper procs out of Utils module, will just have to duplicate it into other modules when needed.

// is_this_yours for flat pool allocator
is_this_yours :: (pool: *Flat_Pool, memory: *void) -> bool {
    return flat_pool_allocator_proc(.IS_THIS_YOURS, 0, 0, memory, pool).(bool);
}

copy_to_pool_if_needed :: (pool: *Flat_Pool, any: Any) -> Any {
    if is_this_yours(pool, any.value_pointer)  return any;
    
    copy := New_Any(any.type, initialized = false,, get_pool_allocator(pool));
    memcpy(copy.value_pointer, any.value_pointer, any.type.runtime_size);
    return copy;
}

copy_to_pool_if_needed :: (pool: *Flat_Pool, value: *$T) -> *T {
    if is_this_yours(pool, value)  return value;
    
    copy := get(pool, size_of(T)).(*T);
    copy.* = value.*;
    return copy;
}

get_pool_allocator :: (pool: *Flat_Pool) -> Allocator {
    return .{ flat_pool_allocator_proc, pool };
}


