
Script :: struct {
    ast_root:               *Node_Block;
    flags:                  Flags;
    
    variables:              [..] External_Variable;
    procedures:             [..] External_Procedure;
    directives:             [..] Directive;
    type_table:             [..] Name_And_Type;
    operator_table:         [..] Operator_Info;
    
    // added for the sake of convenience and use in repl
    global_declarations:    [..] *Node_Declaration;
    
    pool:                   Flat_Pool;
    error:                  Error;
    stack:                  Stack;
    
    #if USING_DYNCALL {
        dyncall_vm:         *DCCallVM;
    }
    
    lexer:                  Lexer;
    
    
    current_scope:          *Node_Block;
    
    // if this is set while evaluating a block, we need to keep breaking until we hit this target
    break_target:           *Node;
    break_result_storage:   *void;
    
    
    // to make it easy to override parsing procedures individually, while still being able to make use of many of the convenient higher-order procs like parse_comma_separated_expressions.
    my_parse_expression := parse_expression;
    my_parse_binary     := parse_binary;
    my_parse_leaf       := parse_leaf;
    
    my_typecheck_node   := typecheck_node;
    my_is_valid_lvalue  := is_valid_lvalue;
    
    my_evaluate_node    := evaluate_node;
    
    my_print_node       := print_node;
    
    resolve_identifier_in_global_scope := default_resolve_identifier_in_global_scope;
    
    
    // NOTE: get_virtual_member gets passed the script, so implicitly they're also getting the get_virtual_member_user_data pointer
    get_virtual_member:             (*Script, Any, string) -> bool, Any;
    get_virtual_member_user_data:   *void;
    
    declare_virtual_member:             (*Script, Any, string, *Type_Info) -> bool, Any, bool;
    declare_virtual_member_user_data:   *void;
    
    Flags :: enum_flags {
        INITIALIZED;
        PARSED;
        TYPECHECKED;
        
        // TODO: this is basically a hack to make use of a script as a debug console work 
        //       we need a better long term solution that manages storage for declarations more intelligently
        //       one solution may be to use a multi-stack model for storing declarations in open blocks
        ALWAYS_ALLOCATE_FOR_GLOBAL_DECLARATIONS;
    }
}

deinit_script :: (using script: *Script) {
    array_reset(*variables);
    array_reset(*procedures);
    array_reset(*directives);
    array_reset(*type_table);
    array_reset(*operator_table);
    array_reset(*global_declarations);
    fini(*pool);
    #if USING_DYNCALL {
        if script.dyncall_vm {
            dcFree(script.dyncall_vm);
            script.dyncall_vm = null;
        }
    }
}

init_script :: (script: *Script, stack_size := 2048) {
    script.* = .{};
    init(*script.pool);
    
    init_stack(*script.stack,, get_pool_allocator(*script.pool));
    
    // allocate dyncall vm that will be reused
    #if USING_DYNCALL {
        script.dyncall_vm = dcNewCallVM(2048);
        dcMode(script.dyncall_vm, DC_CALL_C_DEFAULT);
        dcReset(script.dyncall_vm);
    }
    
    script.ast_root = alloc_node(script, Node_Block);
    script.current_scope = script.ast_root;
    
    array_add(*script.operator_table, ..default_operator_table);
    
    script.flags |= .INITIALIZED;
}

has_error :: (script: *Script) -> bool { return script.error.type != .NO_ERROR; }
clear_error :: (script: *Script) { script.error = .{}; }



Name_And_Type :: struct {
    name: string;
    type: *Type_Info;
}

// ensures no name conflicts
register_type :: (script: *Script, name: string, type: Type) { register_type(script, name, type.(*Type_Info)); }
register_type :: (script: *Script, name: string, type: *Type_Info) {
    for *script.type_table {
        if it.name == name {
            log("Warning: attempt register a type with same name as previously registered type.");
            return;
        }
    }
    array_add(*script.type_table, .{ name, type });
}



Walk_Nodes_Callback :: #type (script: *Script, node: *Node, data: *void) -> (bool, *Node);
walk_nodes :: (script: *Script, _node: **Node, callback: Walk_Nodes_Callback, data: *void) -> bool {
    node := _node.*;
    if node == null {
        dprint("Warning: node was null in walk_nodes.");
        return true; // TODO: make false later probably...
    }
    
    ok, replacement := callback(script, node, data);
    if !ok  return false;
    if replacement  replace_node(_node, replacement);
    
    // recurse for certain node types
    if node.node_type == {
      case Node_Binary_Operator;
        operation := node.(*Node_Binary_Operator);
        return walk_nodes(script, xx *operation.left, callback, data) 
            && walk_nodes(script, xx *operation.right, callback, data);
            
      case Node_Unary_Operator;
        operation := node.(*Node_Unary_Operator);
        return walk_nodes(script, xx *operation.expression, callback, data);
        
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
            && walk_nodes(script, xx *if_statement.subcondition, callback, data)
            && walk_nodes(script, xx *if_statement.then_block, callback, data)
            && walk_nodes(script, xx *if_statement.else_block, callback, data);
        
      case Node_If_Expression;
        if_expression := node.(*Node_If_Expression);
        return walk_nodes(script, xx *if_expression.condition, callback, data)
            && walk_nodes(script, xx *if_expression.then_expression, callback, data)
            && walk_nodes(script, xx *if_expression.else_expression, callback, data);
            
      case Node_While_Loop;
        while_loop := node.(*Node_While_Loop);
        return walk_nodes(script, xx *while_loop.condition, callback, data)
            && walk_nodes(script, xx *while_loop.block, callback, data);
        
      case Node_For_Loop;
        for_loop := node.(*Node_For_Loop);
        if for_loop.control_type == {
          case .RANGE;
            if !walk_nodes(script, xx *for_loop.range.lower, callback, data)  return false;
            if !walk_nodes(script, xx *for_loop.range.upper, callback, data)  return false;
            
          case .ARRAY;
            if !walk_nodes(script, xx *for_loop.array_expression, callback, data)  return false;
        }
        if !walk_nodes(script, xx *for_loop.block, callback, data)  return false;
        return true;
    }
    
    return true;
}


/*
    Would be nice to implement an assertion_failed proc to use while we are parsing/typechecking/executing a script.
    We could store the current node being dealt with and print out some location information for that node, or something akin to a stack trace.
    This will only reall ybe super useful if the assertion_failed proc is also triggered for things like invalid array indexing, null pointer dereferences, and out-of-range integer casts as well. 
*/
script_assertion_failed :: (loc := Source_Code_Location, message: string) -> bool {
    write_loc(loc, to_standard_error = true);
	write_string(": Assertion failed while executing script", to_standard_error = true);
	
	// TODO: write info about node/location in script which we were dealing with when error ocurred
    
	if message {
		write_strings(": ", message, "\n", to_standard_error = true);
	} else {
		write_string("!\n", to_standard_error = true);
	}

    print_stack_trace :: (node: *Stack_Trace_Node) {  // @Copypasta from modules/Basic, but without calling print.
        while node {
            // There are two different line numbers available.
            // node.info.location has a line number, but this is the location of
            // the start of the procedure. That may be useful in some cases, but often
            // what we really want is the line number of the procedure call, which is
            // stored in node.line_number. That's what we print in this case.

            if node.info {
                write_string(node.info.location.fully_pathed_filename, to_standard_error = true);
                write_string(":", to_standard_error = true);
                write_nonnegative_number(node.line_number, to_standard_error = true);
                write_string(": ", to_standard_error = true);
                if node.info.name write_string(node.info.name, to_standard_error = true);
                else write_string("(anonymous procedure)", to_standard_error = true);
                write_string("\n", to_standard_error = true);
            }

            node = node.next;
        }
    }

    if context.stack_trace {
        write_string("\nStack trace:\n", to_standard_error = true);
        print_stack_trace(context.stack_trace);
    } else {
        write_string("Exiting.\n", to_standard_error = true);
    }

    __runtime_support_disable_stack_trace = true;

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
#load "evaluate.jai";
#load "serialize.jai";

#load "variable.jai";
#load "procedure.jai";
#load "directive.jai";
#load "operator.jai";
#load "stack.jai";

#load "result.jai";


#scope_module

#import "Basic";
#import "Math";
#import "String";
using,except(next_power_of_two) Hash_Table :: #import "Hash_Table";
#import "Flat_Pool";
#import "Compiler";     // for 'is_subclass_of'

using Utils :: #import "Utils";
#import "Reflect";
Convert :: #import "Convert";

#if USING_DYNCALL {
    #import "dyncall";
}

#if DEBUG { #add_context dprint: struct { indent: int; skip_indent: bool; }; }

dprint :: (format_string: string, args: ..Any, newline := true) {
    #if DEBUG {
        if !context.dprint.skip_indent {
            for 1..context.dprint.indent  print("  ");
        }
        print(format_string, ..args);
        if newline {
            print("\n");
        }
        context.dprint.skip_indent = !newline;
    }
}

dprint_push_indent :: () #expand {
    #if DEBUG {
        context.dprint.indent += 1;
        `defer context.dprint.indent -= 1;
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


