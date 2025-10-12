/*
    TODO:
        create debug flag
        in debug mode, add a Stack.Push_Header that we can use to check for proper pushes/pops
*/

Stack :: struct {
    memory:         *void;
    allocated:      int;
    top:            *void;
    frame:          *void;
}

// TODO: ensure that stack base is aligned to 8 bytes
init_stack :: (stack: *Stack, size_in_bytes := 1024) {
    stack.allocated = size_in_bytes;
    memory := alloc(stack.allocated);
    stack.memory, stack.top, stack.frame = memory;
}

reset_stack :: (stack: *Stack) {
    stack.top, stack.frame = stack.memory;
}

deinit_stack :: (stack: *Stack) {
    free(stack.memory);
}

/*
    NOTE: 
        the 'indirect' parameters are there so that we can use the same code path for cases where we want to push/pop some value directly and cases where we want to push/pop just a pointer
        the parameter is also an auto-bake though, so you don't pay for the runtime flexibility it if you don't need it
*/

push :: inline (using stack: *Stack, pointer: *void, size: int, $$indirect := false) -> bool {
    push_size := ifx indirect then 8 else (size + 7) & ~7;
    if top + push_size >= memory + allocated  return false;
    
    if indirect {
        // store the pointer itself instead of the value
        top.(**void).* = pointer; 
    } else {
        memcpy(top, pointer, size);
    }
    
    top += push_size;
    return true;
}

pop :: inline (using stack: *Stack, size: int, $$indirect := false) -> *void {
    pop_size := ifx indirect then 8 else (size + 7) & ~7;
    top -= pop_size;
    assert(top >= memory, "Invalid stack pop!");
    return ifx indirect then top.(**void).* else top;
}

peek :: inline (using stack: *Stack, size: int, $$indirect := false) -> *void {
    pop_size := ifx indirect then 8 else (size + 7) & ~7;
    pointer := top - pop_size;
    assert(pointer >= memory, "Invalid stack peek!");
    return ifx indirect then pointer.(**void).* else pointer;
}

push_zeroes :: (using script: *Script, size: int) -> bool {
    push_size := (size + 7) & ~7;
    if stack.top + push_size >= stack.memory + stack.allocated {
        set_execution_error(script, "Ran out of space on stack!", node = null);
        return false;
    }
    
    memset(stack.top, 0, push_size);
    stack.top += push_size;
    return true;
}

push_any :: (using script: *Script, any: Any, indirect := false) -> bool {
    dprint("push_any(%, %)\n", as_type(any.type), indirect);
    if indirect  dprint("pushed ptr value: %\n", any.value_pointer);
    ok := push(*script.stack, any.value_pointer, any.type.runtime_size, indirect);
    if !ok  set_execution_error(script, "Ran out of space on stack!", node = null);
    return ok;
}

pop_any :: (using script: *Script, type: *Type_Info, indirect := false) -> Any {
    dprint("pop_any(%, %)\n", as_type(type), indirect);
    value_pointer := pop(*script.stack, type.runtime_size, indirect);
    if indirect  dprint("popped ptr value: %\n", value_pointer);
    return Any.{ type, value_pointer };
}

peek_any :: (using script: *Script, type: *Type_Info, indirect := false) -> Any {
    value_pointer := peek(*script.stack, type.runtime_size, indirect);
    return Any.{ type, value_pointer };
}

is_on_stack :: (using stack: *Stack, pointer: *void) -> bool {
    return pointer >= stack.memory && pointer < stack.memory + stack.allocated;
}
