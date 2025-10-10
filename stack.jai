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
    
    // Word :: Number_Union;
    WORD_SIZE :: size_of(*void);
    // #assert(size_of(Word) == WORD_SIZE);
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

push :: inline (using stack: *Stack, pointer: *void, size: int, $$indirect := false) -> bool {
    if top >= memory + allocated  return false;
    
    if indirect {
        // store the pointer itself instead of the value
        top.(**void).* = pointer; 
    } else {
        assert(size <= WORD_SIZE);
        memset(top, 0, WORD_SIZE);          // in case element size is less than word size
        memcpy(top, pointer, size); 
    }
    
    top += WORD_SIZE;
    return true;
}

pop :: inline (using stack: *Stack, $$indirect := false) -> *void {
    top -= WORD_SIZE;
    assert(top >= memory, "Invalid stack pop!");
    return ifx indirect then top.(**void).* else top;
}

peek :: inline (using stack: *Stack, $$indirect := false) -> *void {
    pointer := top - WORD_SIZE;
    assert(pointer >= memory, "Invalid stack peek!");
    return ifx indirect then pointer.(**void).* else pointer;
}

push_zeroes :: (using script: *Script, count: int) -> bool {
    if stack.top + count >= stack.memory + stack.allocated {
        set_execution_error(script, "Ran out of space on stack!", node = null);
        return false;
    }
    
    memset(stack.top, 0, count);
    stack.top += count;
    return true;
}

push_any :: (using script: *Script, any: Any, indirect := false) -> bool {
    if stack.top >= stack.memory + stack.allocated {
        set_execution_error(script, "Ran out of space on stack!", node = null);
        return false;
    }
    
    if is_aggr(any.type) || indirect {
        // store the pointer itself instead of the value
        stack.top.(**void).* = any.value_pointer; 
    } else {
        assert(any.type.runtime_size <= stack.WORD_SIZE);
        memset(stack.top, 0, stack.WORD_SIZE);
        memcpy(stack.top, any.value_pointer, stack.WORD_SIZE); 
    }
    
    stack.top += stack.WORD_SIZE;
    return true;
}

pop_any :: (using script: *Script, ti: *Type_Info, indirect := false) -> Any {
    stack.top -= stack.WORD_SIZE;
    assert(stack.top >= stack.memory, "Invalid stack pop!");
    if is_aggr(ti) || indirect {
        return Any.{ ti, stack.top.(**void).* }; // additional dereference to get pointer to value
    } else {
        assert(ti.runtime_size <= stack.WORD_SIZE);
        return Any.{ ti, stack.top };
    }
}

peek_any :: (using script: *Script, ti: *Type_Info, indirect := false) -> Any {
    pointer := stack.top - stack.WORD_SIZE;
    assert(stack.top >= stack.memory, "Invalid stack peek!");
    if is_aggr(ti) || indirect {
        return Any.{ ti, pointer.(**void).* }; // additional dereference to get pointer to value
    } else {
        assert(ti.runtime_size <= stack.WORD_SIZE);
        return Any.{ ti, pointer };
    }
}

is_on_stack :: (using stack: *Stack, pointer: *void) -> bool {
    return pointer >= stack.memory && pointer < stack.memory + stack.allocated;
}
