
Stack :: struct {
    memory:         *void;
    allocated:      int;
    top:            *void;
    
    // Word :: Number_Union;
    WORD_SIZE :: size_of(*void);
    // #assert(size_of(Word) == WORD_SIZE);
}

// TODO: ensure that stack base is aligned to 8 bytes
init_stack :: (stack: *Stack, size_in_bytes := 1024) {
    stack.allocated = size_in_bytes;
    stack.memory    = alloc(stack.allocated);
    stack.top       = stack.memory;
}

reset_stack :: (stack: *Stack) {
    stack.top = stack.memory;
};

deinit_stack :: (stack: *Stack) {
    free(stack.memory);
};

pop :: inline (stack: *Stack, $$indirect := false) -> *void {
    stack.top -= stack.WORD_SIZE;
    assert(stack.top >= stack.memory, "Invalid stack pop!");
    return ifx indirect then stack.top.(**void).* else stack.top;
}

peek :: inline (stack: *Stack, $$indirect := false) -> *void {
    pointer := stack.top - stack.WORD_SIZE;
    assert(pointer >= stack.memory, "Invalid stack peek!");
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
