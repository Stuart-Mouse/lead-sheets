/*
    TODO:
        create debug flag
        in debug mode, add some mechanism to make sure the each push and pop size matches
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

is_on_stack :: (using stack: *Stack, pointer: *void) -> bool {
    return pointer >= stack.memory && pointer < stack.memory + stack.allocated;
}


// TODO: if in debug mode, we should also push some magic number (e.g. the current stack top) in push_frame to check when we call pop_frame

push_frame :: (using stack: *Stack, size: int) -> bool {
    new_frame_pointer := top;
    
    if !push(stack, null, size)    return false;
    if !push_value(stack, *size)   return false;
    if !push_value(stack, *frame)  return false;
    
    frame = new_frame_pointer;
    return true;
}

pop_frame :: inline (using stack: *Stack) {
    frame = pop_as(stack, *void);
    pop(stack, pop_as(stack, int));
}


/*
    NOTE: 
        The 'indirect' parameters are there so that we can use the same code path for cases where we want to push/pop some value directly and cases where we want to push/pop just a pointer.
        The parameter is also an auto-bake though, so you don't pay for the runtime flexibility it if you don't need it.
        
        The value returned by push, pop, and peek depends on the value of the 'indirect' parameter.
            If indirect == false, then the returned pointer points at stack.top.
            If indirect == true,  then the returned pointer points at whatever stack.top (interpreted as a *void) is pointing at.
            The idea here is that the returned pointer can always then be validly written to, whether or not one pushed 'indirectly'.
*/

// NOTE: if pointer is null, we just memset the pushed bytes to zero
push :: inline (using stack: *Stack, pointer: *void, size: int, $$indirect := false) -> *void {
    push_size := ifx indirect then size_of(*void) else (size + 7) & ~7;
    if top + push_size >= memory + allocated  return null;
    
    if indirect {
        top.(**void).* = pointer;   // store the pointer itself instead of the value
    } else if pointer == null {
        memset(top, 0, push_size);
    } else {
        memcpy(top, pointer, size);
    }
    
    defer top += push_size;
    return ifx indirect then pointer else top;
}

pop :: inline (using stack: *Stack, size: int, $$indirect := false) -> *void {
    pop_size := ifx indirect then size_of(*void) else (size + 7) & ~7;
    top -= pop_size;
    assert(top >= memory, "Invalid stack pop!");
    return ifx indirect then top.(**void).* else top;
}

peek :: inline (using stack: *Stack, size: int, $$indirect := false) -> *void {
    pop_size := ifx indirect then size_of(*void) else (size + 7) & ~7;
    pointer := top - pop_size;
    assert(pointer >= memory, "Invalid stack peek!");
    return ifx indirect then pointer.(**void).* else pointer;
}

push_value :: inline (using stack: *Stack, value: *$T) -> *T {
    return push(stack, value, size_of(T));
}

pop_as :: inline (using stack: *Stack, $T: Type) -> T {
    return pop(stack, size_of(T)).(*T).*;
}

peek_as :: inline (using stack: *Stack, $T: Type) -> T {
    return peek(stack, size_of(T)).(*T).*;
}


/*
    push / pop arrays
    
    In addition to pushing the array data, we also push the array count.
    This just seems like a very useful thing, and uses only marginally more stack space.
    You can always push jsut the array data manually if you really want to.
    
    TODO: maybe add a peek_array?
*/

push_array :: inline (using stack: *Stack, array: [] $T, count: int) -> bool, [] T {
    ok, returned_array := push_array(stack, array.data, size_of(T), count);
    return ok, (*returned_array).(*[]T).*;
}

push_empty_array :: inline (using stack: *Stack, $T: Type, count: int) -> bool, [] T {
    ok, array := push_array(stack, null, size_of(T), count);
    return ok, (*array).(*[]T).*;
}

push_array :: (using stack: *Stack, pointer: *void, element_size: int, count: int) -> bool, Array_View_64 {
    array := Array_View_64.{ count, push(stack, pointer, element_size * count) };
    count_pointer := push(stack, *count, size_of(int));
    
    ok := array.data != null && count_pointer != null;
    return ok, array;
}

pop_array :: inline (using stack: *Stack, $T: Type) -> [] T {
    return (*pop_array(stack, size_of(T))).(*[]T).*;
}

pop_array :: inline (using stack: *Stack, element_size: int) -> Array_View_64 {
    count := pop_as(stack, int);
    array := Array_View_64.{ count, pop(stack, element_size * count) };
    return array;
}


/*
    push / pop Any
    
    These procedures currently take the script rather than jsut the stack so that we can just set the execution error here if need be. 
    This removes what would otherwise be a lot of repetition in execute.jai.
    I still would ultimately like to not be passing the script here though, or at least we should also have version that just take the Stack.
*/

push_any :: (using stack: *Stack, any: Any, indirect := false) -> bool, Any {
    result := push(stack, any.value_pointer, any.type.runtime_size, indirect);
    return Any.{ any.type, result };
}

pop_any :: (using stack: *Stack, type: *Type_Info, indirect := false) -> Any {
    value_pointer := pop(stack, type.runtime_size, indirect);
    return Any.{ type, value_pointer };
}

peek_any :: (using stack: *Stack, type: *Type_Info, indirect := false) -> Any {
    value_pointer := peek(stack, type.runtime_size, indirect);
    return Any.{ type, value_pointer };
}
