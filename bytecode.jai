

/*
    TODO: I actually want to start on this sooner rather than later now. 
    
    Evaluating scripts by walking the AST has worked fine for a while, but of course it is not ideal, and I have already started running into some problems.
    
    I really should do one more cleanup/refactoring pass before starting the bytecode stuff
    the main focus of this will be in separating the parse and typecheck info on code nodes
        we should be able to very easily revert any typechecking that was done on a node and re-typecheck it so that we can robustly handle code modification
        biggest issue is how to handle identifiers and still allow renaming. Right now we destructively typecheck these by replacing the source name with other info
            maybe we just take the name member out of the union, but then we need to make sure that this is never used if the identifier has already been resolved!
        
    it may also be a good time to consider making each node more aware of how it is referenced, so that we can trivially replace nodes in-place
        sea of nodes?
            store each node type in its own array, prevent duplication of nodes
            track nodes that refer to each node
            need some Typechecked node that points to underlying lexical node and provides the type information 
                the same lexical node may be interpretted differently based on the context
    
    another big thing to think about is how to improve the robustness of identifier resolution, and how to re-check that identifiers are valid even after being renamed
        
    
    GOALS:
    - [ ] much better performance than AST walking
    - [ ] good debuggability, step through scripts as they execute
    - [ ] easy patching and dynamic bytecode modification
        - when AST is modified, detect this and automatically generate new bytecode
        - use special bytecode instruction that indicates a jump to replacement instructions
        - may require some model like 'statement/expression as smallest compilation unit'
            - maybe consider learning more about sea of nodes
    
    AST nodes will need some handle to the bytecode they generated
        they will also have source code range, so we can link from bytecode, through ast, back to source text
        we should also try rendering the ast as text (a la Dion)
    
    in debug builds, we will also want to be able to get the AST node that generated some bytecode
        we can probably do this by storing some index + count representing range of bytecode, along with pointer to ast node that generated it
        or maybe in the bytecode itself we just have an instruction that sets some 'cursor' in the script context that indicates the ast node 
        
        
    
    Literal Types
    u8, u16, u32, u64
    s8, s16, s32, s64
    f32, f64    
    pointer
    struct/array (type erased, only size known)
    
    
        
    Instruction set:
    
    GOTO(location, condition)
        location: 64-bit pointer/index used to set instruction pointer
        condition: bool
    ADD, SUB, MUL, DIV
        variants:
            u8, u16, u32, u64
            s8, s16, s32, s64
            f32, f64
    CAST
        variants: 
            int -> int
            int -> float
            float -> int
    PUSH_LITERAL
        register-sized types
        aggregate types
    MODIFY_STACK_POINTER
        SET, ADD
    MEMCPY(to, from)
        variants needed for copy from stack or from random memory location
    PUSH_VARIABLE
        performs a lookup in variables table, or maybe directly indexes
    CALL_PROCEDURE(procedure_id)
        what about operands?  some way to optimize the usual procedure calling method?        
    
    
    
*/


Bytecode_Instruction_Type :: enum { 
    // push to stack
    LITERAL;
    VARIABLE;
    BUMP_STACK;
    
    // pop from stack
    OPERATOR;           // for builtin operations only; overloaded operators desugar to procedure calls completely for bytecode
    PROCEDURE_CALL;
    ASSIGN;
    
    // control flow?
    RETURN;
};

Bytecode_Instruction :: struct {
    type: Bytecode_Instruction_Type;
    #if DEBUG {
        // TODO: debug data locate errors
    }
}

Bytecode_Variable :: struct {
    using #as base   : Bytecode_Instruction;
    variable         : *Any;
    flags            : enum { L_VALUE; }; // determines whether we push value or pointer
}

Bytecode_Literal :: struct {
    using #as base   : Bytecode_Instruction;
    value            : *Any;
}

// kind of hacky? we need to bump the stack for return values of operators and procedures so that return value can get before operands.
// Seems like the simplest thing to do at the moment...
Bytecode_Bump_Stack :: struct {
    using #as base: Bytecode_Instruction;
    size: int;
}


Bytecode_Operator :: struct {
    using #as base   : Bytecode_Instruction;
    operator_index   : int;
}

Bytecode_Procedure_Call :: struct {
    using #as base   : Bytecode_Instruction;
    proc_type        : *Type_Info_Procedure
    proc_ptr         : *void;
}

// assumes pointer for dst and value to be copied on stack
// maybe this should just be considered a binary operator?
//      probably something to figure out before moving to using bytecode
//      we would still need special typechecking flags for left side
Bytecode_Assignment :: struct {
    using #as base: Bytecode_Instruction;
    pop_size: int; // size of right side value + size_of(*void)
}

execute_bytecode :: (instruction_pointer: *void, stack_pointer: *void) -> bool {
    ip := cast(*Bytecode_Instruction) instruction_pointer;
    sp := stack_pointer;
    
    while true {
        if ip.type == {
          case .LITERAL;
            ip_lit := cast(*Bytecode_Literal) ip;
            size := ip_lit.value.type.runtime_size
            memcpy(sp, ip_lit.value.value_pointer, size);
            sp += size;
            ip += size_of(Bytecode_Literal);
            
          case .VARIABLE;
            ip_var := cast(*Bytecode_Variable) ip;
            if ip_var.flags & .L_VALUE {
                memcpy(sp, *ip_var.variable.value_pointer, size_of(*void));
                sp += size_of(*void);
            } else {
                size := ip_var.variable.type.runtime_size
                memcpy(sp, ip_var.variable.value_pointer, size);
                sp += size;
            }
            ip += size_of(Bytecode_Variable);
            
          case .OPERATOR;
            ip_op := cast(*Bytecode_Operator) ip;
            execute_builtin_operation(ip_op.operator_index, *sp);
            ip += size_of(Bytecode_Operator);
            
          case .PROCEDURE_CALL;
            ip_proc := cast(*Bytecode_Procedure_Call) ip;
            for proc_type.argument_types  sp -= it.runtime_size;
            sp -= proc_type.return_types[0].runtime_size;
            if !do_dyncall(script.dyncall_vm, procedure.ptr, procedure.type, sp) {
                return false;
            }
            ip += size_of(Bytecode_Procedure_Call);
            
          case .ASSIGNMENT;
            ip_ass := cast(*Bytecode_Variable) ip;
            sp -= ip_ass.pop_size;
            memcpy(sp, sp + size_of(*void), ip_ass.size);
            ip += size_of(Bytecode_Variable);
            
          case .BUMP_STACK;
            ip_bump := cast(*Bytecode_Bump_Stack) ip;
            sp += ip_bump.size;
            ip += size_of(Bytecode_Bump_Stack);
            
          case .RETURN;
            return true;
        }
    }
    
    assert(false, "unreachable");
    return false;
}


#scope_module



