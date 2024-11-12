

/*
    It will still be a bit before I implement a bytecode to execute instead of just walking the AST,
        but I wanted to start this file so that I can begin drafting some of the data structures.
    
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



