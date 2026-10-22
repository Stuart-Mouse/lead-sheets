#if false {

/*
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
        
    
    Notes:
    
    In order to still allow users to easily extend the language without needing to go all the way down to bytecode, 
    we can also implement an instruction that will evaluate an AST node directly.
    I think this should work fine even if we need to interleave bytecode execution and AST evaluation.
    This also gives us a mechanism to do the more dynamic stuff we need to do with Any in this language.
    Aside: For a given node whose value_type is Any, I think we should be able to generate bytecode on the fly and then cache that and switch on the value_type when we return on subsequent runs.
    Another aside: In a way somewhat similar to this idea of interleaving bytecode execution and AST evaluation, we could probably rewrite the main parsing, typechecking, and evaluation procedures to use an explicit state machine with its own stack while still preserving the current interface for user extensions
    
    
*/

// TODO: In order to make this code more cross-platform, we probably need to not rely on the flat pool and having all instructions be absolutely sequential in memory.
//       So I think we will have to manage our memory more manually here and allocate chunks of 1024 bytes or something, and then use indices for things like setting the instruction pointer, so that we can get a pointer into the proper chunk
//       Maybe different blocks/procedures should even get their own buffers of bytecode so that we can just clear those individually when we need to recompile that procedure

Bytecode_Page :: struct {
    // TODO: probably also store metadata about this page, who generated it, etc.
    instruction_buffer: [] u8;
}

Bytecode_VM :: struct {
    script: *Script;
    
    bytecode_pages: [..] Bytecode_Page;
    
    current_bytecode_page:      int;
    instruction_pointer:        *void;
}

Bytecode_Builder :: struct {
    script: *Script;
    
    instruction_buffer: [..] u8;
}


Bytecode_Instruction_Locator :: u32;

Bytecode_Instruction_Type :: enum u8 { 
    BRANCH;
    
    PUSH_WORD;
    PUSH_BYTES;
    
    COPY_MEMORY;
    
    PUSH_EXTERNAL_VARIABLE;
    
    CALL_PROCEDURE;
    
    SWAP_STACK_OPERANDS;
    
    MOVE_STACK_FRAME_POINTER;
    SET_STACK_FRAME_POINTER;
    
    MOVE_STACK_POINTER;
    SET_STACK_POINTER;
    
    BUILTIN_OPERATION;
    
    // RETURN;
    // EXIT;
    
    // CAST;
    
    // TODO: This DYNAMIC_SPECIALIZE will not actually work at all, at least in the way I was thinking about it before.
    //       Because types flow from leaf down to root rather than the other way around, we won't know we have something of a new type until we hit some leaf that changed,
    //       at which point we will need to re-typecheck the entire statement that contains said leaf node.
    //       This becomes even more of a problem when the statement is a declaration, since that could lead to a cascade of things that need to be re-typechecked.
    //       Really the only way this works out well is when we end up implicitly converting back to the same type as before in some larger expression.
    //       Probably, we need some more general mechanism for handling the recompilation of bytecode when virtual member types change...?
    // Ideas for solutions:
    // put label before each statement, back up execution state, then roll back when some leaf node Any's type is not as expected
    //      this won't work because we may have executed code with side effects before 
    // put label before each statement, insert prechecks to see if types of subepxressions of type Any have changed underlying type
    //      this won't work because we can't validly evaluate those subexpressions out-of-order or check their values without potentially having side effects
    // so we have to find some solution that actually allows us to recover at the particular point of failure, and then 
    //      maybe we can at least minimize the problem by requiring explicit types in declarations whose init expression is of type Any
    //      this will ensure that we don't have so much leakage of the Any type into other statements
    
    // When some node has a value_type of Any, we generate different bytecode for each different type we encounter, as needed.
    // Since the most common use of an Any in this language is for virtual members (which rarely change types), we usually only end up generating one specialization anyways.
    // But in the case that such a type change does occur, we have a mechanism to handle that at runtime.
    // DYNAMIC_SPECIALIZE;
    
    // TODO: We will have to solve the problem of the fact that inserting a path instruction may overwrite multiple instructions rather than just a single one.
    //       There are mayn way we could solve bytecode patching when we get there, but I am not there yet, so I'm gonna leave it alone for now.
    // When some statically-typed part of the AST is re-typechecked and the resulting value_type changes, we may have to insert a patch instruction.
    // Patch instructions will overwrite the instructions which they replace in the instruction buffer and divert execution elsewhere. 
    // They are essentially just a special version of an unconditional jump.
    // PATCH;
};

Bytecode_Instruction :: struct {
    instruction_type: Bytecode_Instruction_Type;
}

// @Speed: We could implement variant instructions for unconditional branches and branches with statically-known targets.
// Target and condition are popped from stack
Bytecode_Branch :: struct {
    using #as base: Bytecode_Instruction;
    instruction_type = .BRANCH;
    
    Target :: struct {
        page_index:         u32;
        instruction_offset: u32;
    }
}

Bytecode_Push_Word :: struct {
    using #as base: Bytecode_Instruction;
    instruction_type = .PUSH_WORD;
    value:          u64;
}

Bytecode_Push_Bytes :: struct {
    using #as base: Bytecode_Instruction;
    instruction_type = .PUSH_BYTES;
    count:          u32;
    data:           *void;
}

Bytecode_Push_Word_With_Offset :: struct {
    using #as base: Bytecode_Instruction;
    instruction_type = .PUSH_WORD_WITH_OFFSET;
    value:          u64;
    offset:         u16;
}

Bytecode_Push_Bytes_With_Offset :: struct {
    using #as base: Bytecode_Instruction;
    instruction_type = .PUSH_BYTES_WITH_OFFSET;
    count:          u32;
    data:           *void; 
    offset:         u16;
}

// count is popped from stack
Bytecode_Move_Stack_Pointer :: struct {
    using #as base: Bytecode_Instruction;
    instruction_type = .MOVE_STACK_POINTER;
}

// value is popped from stack
Bytecode_Set_Stack_Pointer :: struct {
    using #as base: Bytecode_Instruction;
    instruction_type = .SET_STACK_POINTER;
}

// count is popped from stack
Bytecode_Move_Stack_Frame_Pointer :: struct {
    using #as base: Bytecode_Instruction;
    instruction_type = .MOVE_STACK_POINTER;
}

// value is popped from stack
Bytecode_Set_Stack_Frame_Pointer :: struct {
    using #as base: Bytecode_Instruction;
    instruction_type = .SET_STACK_POINTER;
}

// left, right, and result are popped from stack
Bytecode_Builtin_Operation :: struct {
    using #as base: Bytecode_Instruction;
    instruction_type = .BUILTIN_OPERATION;
    builtin_operation_id: Builtin_Operation_ID;
}


Bytecode_Exit :: struct {
    using #as base: Bytecode_Instruction;
    instruction_type = .EXIT;
    exit_code: s64;
}


Bytecode_Push_External_Variable :: struct {
    using #as base: Bytecode_Instruction;
    locator: u16;
    flags: enum_flags { PUSH_BY_POINTER; }; 
}

Bytecode_Call_Procedure :: struct {
    using #as base: Bytecode_Instruction;
    wrapper_locator: u16;
}


// pop dst, src, size and memcpy
Bytecode_Copy_Memory :: struct {
    using #as base: Bytecode_Instruction;
    instruction_type = .COPY_MEMORY;
}

/*
    Notes on generating bytecode:
    
    Unlike our evaluate procs which return their results as an Any, bytecode can only push and pop from the stack.
    In theory, our evaluate procs should also follow the same general rules as bytecode would, in terms of how it uses the stack,
        but in practice we take a few shortcuts to make things easier on ourselves, as in the case of procedure calls.
        In this case, we basically just use the stack however we want as scratch space and then just make sure we put it back in a valid state before returning.
        We can do this because for AST-based evaluation, we essentially just have a contract about what gets left on the stack when a node is done evaluating, and that's it.
    
    As a first pass at implementing a bytecode, we should just push stuff to the stack relatively indiscriminately,
        even in cases where we would typically want to use some provided storage for the value.
        This will mean a lot of unnecessary copies, but we can always optimize after it is actually working.
    
    Speaking of provided storage, this is something that will need to work quite differently now.
        The primary utility of the provided storage mechanism is for filling in members of structures.
        For now we can probably just push the provided storage pointer, then the 
        
    
    Perhaps the way to handle the provided storage issue is just to make all of our instructions essentially ternary
        as in, they will take 3 operands rather than 2,
        for example, our builtin operations take a result pointer and two words, which may also be pointers
        in cases where we just want to store the result onto the stack, we can just push a pointer to the top of the stack which points at itself
            or we can just use null to indicate a store to the top of the stack
    
    For evaluation, the rule was always that we may or may not be passed some provided_storage, and if we are then it has to be filled in 
        With evaluation, this sort of makes sense since this flexibility allows us to optionally evaluate specific ubsexpressions,
        and at the root level we can provide the final storage and prevent a copy when pulling out the final value.
        But for bytecode, we can just establish the proper storage for all subexpression results up front.
    
    So, another problem that we have to consider now that we need to be more rigorous about stack usage is that we probably can't just push aggregates onto the stack willy nilly anymore
    We were only able to get away with this before because, as mentioned above, our only concern was that each node left the proper stack pointer in the right spot when it returned
    and this meant we could just use the stack as free scratch space so long as we stored the value of the stack pointer before doing so
    
    but now, we have ot worry about instructions that can't just do arbitrarily-sized pops
    the instructions need to pop only word-size operands
    this means we will have to calculate our max required scratch space ofr intermediate values up front, and then include that scratch space in the containing stack frame.
    then we can just store intermediate values for aggregates directly into our scratch space, and push pointers 
    
    
    NOTE: We also will probably need to refactor execute_builtin_operation so that it takes Words as operands rather than pointers to values
    because for register-sized types, we really are better off just passing the value itself rather than a pointer to the value
    this will make the code gen only slightly omre complicated, but should result in faster runtime code and will make bytecode generation much simpler 
*/

/*
    When generating bytecode, we often want to know where the result of some operation is going to be stored so that we can emit better bytecode and prevent unnecessary copying.
    So I have this notion of a storage locator that we may pass to generate_bytecode, which will indicate exactly where we want to result of the operation to be stored.
    When the kind is NONE, that indicates that we should just push the result directly onto the stack.
*/
Bytecode_Storage_Locator :: struct {
    kind:           Kind;
    __padding__:    u8;
    locator:        u32;
    
    Kind :: enum { NONE; STACK_POINTER_OFFSET; STACK_FRAME_OFFSET; POINTER; CONSTANT_POINTER; };
} #no_padding;

#assert size_of(Bytecode_Storage_Locator) == 8;


generate_bytecode :: (builder: *Bytecode_Builder, node: *Node, provided_storage := Bytecode_Storage_Locator.{}) {
    if node.node_type == {
      case Node_Directive;
      case Node_Literal;
        if #complete literal.literal_type == {
          case .ANY;
            assert(false, "TODO: implement ANY literal");
          //   // == section copied from prepare_result ===
          //   result = prepare_result(script, literal, literal.any, provided_storage);
          //   if provided_storage {
          //       // assert(node.value_type.type != .ANY);
                
          //       #if DEBUG {
          //           debug_print("Returning result using provided storage @ %: ", provided_storage, newline = false);
          //           if !is_aggregate(any.type)  debug_print("%", any, newline = false);
          //           debug_print(" (%)", as_type(any.type));
          //       }
                
          //       if any.value_pointer 
          //           then memcpy(provided_storage, any.value_pointer, any.type.runtime_size);
          //           else memset(provided_storage, 0, any.type.runtime_size);
                
          //       return Any.{ any.type, provided_storage };
          //   }
            
            
          //   ok, result := push_any(*script.stack, any, should_push_by_pointer(node));
          //   if !ok  set_execution_error(script, "Ran out of space on stack!", node = node);
            
          //   #if DEBUG {
          //       debug_print("Returning result @ % using stack: ", result.value_pointer, newline = false);
          //       if !is_aggregate(any.type)  debug_print("%", any, newline = false);
          //       debug_print(" (%)", as_type(any.type));
          //   }
          //   // == end section ==
            
          //   append_instruction(builder, Bytecode_Push_Word.{  });
            
          case .STRING;
            result = prepare_result(script, literal, literal.text, provided_storage);
            if provided_storage.kind == {
              case .STACK_OFFSET;
                append_instruction(builder, Bytecode_Push_Bytes.{ count = size_of(string), data = *literal.text });
            }
            
          case .BOOLEAN; #through;
          case .NUMBER;
            // TODO: how to handle storing things directly when we know for sure that we will have some provided storage?
            if provided_storage.kind == .STACK_POINTER_OFFSET {
                append_instruction(builder, Bytecode_Copy_Literal_To_Stack.{ 
                    stack_offset = provided_storage.locator,
                    value        = literal.number.as_u64, 
                });
            } else if provided_storage.kind == .STACK_FRAME_OFFSET {
                append_instruction(builder, Bytecode_Copy_Literal_To_Memory.{ 
                    stack_offset = provided_storage.locator,
                    value        = literal.number.as_u64, 
                });
            } else if provided_storage.kind == .POINTER {
                append_instruction(builder, Bytecode_Copy_Literal_To_Memory.{ 
                    stack_offset = provided_storage.locator,
                    value        = literal.number.as_u64, 
                });
            } else {
                append_instruction(builder, Bytecode_Push_Word.{ value = literal.number.as_bytes });
            }
            
          case .STRUCT;
            assert(false, "TODO: implement STRUCT literal");
            
            // TODO: if the value of the struct literal is constant, then we should not have to recompute the inner expressions every time.
            
            // value_pointer: *void;
            // if literal.aggregate.value_pointer
            //     then value_pointer = literal.aggregate.value_pointer;
            //     else value_pointer = get_result_storage(script, literal, provided_storage);
            
            // if !value_pointer  return NULL_ANY;
            
            // write_pointer := value_pointer;
            // for literal.aggregate.expressions {
            //     evaluate_node_or_return(script, it, provided_storage = write_pointer);
            //     write_pointer += it.value_type.runtime_size;
            // }
            
            // if literal.aggregate.value_pointer {
            //     result = prepare_result(script, literal, Any.{ literal.value_type, literal.aggregate.value_pointer }, provided_storage);
            // } else {
            //     result = Any.{ literal.value_type, value_pointer };
            // }
            
          case;
            assert(false, "Invalid literal type in evaluate_literal.");
        }
      case Node_Identifier;
      case Node_Unary_Operator;
      case Node_Binary_Operator;
      case Node_Procedure_Call;
      case Node_Cast;
      case Node_Dot;
      case Node_Arrow;
      case Node_Subscript;
      case Node_Declaration;
      case Node_Block;
      case Node_If_Statement;
      case Node_If_Expression;
      case Node_While_Loop;
      case Node_For_Loop;
      case Node_Break;
      case;
        set_typecheck_error(script, "Unhandled node type % in generate_bytecode.", node.node_type, node = node);
    }
}

print_bytecode :: (builder: *String_Builder, buffer: [] u8) {
    case_preamble :: ($T: Type) #expand {
        if `instruction_pointer + size_of(T) > `buffer_end_pointer {
            set_execution_error(script, "Attempt to read beyond end of instruction buffer! Instruction type: %", `instruction_type);
            return false;
        }
        #if T != Bytecode_Branch {
            `defer `instruction_pointer += size_of(T);
        }
        `instruction := instruction.(*T);
    }
    
    buffer_end_pointer := buffer.data + buffer.count;
    if buffer.count < size_of(ByteCode_Instruction) then return;
    
    `instruction_pointer := buffer.data;
    while main_loop := true {
        instruction_type := instruction_pointer.(*Bytecode_Instruction).instruction_type;
        print(builder, "% ", instruction.instruction_type);
        
        if #complete instruction.instruction_type == {
          case .EXIT;  case_preamble(Bytecode_Exit);
            print(builder, " : %", instruction.exit_code);
            return;
            
          case .PUSH_WORD;  case_preamble(Bytecode_Push_Word);
            print(builder, " : %", instruction.value);
            
          case .PUSH_EXTERNAL_VARIABLE;  case_preamble(Bytecode_Push_External_Variable);
            variable := *vm.script.variables[instruction.locator];
            print(builder, " : % (%)", instruction.locator, variable.name);
        }
    }
}

// TODO: add parameter to allow breaking in execution, or to execute a single instruction at a time.
execute_bytecode :: (vm: *Bytecode_VM) -> bool {
    case_preamble :: ($T: Type) #expand {
        if `vm.instruction_pointer + size_of(T) > `buffer_end_pointer {
            set_execution_error(script, "Attempt to read beyond end of instruction buffer! Instruction type: %", `instruction_type);
            return false;
        }
        #if T != Bytecode_Branch {
            `defer `vm.instruction_pointer += size_of(T);
        }
        `instruction := instruction.(*T);
    }
    
    // Note that we only set these before the main loop, so if we need to switch pages during the loop (e.g., for a branch),
    // then we will need to update vm.current_bytecode_page, current_page, and buffer_end_pointer.
    current_page       := vm.bytecode_pages[vm.current_bytecode_page];
    buffer_end_pointer := current_page.buffer.data + current_page.buffer.count;
    
    if current_page.buffer.count < size_of(ByteCode_Instruction) {
        set_execution_error(script, "Attempt to execute empty instruction buffer!");
        return false;
    }
    
    // We may be resuming execution that was previously paused, so the instruction pointer may not point to the start of the page's instruction buffer.
    if vm.instruction_pointer >= buffer_end_pointer {
        set_execution_error(script, "Attempt to read beyond end of instruction buffer!");
        return false;
    }
    
    while main_loop := true {
        instruction_type := vm.instruction_pointer.(*Bytecode_Instruction).instruction_type;
        if #complete instruction_type == {
          case .EXIT;  case_preamble(Bytecode_Exit);
            success := instruction.exit_code == 0;
            if !success {
                set_execution_error(script, "Bytecode execution exited with code '%'.", instruction.exit_code);
            }
            vm.exit_code = instruction.exit_code;
            return success;
            
          case .BRANCH;  case_preamble(Bytecode_Branch);
            condition := pop_as(*vm.script.stack, bool);
            target    := pop_as(*vm.script.stack, Bytecode_Branch.Target);
            if condition {
                vm.current_bytecode_page = target.page_index;
                vm.instruction_pointer   = target.instruction_offset;
                current_page             = vm.bytecode_pages[vm.current_bytecode_page];
                buffer_end_pointer       = current_page.buffer.data + current_page.buffer.count;
            }
            
          case .PUSH_WORD;  case_preamble(Bytecode_Push_Word);
            inst := instruction.(*Bytecode_Push_Word);
            push_value(*vm.script.stack, instruction.value);
            
          case .PUSH_EXTERNAL_VARIABLE;  case_preamble(Bytecode_Push_External_Variable);
            inst := instruction.(*Bytecode_Push_External_Variable);
            variable := *vm.script.variables[instruction.locator];
            if variable.value_pointer == null {
                set_execution_error(script, "Unable to get value of external variable '%'.", variable.name);
                return false;
            } else {
                value := get_value(variable);
                
            }
            
          case .COPY_MEMORY;  case_preamble(Bytecode_Copy_Memory);
            count := pop_as(*vm.script.stack, int);
            src   := pop_as(*vm.script.stack, *void);
            dst   := pop_as(*vm.script.stack, *void);
            if dst {
                memcpy(dst, src, count);
            } else {
                push(*vm.script.stack, src, count);
            }
            
          case .BUILTIN_OPERATION;  case_preamble(Bytecode_Builtin_Operation);
            left   := pop_as(*vm.script.stack, *void);
            right  := pop_as(*vm.script.stack, *void);
            result := ifx pop_as(*vm.script.stack, *void) else vm.stack.top;
            
            if !vm.script.execute_builtin_operation(instruction.builtin_operation_id, left, right, result) {
                set_execution_error(script, "Failed to execute builtin operation '%'.", instruction.builtin_operation_id);
                return false;
            }
            
          case .CALL_PROCEDURE;  case_preamble(Bytecode_Call_Procedure);
            // When generating a procedure call, we need to do the following:
            //  push return value pointer
            //  push procedure pointer
            //  push empty array for argument Anys
            //  for each argument
            //      push value of argument to stack
            //      store pointer to value and type into corresponding Any
            
            arguments_array      := pop_array(*vm.script.stack, Any);
            procedure_pointer    := pop_as(*vm.script.stack, *void);
            return_value_pointer := pop_as(*vm.script.stack, *void);
            
            result := Any.{ procedure_call.value_type, return_value_pointer };
            result_array: [] Any;
            if result.type != xx void then result_array = .[ result ];
            
            // #if USING_DYNCALL {
            //     if !do_dyncall(script.dyncall_vm, procedure_pointer, arguments_array, result_array) {
            //         set_execution_error(script, "Failed while trying to do dyncall.", node = procedure_call);
            //         return false;
            //     }
            // } else {
                assert(instruction.wrapper_locator >= 0 && instruction.wrapper_locator < procedure_wrappers.count);
                
                wrapper := procedure_wrappers[instruction.wrapper_locator];
                if !wrapper(procedure_pointer, arguments_array, result_array) {
                    set_execution_error(script, "Error: Failed while trying to call procedure of type: %", as_type(procedure_info), node = procedure_call);
                    return false;
                }
            // }
            
          
          case;
            set_execution_error(script, "Unhandled instruction type: %", instruction_type);
            return false;
        }
    }
}

}