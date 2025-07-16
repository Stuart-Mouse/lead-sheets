
External_Procedure :: struct {
    using #as procedure: Any_Proc;
    name:   string;
}

can_register_procedure :: (proc_info: *Type_Info_Procedure) -> bool {
    return (proc_info.return_types.count <= 1);
}

// To start with, I'm completely separating the dyncall and non-dyncall methods of calling procedures
// in the future I will try to re-integrate them, though that may be slightly tricky because of how Any is preferred over polymorphic parameters when doing overload resolution
// we might have to do some weird thing where we investiage #caller_code to check if 'procedure' is a constant and manually add the call for register_procedure_type
#if USING_DYNCALL {
    register_procedure :: (script: *Script, name: string, procedure: Any) {
        assert(procedure.type.type == .PROCEDURE && can_register_procedure(xx procedure.type));
        array_add(*script.procedures, .{ Any_Proc.from(procedure), name });
    }
} else {
    register_procedure :: inline (script: *Script, name: string, procedure: $P) #modify {
        proc_info := P.(*Type_Info_Procedure);
        if proc_info.type != .PROCEDURE  return false, "'procedure' must be a procedure! What are you stupid?";
        if !can_register_procedure(proc_info)  return false, tprint("Unable to register procedure of type: %", P);
        return true;
    } {
        register_procedure_type(P);
        array_add(*script.procedures, .{ Any_Proc.from(procedure), name });
    }
}

Procedure_Overload :: struct {
    proc_index:         int;
    coercion_distance:  int;                                    // a score assigned based on how 'far' we have to coerce arguments in order to make the candidate procedure match
    swap_args:          bool;
    reject_reason:      enum { NONE; ARG_COUNT; ARG_TYPE; };    // if .NONE, then candidate is still in the running
    reject_details: union {
        arg_count:  struct { expected: int; provided: int; };
        arg_type:   struct { index: int; expected_type: *Type_Info; argument: *Node; };
    };
}


get_procedure_overloads :: (script: *Script, name: string) -> [] Procedure_Overload {
    overloads: [..] Procedure_Overload;
    for *script.procedures  
        if it.name == name  
            array_add(*overloads, .{ proc_index = it_index });
    return overloads;
}


resolve_procedure_index :: (script: *Script, name: string, arguments: [] *Node, is_operator := false) -> (index: int, swap_args: bool) {
    if is_operator {
        assert(arguments.count == 2, "arguments.count was %", arguments.count);
    }
    
    overloads := get_procedure_overloads(script, name,, temp);
    if !overloads {
        print("Unable to resolve procedure '%'\n", name);
        print("No procedure found with the given identifier.\n");
    }
    
    // typecheck overloads and determine best choice
    best_overload_index     := -1; // indexes overloads, not script.procedures
    best_overload_distance  := S64_MAX; 
    tied_for_best           := 0;
    
    for *overloads {
        proc := *script.procedures[it.proc_index];
        
        // typecheck arguments
        if proc.argument_types.count != arguments.count {
            it.reject_reason = .ARG_COUNT;
            it.reject_details.arg_count = .{ 
                expected = proc.argument_types.count, 
                provided = arguments.count 
            };
            continue;
        }
        
        // returns the coercion distance, else -1 if arguments cannot be made to match through implicit coercions alone
        typecheck_arguments :: (overload: *Procedure_Overload, expected_types: [] *Type_Info, arguments: [] *Node) -> bool {
            // the overall idea on the control flow here is just to `continue` if we are good, else we use the common reject case 
            for expected_type: expected_types {
                argument      := arguments[it_index];
                argument_type := argument.value_type;
                
                if expected_type == argument_type  continue; // ezpz match
                
                if argument_type == null {
                    // we allow arguments to fail typechecking on the first pass so that 
                    // if they are implicitly typed structs, they can still be resolved after overload resolution
                    if expected_type.type == .STRUCT  continue;
                } else {
                    if get_implicit_cast_distance(argument, expected_type) >= 0  continue;
                    if can_do_implicit_reference(argument, expected_type)  continue;
                }
                
                // else argument absolutely does not match expected type
                overload.reject_reason = .ARG_TYPE;
                overload.reject_details.arg_type = .{ 
                    index         = it_index + 1,
                    expected_type = expected_type, 
                    argument      = argument
                };
                return false;
            }
            
            return true;
        }
        
        ok := typecheck_arguments(it, proc.argument_types, arguments);
        
        // fallback case to try reordering args for symmetric case
        if !ok && is_operator && (proc.procedure_flags & .IS_SYMMETRIC) {
            assert(proc.argument_types.count == 2);
            assert(arguments.count == 2);
            it.swap_args = true;
            ok = typecheck_arguments(it, proc.argument_types, .[ arguments[1], arguments[0] ]);
        }
        if !ok  continue;
        
        if it.coercion_distance < best_overload_distance {
            best_overload_index    = it_index;
            best_overload_distance = it.coercion_distance;
            tied_for_best          = 0;
        }
        else if it.coercion_distance == best_overload_distance {
            tied_for_best += 1;
        }
    }
    
    if !tied_for_best && best_overload_index >= 0 {
        chosen_overload := overloads[best_overload_index];
        return chosen_overload.proc_index, chosen_overload.swap_args;
    }
    
    print("Unable to resolve procedure '%'\n", name);
    
    if tied_for_best {
        print("Could not decide between % matching overloads:\n\n", tied_for_best);
        for overloads {
            if it.reject_reason != .NONE 
            || it.coercion_distance != best_overload_distance 
                then continue;
                
            print("%(", name);
            for script.procedures[it.proc_index].argument_types {
                if it_index != 0  print(", ");
                print("%", as_type(it));
            }
            print(")\n");
        }
    }
    
    if overloads.count > tied_for_best {
        print("% failing overload(s):\n\n", overloads.count);
        for overloads {
            if it.reject_reason == .NONE  continue;
            
            print("%(", name);
            for script.procedures[it.proc_index].argument_types {
                if it_index != 0  print(", ");
                print("%", as_type(it));
            }
            print(")\n");
            
            if it.reject_reason == {
              case .ARG_COUNT;
                print(
                    "Mismatch on argument count: expected %, got %\n\n", 
                    it.reject_details.arg_count.expected, 
                    it.reject_details.arg_count.provided
                );
                
              case .ARG_TYPE;
                print(
                    "Type mismatch on argument %: expected %, got %\n\n", 
                    it.reject_details.arg_type.index, 
                    as_type(it.reject_details.arg_type.expected_type), 
                    as_type(it.reject_details.arg_type.argument.value_type)
                );
            }
        }
    }
    
    print("Argument types were: ");
    for arguments {
        if it_index != 0  print(", ");
        print("%", as_type(it.value_type));
    }
    print("\n");
    
    return -1, false;
}

#scope_module

#if !USING_DYNCALL {
    /*
        call_procedure is used to provide a generic interface for calling any type of procedure, 
            where arguments and return values are provided as `[] Any`.
        Using `#procedure_of_call call_procedure(null.(P), .[], .[])` we can generate a thin wrapper proc 
            that will handle the marshaling of arguments and return types from/to the provided Anys.
        This generated procedure will match the interface of a `Wrapped_Procedure_Call`,
            allowing us to store all the generated wrapper procs in a single array.
        We need only generate one such wrapper for each type of procedure, so if two procedures have the same exact signature,
            then we will not add bloat the code by baking the target procedure's specific pointer.
        
        All things considered, this is actually relatively simple. 
        Getting the argument and return types could be simplified, but other than that it's not too bad to generate the calling code for any given procedure type.
        If the compiler never gets built-in dyncall functionality, but does one day get a runtime "compile and link" sort of system, I could probably get by with using this code in addition to a few other little helper procs.
        Although, I still think it would be dumb for the compiler to do all that and not have a simple dyncall-like facility which is exposed to the user.
        If we can compile and link in arbitrary code, we should certainly be able to generate some simple procedure-calling code.
        
        NOTE: This doesn't perform any type checking on its own. 
              That can be done generically, so it's not included in the generated procedure body.
    */
    call_procedure :: (procedure: $P, arguments: [] Any, return_values: [] Any) #modify {
        return P.(*Type_Info).type == .PROCEDURE, "P must be a procedure type.";
    } {
        proc_info :: type_info(P);
        
        #insert -> string {
            builder: String_Builder;
            
            // Because we don't want to rely on simply printing types into the generated code (which won't work for procedure types),
            //   we need to first do some nonsense to unpack the types as constants we can easily reference.
            // Perhaps this sort of thing would be a lot easier to do if the compiler just collapsed the distinction between `Type` and `*Type_Info`, 
            //   or allowed casting between the two to be an implicitly comptime operation. 
            
            for 0..proc_info.argument_types.count-1 {
                print(*builder, "Ta%1 :: #run (*proc_info.argument_types[%1]).(*Type).*;\n", it);
            }
            for 0..proc_info.return_types.count-1 {
                print(*builder, "Tr%1 :: #run (*proc_info.return_types[%1]).(*Type).*;\n", it);
            }
            
            for 0..proc_info.return_types.count-1 {
                if it != 0  append(*builder, ", ");
                print(*builder, "return_values[%1].value_pointer.(*Tr%1).*", it);
            }
            if proc_info.return_types.count > 0 {
                append(*builder, " = ");
            }
            
            append(*builder, "procedure(");
            for 0..proc_info.argument_types.count-1 {
                if it != 0  append(*builder, ", ");
                print(*builder, "arguments[%1].value_pointer.(*Ta%1).*", it);
            }
            append(*builder, ");");
            
            return builder_to_string(*builder);
        }
    }
    
    
    register_procedure_type :: inline ($P: Type) #modify {
        return P.(*Type_Info).type == .PROCEDURE, "P must be a procedure type."; 
    } {
        for procedure_wrappers if it.type == xx P  return;
        array_add(*procedure_wrappers, .{ xx P, xx #procedure_of_call call_procedure(null.(P), .[], .[]) });
    }
    
    Wrapped_Procedure_Call :: #type (pointer: *void, arguments: [] Any, return_values: [] Any);
    
    procedure_wrappers: [..] struct { type: *Type_Info; wrapper: Wrapped_Procedure_Call; };
    
    try_calling_procedure_with_wrapper :: (proc_info: *Type_Info_Procedure, proc_ptr: *void, arguments: [] Any, return_values: [] Any) -> bool {
        for procedure_wrappers {
            if it.type == proc_info {
                it.wrapper(proc_ptr, arguments, return_values);
                return true;
            }
        }
        return false;
    }
}



