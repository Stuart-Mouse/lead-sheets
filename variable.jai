
/*
    External Variables
    
    External variables are the primary means of getting data from outside of scripts into scripts.
    The tricky bit is, of course, that sometimes we want to sub out the actual value pointer for a variable because the backing storage for that value has changed.
    The way we currently handle this is simply to allow the user to re-set the value of the same variable, and they're responsible for updating these pointers so that they're always valid for the script.
    
    All external variables are essentially 'global scope' in the script.
    
    
    TODO: if/when we lower to bytecode, bake constant external variables as actual constants 
          but this is not too much of a priority for now, so I am just leaving this note for later development
    
    TODO: experiment with making external variables that store a user handle instead of and Any
          these external variables would also use a get_name and get_value callback so that the name can change over time without affecting the script's ability to execute properly
          this would prevent the user from having to manually reset all the volatile variables before every time they call the script
*/

External_Variable :: struct {
    type:           *Type_Info;
    value_pointer:  *void;
    name:           string;
    flags:          Flags;
    
    Flags :: enum_flags { 
        NONE :: 0; 
        STATIC :: 1; 
        CONSTANT; 
    }
}

get_value :: (v: External_Variable) -> Any { return Any.{ v.type, v.value_pointer }; }

// NOTE: This procedure exists so that one can invalidate all external variables they previously passed to a script.
//       This can be useful to catch bugs that would result from invalidated pointers or changing identifiers.
reset_external_variables :: (script: *Script) {
    for *script.variables {
        if !(it.flags & (.STATIC | .CONSTANT)) {
            it.value_pointer = null;
        }
    }
}

check_external_variables :: (script: *Script) {
    for *script.variables {
        if it.value_pointer == null {
            log("Warning: Value of external variable '%' has not been set.", it.name);
        }
    }
}

declare_variable :: (script: *Script, name: string, any: Any, flags := External_Variable.Flags.NONE, override := false) -> bool {
    for *script.variables {
        if it.name == name {
            if override {
                log("Info: Forced redeclaration of external variable '%' from type % to type %.", name, as_type(it.type), as_type(any.type));
                it.type          = any.type;
                it.value_pointer = any.value_pointer;
                return true;
            }
            log("Error: External variable '%' has already been declared with type %.", name, as_type(it.type));
            return false;
        }
    }
    array_add(*script.variables, .{ 
        name = name, 
        type = any.type,
        value_pointer = any.value_pointer,
        flags = flags,
    });
    return true;
}

declare_variable :: (script: *Script, name: string, type: Type, flags := External_Variable.Flags.NONE, override := false) -> bool {
    return declare_variable(script, name, Any.{ type = xx type }, flags, override);
}

set_variable :: (script: *Script, name: string, any: Any, override := true) -> bool {
    for *script.variables {
        if it.name == name {
            if it.type != any.type {
                log("Error: Cannot set value of external variable which was declared with type % using a value of type %.", as_type(it.type), as_type(any.type));
                return false;
            }
            if !override && it.value_pointer != null {
                log("Error: External variable '%' has already had its value set.", name);
                return false;
            }
            if !override && it.flags & .CONSTANT {
                log("Error: Cannot set the value of constant external variable '%'.", it.name);
                return false;
            }
            it.value_pointer = any.value_pointer;
            return true;
        }
    }
    
    log("Error: External variable '%' has not been declared.", name);
    return false;
}
