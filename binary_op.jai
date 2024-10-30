
/*
    currently binary operations are just sugar for procedure calls
    
    later, I definitely want to add some optimization to remove the dynamic function call overhead for the most simple/common operators  
    
*/

set_basic_operators :: (script: *Script) {
    {
        operator + :: (a: $T, b: T) -> type_of(a + b) { return a + b; }
        operator - :: (a: $T, b: T) -> type_of(a - b) { return a - b; }
        operator * :: (a: $T, b: T) -> type_of(a * b) { return a * b; }
        operator / :: (a: $T, b: T) -> type_of(a / b) { return a / b; }
        
        register_operator(script, "+", #procedure_of_call operator + (cast(float)1.0, cast(float)1.0));
        register_operator(script, "-", #procedure_of_call operator - (cast(float)1.0, cast(float)1.0));
        register_operator(script, "*", #procedure_of_call operator * (cast(float)1.0, cast(float)1.0));
        register_operator(script, "/", #procedure_of_call operator / (cast(float)1.0, cast(float)1.0));
    }
    
    register_operator(script, "+", #procedure_of_call operator + (Vector2.{}, Vector2.{}));
    register_operator(script, "-", #procedure_of_call operator - (Vector2.{}, Vector2.{}));
    register_operator(script, "*", #procedure_of_call operator * (Vector2.{}, Vector2.{}));
    register_operator(script, "/", #procedure_of_call operator / (Vector2.{}, Vector2.{}));
    
    register_operator(script, "*", #procedure_of_call operator * (Vector2.{}, cast(float)1.0));
    register_operator(script, "/", #procedure_of_call operator / (Vector2.{}, cast(float)1.0));
}


