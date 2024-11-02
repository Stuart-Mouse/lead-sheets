
/*
    currently binary operations are just sugar for procedure calls
    
    later, I definitely want to add some optimization to remove the dynamic function call overhead for the most simple/common operators  
    For these "builtin" operators, we have some things to figure out
    we want to typecheck operators in the same way we typecheck procedures, before runtime of the script
    perhaps for operators, if it is a builtin operation, then we will leave the operator *proc null as a signifier that we should try a builtin op
    then if we are unable to find such an operation, we will have to error out
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


// typecheck_operator_node :: (
//     script : *Script, 
//     expr   : *AST.Expression, 
//     node_i : int
// ) -> Type, bool {
    
    
    
// }



