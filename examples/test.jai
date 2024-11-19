
#import "Basic";
#import "Math";
#import "String";
#import "Hash_Table";
#import "Lead_Sheets";


main :: () {
    // input := #string DONE
    // r1 = -Vec2(3, 4) * 5;
    // r2 = (1 + 2) * pow(2, sin(3.14159/2) + 2);
    // r3 = a + b;
    
    // while r2 > 20 {
    //     r2 = r2 - 1;
    //     r3 = r3 + r3;
    // }
    
    // for arr1  if it > 1  it = f32(it_index) * 7;
    // for arr1  echo(it);
    
    // DONE;
    
    // input := "r1 = Vec2(3, 4) * 2;"; // assignment to aggr, dyncall, builtin operator
    // input := "r2 = x;";              // assignment to non-aggr
    // input := "r1.y = z.x + z.y;";    // member access as lvalue and rvalue
    // input := "r3 = a + b;";          // overloaded operator (int + int)
    // input := #string DONE
    // arr1[1 + z.y - pow(2, 2)] = 17;
    // r2 = arr1[1 + sin(3.14159/2)];
    // DONE;
    // input := "r2 = 5 * 2 - 2;";
    input := "for -9..5  echo(it);";
    
    script: Script;
    
    register_procedure(*script, "Vec2", (x: float, y: float) -> Vector2 { return .{ x, y }; });
    register_procedure(*script, "pow", pow);
    register_procedure(*script, "sin", sin);
    register_procedure(*script, "cos", cos);
    register_procedure(*script, "+", (x: int, y: int) -> int { return x + y; });
    register_procedure(*script, "echo", (value: float) -> float { print("%\n", value); return value; });
    register_procedure(*script, "echo", (value: int) -> int { print("%\n", value); return value; });
    register_procedure(*script, "echo", (value: Vector2) -> Vector2 { print("%\n", value); return value; });
    register_procedure(*script, "f32", (value: int) -> float { return xx value; });
    
    r1: Vector2;
    r2: float;
    r3: int;
    
    arr1: [3] float = .[ 1, 2, 3 ];
    
    x :=  4.0;
    y := 16.0;
    z := Vector2.{ 3, 5 };
    
    a := 3;
    b := 5;
    
    // TODO: add some proc for 'add_constant' or 'add_local_variable' that will make a copy of the value in the script's pool
    //       this will allow us to pass temporary values, which wecannot do with set_variable
    //       all variables are assumed to be modifiable lvalues, with an externally defined storage duration
    
    set_variable(*script, "x", x);
    set_variable(*script, "y", y);
    set_variable(*script, "z", z);
    
    set_variable(*script, "a", a);
    set_variable(*script, "b", b);
    
    set_variable(*script, "r1", r1);
    set_variable(*script, "r2", r2);
    set_variable(*script, "r3", r3);
    
    set_variable(*script, "arr1", arr1);
    
    print("\nCONSTRUCTING SCRIPT\n");
    if !construct_script_from_source_string(*script, input) {
        print("failed to construct script from source!\n");
        return;
    }
    
    print("\nEXECUTING SCRIPT\n");
    ok := execute_script(*script);
    if ok {
        print("\nRESULTS:\n");
        print("r1: %\n", r1);
        print("r2: %\n", r2);
        print("r3: %\n", r3);
    } else {
        print("failed!\n");
    }
}

