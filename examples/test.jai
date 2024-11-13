
#import "Basic";
#import "Math";
#import "String";
#import "Hash_Table";
#import "Lead_Sheets";


main :: () {
    input := #string DONE
    r1 = -Vec2(3, 4) * 5;
    r2 = (1 + 2) * pow(2, sin(3.14159/2) + 2);
    DONE;
    
    script: Script;
    
    register_procedure(*script, "Vec2", (x: float, y: float) -> Vector2 { return .{ x, y }; });
    register_procedure(*script, "pow", pow);
    register_procedure(*script, "sin", sin);
    register_procedure(*script, "cos", cos);
    
    r1: Vector2;
    r2: float;
    
    x :=  4.0;
    y := 16.0;
    z := Vector2.{ 3, 5 };
    
    set_variable(*script, "x", x);
    set_variable(*script, "y", y);
    set_variable(*script, "z", z);
    set_variable(*script, "r1", r1);
    set_variable(*script, "r2", r2);
    
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
    } else {
        print("failed!\n");
    }
}

