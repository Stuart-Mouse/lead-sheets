
#import "Basic";
#import "Math";
#import "String";
#import "Hash_Table";
#import "Lead_Sheets";

main :: () {
    // input := "Vec2(1, 2) * pow(2, sin(3.14159/2) + 2)";
    input := #string DONE
    r1 = -Vec2(3, 4) * 5;
    r2 = (1 + 2) * pow(2, sin(3.14159/2) + 2);
    DONE;
    // input := "result = 3 + 4 * 5;";
    
    script: Script;
    
    set_basic_operators(*script);
    
    register_procedure(*script, "Vec2", (x: float, y: float) -> Vector2 { return .{ x, y }; });
    register_procedure(*script, "pow", pow);
    register_procedure(*script, "sin", sin);
    register_procedure(*script, "cos", cos);
    
    r2: Vector2;
    r1: float;
    x: s64 = 4;
    y: u8  = 16;
    
    set_variable(*script, "x", x);
    set_variable(*script, "y", y);
    set_variable(*script, "r1", r2);
    set_variable(*script, "r2", r1);
    
    if !construct_script_from_source_string(*script, input) {
        print("failed to construct script from source!\n");
        return;
    }
    
    ok := execute_script(*script);
    if ok {
        print("\nresult: %\n", r1);
        print("result_2: %\n", r2);
    }
}

