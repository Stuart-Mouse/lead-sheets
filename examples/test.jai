
#import "Basic";
#import "Math";
#import "String";
#import "Hash_Table";
#import "Lead_Sheets"(DEBUG = true);
#import "File";

main :: () {
    script: Script;
    init_script(*script);
    
    register_procedure(*script, "Vec2", (x: float, y: float) -> Vector2 { return .{ x, y }; });
    register_procedure(*script, "pow", pow);
    register_procedure(*script, "sin", sin);
    register_procedure(*script, "cos", cos);
    register_procedure(*script, "echo", (value: float) -> float { print("%\n", value); return value; });
    register_procedure(*script, "echo", (value: int) -> int { print("%\n", value); return value; });
    register_procedure(*script, "echo", (value: Vector2) -> Vector2 { print("%\n", value); return value; });
    register_procedure(*script, "echo", (value: Vector3) -> Vector3 { print("%\n", value); return value; });
    register_procedure(*script, "print", (value: string) -> string { print("%\n", value); return value; });
    register_procedure(*script, "f32", (value: int) -> float { return xx value; });
    register_procedure(*script, "*", #procedure_of_call operator*(Vector3.{}, 1.0));
    
    register_type(*script, "float", type_info(float));
    register_type(*script, "int", type_info(int));
    register_type(*script, "Vector2", type_info(Vector2));
    register_type(*script, "Vector3", type_info(Vector3));
    
    Named_Value :: struct { name: string; value: float; };
    named_values: [..] Named_Value;
    
    add_named_value :: (script: *Script, data: *void, name: string) -> bool {
        print("data %\n", data);
        array := cast(*[..] Named_Value) data;
        new := array_add(array);
        new.name = name;
        set_variable(script, name, new.value); // we can add new external declarations while the script is being parsed
        
        print("array: %\n", array.*);
        
        return true;
    }
    register_directive(*script, "declare", add_named_value, *named_values);
    
    r1: Vector2;
    r2: float;
    r3: int;
    
    arr1: [3] float = .[ 1, 2, 3 ];
    
    x :=  4.0;
    y := 16.0;
    z := Vector2.{ 3, 5 };
    
    a := 3;
    b := 5;
    
    str := "this is a string";
    
    
    Some_Struct :: struct {
        value: float;
        func: (float) -> float;
    }
    
    struct1 := Some_Struct.{ 5, (x: float) -> float { return x * x; } };
    
    
    // TODO: add some proc for 'add_constant' or 'add_local_variable' that will make a copy of the value in the script's pool
    //       this will allow us to pass temporary values, which we cannot do with set_variable
    //       all variables are assumed to be modifiable lvalues, with an externally defined storage duration
    //       better option would be to just add functions for naviagting AST and inserting declarations into a block
    //       we could already add top-level declarations pretty trivially
    
    set_variable(*script, "x", x);
    set_variable(*script, "y", y);
    set_variable(*script, "z", z);
    
    set_variable(*script, "a", a);
    set_variable(*script, "b", b);
    
    set_variable(*script, "r1", r1);
    set_variable(*script, "r2", r2);
    set_variable(*script, "r3", r3);
    
    set_variable(*script, "arr1", arr1);
    set_variable(*script, "struct1", struct1);
    set_variable(*script, "str", str);
    
    print("\nPARSING SCRIPT\n");
    source := read_entire_file("test.ls");
    if !parse_source_file(*script, source) {
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
        
        print("arr1: %\n", arr1);
        print("struct1: %\n", struct1);
        print("str: %\n", str);
        
        print("named values: %\n", named_values);
    } else {
        print("failed!\n");
    }
}

