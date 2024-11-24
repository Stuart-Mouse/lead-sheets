
#import "Basic";
#import "Math";
#import "String";
#import "Hash_Table";
#import "Lead_Sheets";
#import "File";

main :: () {
    input := read_entire_file("test.ls");
    
    // input := "r1 = Vec2(3, 4) * 2;"; // assignment to aggr, dyncall, builtin operator
    // input := "r2 = x;";              // assignment to non-aggr
    // input := "r1.y = z.x + z.y;";    // member access as lvalue and rvalue
    // input := "r3 = a + b;";          // overloaded operator (int + int)
    // input := #string DONE
    // arr1[1 + z.y - pow(2, 2)] = 17;
    // r2 = arr1[1 + sin(3.14159/2)];
    // DONE;
    // input := "r2 = 5 * 2 - 2;";
    
    // input := "if 1 > 0  echo(5);";
    // input := #string DONE 
    // r2 = 5; 
    // while r2 > 1 {
    //     r2 = r2 - 1;
    //     echo(r2);
    // }
    // DONE;
    
    /*
        this was failing because unary minus was being applied as lower precedence than range operator
            well, really it was only getting its precedence from how it is applied in parse_expression in such a hacky way
            we need to be able to make unary operators actually obey operator precedence, but I haven't quite figured that one out yet
        another issue is that if we have an operation in the range expression here, then the type hint for int will not flow through the operation
            I am really not sure how to solve that one, since the typechecking there could get really nasty really quickly.
            a
        OK, now we got it to use s64 here by just simply passing the type hint through the operation node
        but of course this will probably not work in many cases, since this assumes that result type of operation is the same as the inputs
        but maybe it really only matters that we solve this simple case?
        still, we should probably put in logic to widen an int (especially an int-typed .NUMBER node) to a float if the other operand is a float
    */
    // input := "for -2..5  echo(it);";
    
    // test procedure variables
    // input := "r2 = struct1.func(struct1.value);";
    
    script: Script;
    init_script(*script);
    
    register_procedure(*script, "Vec2", (x: float, y: float) -> Vector2 { return .{ x, y }; });
    register_procedure(*script, "pow", pow);
    register_procedure(*script, "sin", sin);
    register_procedure(*script, "cos", cos);
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
    
    
    Some_Struct :: struct {
        value: float;
        func: (float) -> float;
    }
    
    struct1 := Some_Struct.{ 5, (x: float) -> float { return x * x; } };
    proc1 := (x: float) -> float { return x * x; };
    
    // TODO: add some proc for 'add_constant' or 'add_local_variable' that will make a copy of the value in the script's pool
    //       this will allow us to pass temporary values, which wecannot do with set_variable
    //       all variables are assumed to be modifiable lvalues, with an externally defined storage duration
    
    print("%\n", struct1.func);
    
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
    set_variable(*script, "proc1", proc1);
    
    print("*arr1: %\n", *arr1);
    print("*r1: %\n", *r1);
    print("*r2: %\n", *r2);
    print("*r3: %\n", *r3);

    print("\nCONSTRUCTING SCRIPT\n");
    if !parse_source_file(*script, input) {
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
    } else {
        print("failed!\n");
    }
}

