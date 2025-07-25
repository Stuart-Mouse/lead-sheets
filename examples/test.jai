
#import "Basic";
#import "Math";
#import "String";
#import "Hash_Table";
#import "Lead_Sheets"()(DEBUG = true, USING_DYNCALL = false);
#import "File";

Fruits :: enum {
    APPLE;
    PEAR;
    BANANA;
    STRAWBERRY;
}

main :: () {
    script: Script;
    init_script(*script);
    
    register_common_types(*script);
    register_type(*script, "Vector2", type_info(Vector2));
    register_type(*script, "Vector3", type_info(Vector3));
    register_type(*script, "Fruits",  type_info(Fruits));
    
    echo :: (value: $T) -> T { print("%\n", value); return value; };
    
    register_procedure(*script, "Vec2", (x: float, y: float) -> Vector2 { return .{ x, y }; });
    register_procedure(*script, "pow", pow);
    register_procedure(*script, "sin", sin);
    register_procedure(*script, "cos", cos);
    register_procedure(*script, "echo", (value: float) -> float { print("%\n", value); return value; });
    register_procedure(*script, "echo", (value: int) -> int { print("%\n", value); return value; });
    register_procedure(*script, "echo", #procedure_of_call echo(Vector2.{}));
    register_procedure(*script, "echo", #procedure_of_call echo(Vector3.{}));
    register_procedure(*script, "echo", #procedure_of_call echo(Fruits.APPLE));
    register_procedure(*script, "length", #procedure_of_call length(Vector2.{}));
    register_procedure(*script, "print", (value: string) -> string { print("%\n", value); return value; });
    register_procedure(*script, "f32", (value: int) -> float { return xx value; });
    register_procedure(*script, "*", #procedure_of_call operator*(Vector3.{}, 1.0));
    register_procedure(*script, "scale", (vec: *Vector2, scale: float) { vec.* *= scale; });
    
    
    Named_Value :: struct { name: string; value: float; };
    named_values: [..] Named_Value;
    
    add_named_value :: (using call: *Directive_Call, array: *[..] Named_Value, name: string) -> bool {
        if phase == .PARSE {
            new := array_add(array);
            new.name = name;
            set_variable(script, name, new.value); // we can add new external declarations while the script is being parsed
        }
        return true;
    }
    register_directive(*script, "declare", add_named_value, *named_values);
    
    make_binary :: (using call: *Directive_Call, left: *Node, right: *Node) -> bool, *Node {
        if phase == .TYPECHECK {
            node := alloc_node(script, Node_Operation);
            node.name          = "+";
            node.operator_type = .BINARY;
            node.left          = left;
            node.right         = right;
            print("left : %\n", left);
            print("right: %\n", right);
            print("Creating operation node: % + %\n", sprint_node(script, left), sprint_node(script, right));
            return true, node;
        }
        return true, null;
    }
    register_directive(*script, "add", make_binary);
    
    
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
    source_path := "test.ls";
    source := read_entire_file(source_path);
    if !parse_source_file(*script, source, source_path) {
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
    
    
    print("\nSERIALIZING SCRIPT\n");
    builder: String_Builder;
    print_script(*script, *builder);
    output := builder_to_string(*builder);
    write_entire_file("output.ls", output);
    
    
    // print("\nWALK NODES TEST\n");
    // walk_nodes(*script, script.ast_root, #code { // TODO: report this, it will crash the compiler
    // walk_nodes(*script, xx *script.ast_root, #code {
    //     if node.node_type == Node_Identifier {
    //         print("%\n", node.(*Node_Identifier).name);
    //     }
    // });
    
    
    // print("\nEVAL TEST\n");
    // {
    //     expressions_to_test := struct { expr: string; result: Any; }.[
    //         .{ "v := 3", 3.0 },
    //         .{ "v + 5", 8.0 },
    //         .{ "Vector2.{ 1, 2 } * 3", Vector2.{ 1, 2 } * 3 },
    //         .{ "(1 + 2) * pow(2, sin(3.14159/2) + 2)", (1 + 2) * pow(2, sin(3.14159/2) + 2) }
    //     ];
        
    //     for expressions_to_test {
    //         log("Evaluating statement: '%'", it.expr);
    //         value, ok := evaluate_statement(*script, it.expr, store_global_declarations = true);
    //         if !ok {
    //             log("\tFailed to evaluate statement.");
    //             continue;
    //         };
    //         if !compare_any(value, it.result) {
    //             log("\tValue did not match expected result!");
    //         }
    //         log("\tvalue:    (%) %", as_type(value.type), value);
    //         log("\texpected: (%) %", as_type(it.result.type), it.result);
    //     }
    // }
    
    
    // print("\nTEST NAMED BLOCKS\n");
    // block := get_named_block(*script, "test_block");
    // ok = execute_node(*script, block, explicit_call = true);
    // if !ok  log("failed to execute %", "test_block");
}

compare_any :: (a: Any, b: Any) -> bool {
    if a.type != b.type  return false;
    return a.type == null || !memcmp(a.value_pointer, b.value_pointer, a.type.runtime_size);
}

as_type :: inline (ti: *Type_Info) -> Type {
    return (*ti).(*Type).*;
}