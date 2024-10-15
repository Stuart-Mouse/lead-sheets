
#import "Basic";
#import "Math";
#import "String";
#import "Hash_Table";
#import "Lead_Sheets";

main :: () {
    // input := "1+2+3";
    // input := "1*2+3";
    // input := "(3+1)*2";
    // input := "(3 + x) * (2 + y) / 2";
    // input := "sin(3.14159/2)";
    
    input := "(1 + 2) * pow(2, sin(3.14159/2) + 2)";
    
    script : Lead_Sheet;
    ok     : bool;
    
    script.tokens, ok = tokenize(input);
    for script.tokens  print_token(it);
    
    var_x: s64 = 4;
    var_y: u8  = 16;
    
    array_add(*script.variables, .{ "x", var_x });
    array_add(*script.variables, .{ "y", var_y });
    
    array_add(*script.procedures, .{
        "sin", (params: []float64) -> float64, bool {
            if params.count != 1 {
                print("Invalid number of parameters passed to sin. Expected 1, got %\n", params.count);
                return 0, false;
            }
            
            return sin(params[0]), true;
        }
    });
    array_add(*script.procedures, .{
        "cos", (params: []float64) -> float64, bool {
            if params.count != 1 {
                print("Invalid number of parameters passed to cos. Expected 1, got %\n", params.count);
                return 0, false;
            }
            
            return cos(params[0]), true;
        }
    });
    array_add(*script.procedures, .{
        "pow", (params: []float64) -> float64, bool {
            if params.count != 2 {
                print("Invalid number of parameters passed to pow. Expected 2, got %\n", params.count);
                return 0, false;
            }
            
            return pow(params[0], params[1]), true;
        }
    });
    
    script.ast.root = parse_expression(*script, 0);
    if script.ast.root == -1 {
        print("Unable to parse expression!");
        return;
    }
    
    print_ast(*script, script.ast.root);
    
    result := evaluate_expression(*script, script.ast.root);
    print("\n\nresult: %\n", result);
}
