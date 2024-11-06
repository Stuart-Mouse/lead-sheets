
#import "Basic";
#import "Math";
#import "String";
#import "Hash_Table";

#import "Data_Packer";
#import "dyncall";

#load "token.jai";
#load "ast.jai";
#load "procedure.jai";
#load "binary_op.jai";

/*
    Notes:
    
    todo: typecheck assignment statement and store left side as index to variable
          allow assignment to declare a new variable (type inferred) if no matching name is provided as external variable   
          this internally declared variable will act as a temporary that is in temp storage, so must be flagged as internal so that we can reallocate each time script is run
          
          allow left side of assignment to be an expression
            this will require some concept of L-values
            
    how to parse other types of statements?
    
    begin parsing as expression
        then if we hit a =, we know that what we just parsed needs to be an L value
            means parse expression should flag each node or pass some value back up saying that it can be treated as l value
            variable, and some operations (indexing is considered an operation) can be an L value
        if we just hit a ;, then the expression needs to be a procedure_call, since that is the only expression that can also be a statement on its own
        
    for indexing operator, we may run into some trouble bc of our current typechecking model...
        I wish I had just a bit more experience so I would not be short-sighted! but alas, you dont know what you dont know
        we can't know return type of indexing operator (or which one to apply, i.e. [] vs *[]) until we know whether we are being used in a context wehere it needs to be an lvalue
        perhaps we can just assume *[] and then simply deref in the case that we need to value itself?
            but then in any case we will have to fix up the ast after the intial parse...
    
    indexing operator can probably be its own ast node type for now, that way we can just determine if something is an lvalue 
        based on whether it is type variable or indexer
        since we have all variables by reference at all times, any variable can be an lvalue
        we will want to be able to have overloads on indexing operator, for matrix types or something probably
            this will have its own overload set probably
            
            
            
    'subexpressions' rather than internal variable declarations
    $ before 'variable' name means we are declaring a name for a common subexpression that may be used in later expressions/statements
    while the usage within the script is essentailly the same as a loca variable would be, 
        the distinction of calling it a common subexpression has a purpose in the AST analysis functions that I plan to add
    when we search for certain patterns or forms of expressions, it aid in our analysis if the node is marked as a reference to a subexpression rather than simply as a variable value
    that way we can avoid duplicating work by pulling out subexpressions, but still recognize that those expressions are common to multiple larger expressions
    
        $main_cycle    = time % 5;
        $main_cycle_pi = main_cycle * PI;
        
        tilemaps[1].offset = Vec2.{ 3 * cos(main_cycle), 5 * sin(main_cycle) };
        
        
    
    if left of assignment is to a tilemap's position member
        analyze expressions for vec x and y components
    
    
    we need to have some simple but versatile syntax for matching expressions and extracting them 
    
    match expression form
    extract expression
    extract literals
    when extracting some expression, we want to be able to assert in format string that it is of correct form
    
    
    identify certain forms of expressions with #
        builtins
            #num
            #op
            #var
            #proc
            #expr
        will probably allow user to define more of these?
    
    identify exact expressions by name
    this includes subexpressions declared with $ inside script
    
    
    
    easier way to do certain things for certain subexpressions is just to parameterize jai procedures
    then you can just trivially check an expression ast for the procedure call in question and extract procedure values
    
        $main_cycle = time % 5;
        tilemaps[1].offset = ellipse(main_cycle, 1, 5);
        tilemaps[2].offset = figure_eight(main_cycle, 3, 5);
        
    in above example, we could just pull out values from parameters and use to generate music elements
    
    
    
    Named_Expression :: struct { name: string; expression: AST.Expression; };
    
    get_expr_of_form :: (form: string) -> ([] int, bool) {
        extracts expressions based on a format string
        format string can use $syntax to denote subexpression to extract
    }
    
    // here, I guess type matching on $1 and $2 is determined by provided 'extract' args
    // but, it would be useful to be able to get other subexpression out rather than just values
    extracted, match := get_expr_of_form(script, "$scalar(0) * sin(time % $time_scale(0))");
    if match {
        scalar_expr := get_name_expr(extracted, "scalar");
        if script.nodes[scalar_expr.root].type == .NUMBER
        
    }
    
    
    get_assignments_variable(name: string)
    get_assignments_to_type(type: *Type_Info)
    get_assignments_to_struct_member(base_type: *Type_Info, member: string)
    
*/


// need to remember how to not have to specify the count explicitly here
operator_precedence_table :: struct { op: string; prec: int; } .[
    .{ "+", 1 },
    .{ "-", 1 },
    .{ "*", 2 },
    .{ "/", 2 },
];

Source_Location :: struct {
    line, char: int;
}

Script_Variable :: struct {
    name    : string;
    binding : Any;
}

Script :: struct {
    ast         : AST;
    flags       : Flags;
    variables   : [..] Script_Variable;
    procedures  : [..] Script_Procedure;
    operators   : [..] Script_Procedure; // TODO: enumerate what operators can actually be defined, prevent user registering disallowed operators
    
    // TOOD: replace resizable arrays with views, use resizable in script constructor instead?
    // these really will not need to or even be able to change after ast has been created
    // first, need to make sure that we are not appending to these in any procedures taking a Script rather than constructor
    // maybe we just reference these dynamic arrays by pointer?
    // hard call to make, since we probably want to have common arrays for many procedures/operators that we want to be accessible across multiple scripts
    // maybe we even want to have common variables between scripts for things like global timers and such 
    // maybe we just have global arrays or ones in context and a flag on script to allow access to these arrays
    //      if we use multiple arrays, then we need more than just an index to refer to variables/procs/ops. need some kind of locator struct
    
    Flags :: enum_flags {
        AST_CONSTRUCTED;
    }
}

free_script :: (using script: *Script) {
    array_free(variables);
    memset(*variables, 0, size_of(Resizable_Array));
    array_free(procedures);
    memset(*procedures, 0, size_of(Resizable_Array));
    array_free(operators);
    memset(*operators, 0, size_of(Resizable_Array));
    free_ast(*ast);
}

// ensures no name conflicts
set_variable :: (script: *Script, name: string, any: Any) {
    for *script.variables {
        if it.name == name {
            it.binding = any;
            return;
        }
    }
    array_add(*script.variables, .{ name, any });
}


Script_Constructor :: struct {
    using #as script : *Script;
    tokenizer   : Tokenizer;
    
    // these will be copied to script after constructing ast
    // variables   : [..] Script_Variable;
    // procedures  : [..] Script_Procedure;
    // operators   : [..] Script_Procedure;
}

construct_script_from_source_string :: (script: *Script, source: string) -> bool {
    success: bool;
    defer if !success  free_script(script);
        
    constructor: Script_Constructor;
    constructor.script = script;
    constructor.tokenizer.file = source;
    if !consume_token(*constructor.tokenizer) {
        return false;
    }
    
    parse_statement :: (using ctxt: *Script_Constructor) -> bool {
        // for now, we only have assignement statements, so just do that
        // this is all very much hardcoded for that one case
        
        statement: AST.Statement;
        
        statement.type == .ASSIGNMENT;
        
        ident_token, ok := get_token(*tokenizer);
        if !ok || ident_token.type != .IDENTIFIER {
            return false;
        }
        
        equals_token:, ok = get_token(*tokenizer);
        if !ok || equals_token.type != .EQUALS {
            return false;
        }
        
        // TODO: parse left side as an expression
        // for now it should always be a variable, later can be any valid lvalue
        
        dbg_print("parsed identifier '%' for left side of assignment statement\n", ident_token.text);
        var_idx := resolve_variable(ctxt, ident_token.text);
        if var_idx == -1 {
            dbg_print("Error: unable to resolve variable '%'\n", ident_token.text);
            return false;
        }
        statement.assignment.left = var_idx;
        
        dbg_print("parsing expression for right side of assignment statement\n");
        statement.assignment.right.root = parse_expression(ctxt, 0);
        if statement.assignment.right.root == -1  return false;
        
        semicolon_token:, ok = get_token(*tokenizer);
        if !ok || semicolon_token.type != .SEMICOLON 
            return false;
        
        array_add(*script.ast.statements, statement);
        
        return true;
    }
    
    while true {
        if !parse_statement(*constructor) {
            return false;
        } 
        if peek_token(*constructor.tokenizer).type == .EOF {
            break;
        }
    }
    
    script.flags |= .AST_CONSTRUCTED;
        
    success = true;
    return true;
}

resolve_variable :: (using ctxt: *Script_Constructor, ident: string) -> int {
    for * script.variables {
        if it.name == ident {
            return it_index;
        }
    }
    return -1;
}

resolve_identifier :: (using ctxt: *Script_Constructor, ident: string) -> int {
    dbg_print("resolve_identifier(ident = '%')\n", ident);
    
    token := peek_token(*tokenizer);
    // don't need to check if next token was invalid rn, only need to know if it was or wasn't an open paren
    
    if token.type != .OPEN_PAREN {  
        node := array_add(*ast.nodes);
        node.type = .VARIABLE;
        node.src_text = operator_ident;
        return ast.nodes.count - 1;
    } 
    else {
        if !consume_token(*tokenizer)  return -1; // consume OPEN_PAREN just peeked
        
        argument_nodes: [..] int; // TODO: later we will allocate this array somewhere contiguous with other script constants
        while true {
            node_i := parse_expression(ctxt, 0);
            if node_i == -1  return -1;
            
            dbg_print("added node % to param list\n", sprint_ast_node(ctxt, ast.nodes[node_i]));
            array_add(*argument_nodes, node_i);
            
            token = peek_token(*tokenizer);
            dbg_print("next token after expression in param list: %\n", sprint_token(token));
            if token.type != .COMMA  break;
            if !consume_token(*tokenizer)  return -1; // consume COMMA just peeked
        }
        
        token, ok := get_token(*tokenizer);
        if !ok  return -1;
        if token.type != .CLOSE_PAREN {
            dbg_print("Error, expected a close paren at the end of parameter list!\n");
            return -1;
        }
        
        node := array_add(*ast.nodes);
        node.type = .PROCEDURE_CALL;
        node.src_text = operator_ident;
        node.procedure_call.argument_nodes = argument_nodes;
        return ast.nodes.count - 1;
    }
    
    dbg_print("Unreachable");
    return -1;
}

// TODO: we really need to have a way to properly peek a token

get_operator_precedence :: (op: string) -> int {
    for operator_precedence_table 
        if it.op == op
            return it.prec;
            
    return -1;
}

token_is_binary_operator :: (token: Script_Token) -> bool {
    return token.type == .ADD ||
           token.type == .SUB ||
           token.type == .MUL ||
           token.type == .DIV;
}

parse_leaf :: (using ctxt: *Script_Constructor) -> int {
    token, ok := get_token(*tokenizer);
    if !ok  return -1;
    
    do_unary_minus := token.type == .SUB;
    if do_unary_minus {
        token, ok = get_token(*tokenizer);
        if !ok  return -1;
    }
    
    node_i := -1;
    if token.type == {
        case .OPEN_PAREN;
            dbg_print("encountered open paren...\n");
            node_i = parse_expression(ctxt, 0);
            if node_i == -1  return -1;
            
            token, ok = get_token(*tokenizer);
            if !ok  return -1;
            
            if token.type != .CLOSE_PAREN {
                dbg_print("Error, expected a closing paren!\n");
                return -1;
            }
            dbg_print("consumed corresponding close paren\n");
            
        case .NUMBER;
            node := array_add(*ast.nodes);
            node.type   = .NUMBER;
            node.number = token.number;
            node_i = ast.nodes.count - 1;
            
        case .IDENTIFIER;
            // instead of resolving identifiers completely at AST contruction time, 
            // we only need to check if the identifier was a variable or a procedure
            node_i = resolve_identifier(ctxt, token.text);
            
        case;
            dbg_print("Unexpected token '%'. Expected a number or identifier.\n", sprint_token(token));
            assert(false);
    }
    
    if do_unary_minus {
        // makes a binary op of -1 * node
        neg := array_add(*ast.nodes);
        neg.type   = .NUMBER;
        neg.number = -1;
        neg_i := ast.nodes.count - 1;
        
        node_i = create_binary_operation(ctxt, "*", neg_i, node_i);
    }
    
    return node_i;
}

parse_expression :: (using ctxt: *Script_Constructor, min_prec: int) -> int {
    dbg_print("parse_expression(min_prec = %)\n", min_prec); 

    left_i := parse_leaf(ctxt);
    if left_i == -1  return -1;
    
    dbg_print("parsed a leaf: %\n", sprint_ast_node(ctxt, ast.nodes[left_i]));
    
    while true {
        node_i := parse_increasing_precedence(ctxt, left_i, min_prec);
        if node_i == -1      return -1;
        if node_i == left_i  break;
        
        left_i = node_i;
    }
    
    return left_i;
}

parse_increasing_precedence :: (using ctxt: *Script_Constructor, left_i: int, min_prec: int) -> int {
    dbg_print("parse_increasing_precedence(left_i = %, min_prec = %)\n", left_i, min_prec); 

    next_token := peek_token(*tokenizer);
    
    if !token_is_binary_operator(next_token) {
        dbg_print("peeked token %, but it was not an operator. Returning node % to caller.\n",
            sprint_token(next_token), 
            sprint_ast_node(ctxt, ast.nodes[left_i]), 
        );
        return left_i;
    }
    
    next_token_prec := get_operator_precedence(next_token.text);
    if next_token_prec < min_prec {
        dbg_print("peeked token %, but it was of lower precedence % than min_prec %. Returning node % to caller.\n",
            sprint_token(next_token), 
            next_token_prec,
            min_prec,
            sprint_ast_node(ctxt, ast.nodes[left_i]), 
        );
        return left_i;
    }
    
    if !consume_token(*tokenizer)  return -1;
    
    right_i := parse_expression(ctxt, next_token_prec);
    if right_i == -1  return -1;
    
    return create_binary_operation(ctxt, next_token.text, left_i, right_i);
}


create_binary_operation :: (using ctxt: *Script_Constructor, operator_ident: string, left_i: int, right_i: int) -> int {
    dbg_print("creating binary operation % % %\n", sprint_ast_node(ctxt, ast.nodes[left_i]), operator_ident, sprint_ast_node(ctxt, ast.nodes[right_i]));
    
    node := array_add(*ast.nodes);
    node.type = .OPERATION;
    node.src_text = operator_ident;
    node.operation = .{
        left  = left_i,
        right = right_i,
    };
    
    return ast.nodes.count - 1;
}







get_numeric_value_as_f32 :: (any: Any) -> (float32, bool) {
    if any.type.type == {
        case .INTEGER;
            ti_int := cast(*Type_Info_Integer) any.type;
            if ti_int.signed {
                if any.type.runtime_size == {
                    case 1; return xx (cast(*s8 )any.value_pointer).*, true;
                    case 2; return xx (cast(*s16)any.value_pointer).*, true;
                    case 4; return xx (cast(*s32)any.value_pointer).*, true;
                    case 8; return xx (cast(*s64)any.value_pointer).*, true;
                    case  ; return 0, false;
                }
            } else {
                if any.type.runtime_size == {
                    case 1; return xx (cast(*u8 )any.value_pointer).*, true;
                    case 2; return xx (cast(*u16)any.value_pointer).*, true;
                    case 4; return xx (cast(*u32)any.value_pointer).*, true;
                    case 8; return xx (cast(*u64)any.value_pointer).*, true;
                    case  ; return 0, false;
                }
            }
        case .FLOAT;
            if any.type.runtime_size == {
                case 4; return xx (cast(*float32)any.value_pointer).*, true;
                case 8; return xx (cast(*float64)any.value_pointer).*, true;
                case  ; return 0, false;
            }
    }
    return 0, false;
}

set_numeric_value_from_f32 :: (any: Any, value: float32) -> bool {
    if any.type.type == {
        case .INTEGER;
            ti_int := cast(*Type_Info_Integer) any.type;
            if ti_int.signed {
                if any.type.runtime_size == {
                    case 1; (cast(*s8 )any.value_pointer).* = xx value; return true;
                    case 2; (cast(*s16)any.value_pointer).* = xx value; return true;
                    case 4; (cast(*s32)any.value_pointer).* = xx value; return true;
                    case 8; (cast(*s64)any.value_pointer).* = xx value; return true;
                    case  ; return false;
                }
            } else {
                if any.type.runtime_size == {
                    case 1; (cast(*u8 )any.value_pointer).* = xx value; return true;
                    case 2; (cast(*u16)any.value_pointer).* = xx value; return true;
                    case 4; (cast(*u32)any.value_pointer).* = xx value; return true;
                    case 8; (cast(*u64)any.value_pointer).* = xx value; return true;
                    case  ; return false;
                }
            }
        case .FLOAT;
            if any.type.runtime_size == {
                case 4; (cast(*float32)any.value_pointer).* = xx value; return true;
                case 8; (cast(*float64)any.value_pointer).* = xx value; return true;
                case  ; return false;
            }
    }
    return false;
}



#scope_module

advance :: inline (file: *string, amount := 1) -> bool {
    _amount := min(amount, file.count);
    file.data  += _amount;
    file.count -= _amount;
    return file.count > 0;
}

memzero :: (t: *$T) { memset(t, 0, size_of(T)); };


array_find_where :: (array: []$T, $$cond: (T, *void) -> bool, data: *void) -> int #expand {
    for array  if cond(it, data)  return it_index;
}

is_numeric_type :: (t: Type) -> bool {
    ti := cast(*Type_Info)t;
    return ti.type == .INTEGER || ti.type == .FLOAT;
}

is_numeric_type :: (ti: *Type_Info) -> bool {
    return ti.type == .INTEGER || ti.type == .FLOAT;
}

type_from_info :: inline (ti: *Type_Info) -> Type { return (cast(*Type)*ti).*; }


// copied from how_to/170_modify.jai
Dynamic_New :: (ti: *Type_Info, initialized := true) -> *void {
    size := ti.runtime_size;
    if size < 0 return null;   // Maybe they gave us a weird polymorphic type for some reason.

    memory := alloc(size);

    if initialized {
        if ti.type == .STRUCT {
            // Structs can have initializers. If the initializer is null, the struct is to be zeroed.
            tis := cast(*Type_Info_Struct) ti;
            if tis.initializer  tis.initializer(memory);
            else                memset(memory, 0, size);
        } else {
            // Non-structs always default to 0.
            // @Incomplete: Here we are not handling variants, or arrays of things that might have initializers.
            memset(memory, 0, size);
        }
    }

    return memory;
}

Dynamic_New_Any :: inline (ti: *Type_Info, initialized := true) -> Any {
    memory := Dynamic_New(ti, initialized);
    return ifx memory then Any.{ ti, memory } else Any.{};
}


type_info_to_type :: inline (ti: *Type_Info) -> Type {
    return (cast(*Type)*ti).*;
}