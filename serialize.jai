
/*
    Serialization procedures will allow you to print out the AST so you can save it back to a file after modifying it internally.

    Should be structure almost the exact same as the typecheck_node or execute_node procs, except that we will be appending shit to a string builder
    
    TODO: 
        make it work right
        add a proper concept of unary operators
            otherwise, we will get extra `* -1` for every single time script gets run and reserialized
        
        
        make it not ugly
        preserve whitespace in some manner, preserve comments in file as well
            probably won't preseve whitespace on a given line, but will preseve blank lines between lines with content
        keeping line numbers of all statements shoudl work, but we will also want to track line number that we are currently on, so that we know we are accurate
            otherwise I could see a situation where we end up overshooting where we should be in the file and things get fugged in some manner
        remove extra space before `{` in loops, but keep that extra space for single-line statements

        
    
    
    TOOD: maybe we should change how struct literals with explicit types are parsed so that we don't end up with the wierd thing
    of having and identifier, dot, and struct literal to simply express that the struct literal has an explicit type attached
    instead, when we get the brace after the dot, we should just throw away the identifier and dot and move whatever necessary data is there to the literal node
    and I think it is safe to assume we will not implement more complex type expressions than just simple identifiers
*/


// SPACES_STRING :: "                " // 16 characters long
INDENT_SIZE :: 4;

append_spaces :: (builder: *String_Builder, spaces: int) {
    for 1..spaces append(builder, " ");
}

print_script :: (
    using script:   *Script, 
    builder:        *String_Builder
) -> bool {
    for ast_root.statements {
        print_node_or_return(script, it, builder, 0);
        if !should_skip_placing_semicolon(it) {
            append(builder, ";");
        } 
        if it.next != null {
            append(builder, "\n");
        }
    }
    return true;
}

print_node_or_return :: (
    using script:   *Script, 
    node:           *Node, 
    builder:        *String_Builder,
    indent:         int
) #expand {
    if !print_node(script, node, builder, indent)  `return false;
}

print_node :: (
    using script:   *Script, 
    node:           *Node, 
    builder:        *String_Builder,
    indent:         int
) -> bool {
    if node == null {
        log("Tried to read from null pointer in print_node().");
        // TODO: proabbly print call stack here...
        return true;
    }
     
    if node.node_type == {
      case Node_Directive;
        directive := node.(*Node_Directive);
        print_to_builder(builder, "#%(", directive.name);
        for directive.arguments {
            print_node_or_return(script, it, builder, indent);
            if it.next != null  append(builder, ", ");
        }
        print_to_builder(builder, ")", );
        return true;
        
        
      case Node_Literal;
        literal := node.(*Node_Literal);
        
        if literal.literal_type == {
          case .STRING;
            print_to_builder(builder, "\"%\"", literal.text);
            
          case .NUMBER;
            print_to_builder(builder, "%", to_any(literal.number));
            
          case .STRUCT;
            ti_struct := literal.value_type.(*Type_Info_Struct);
            // print_to_builder(builder, "%.{ ", as_type(ti_struct));
            append(builder, ".");
            print_to_builder(builder, "{ ");
            // what the fuck is going on here? who is printing hte struct name and dot?
            // ohhhh, so I figured it out. we get identifier.struct_literal if the identifier was actually there in code
            // otherwise we would just get the struct literal without the typename or dot
            // maybe this is ok though, and we can jsut hack if for now to add the dot if parent is not a Node_Dot
            
            // ignoring #place members. Wish there were a better way to do this.
            last_member_offset := -1;
            for ti_struct.members {
                if it.flags & .CONSTANT  continue;
                if it.offset_in_bytes < last_member_offset  break;
                
                if last_member_offset != -1  append(builder, ", ");
                last_member_offset = it.offset_in_bytes;
                
                print_to_builder(builder, "%", Any.{ it.type, literal.aggr.value_pointer + it.offset_in_bytes });
            }
            
            append(builder, " }");   
            
          case .ARRAY;
            log("Error: array literals are not yet implemented.");
            return false;
        }
        
        if literal.flags & .IS_MALLEABLE {
            append(builder, "?");
        }
        
        return true;
        
        
      case Node_Identifier;
        append(builder, node.(*Node_Identifier).name);
        return true;
        
        
      case Node_Operation;
        operation := node.(*Node_Operation);
        
        node_prec := get_operator_precedence(operation.name);
        do_parenthesize_left, do_parenthesize_right := false;
        if operation.left.node_type == Node_Operation {
            left_prec := get_operator_precedence(operation.left.(*Node_Operation).name);
            do_parenthesize_left = left_prec < node_prec;
        }
        if operation.right.node_type == Node_Operation {
            right_prec := get_operator_precedence(operation.right.(*Node_Operation).name);
            do_parenthesize_right = right_prec < node_prec;
        }
        
        if do_parenthesize_left  append(builder, "(");
        print_node(script, operation.left, builder, indent);
        if do_parenthesize_left  append(builder, ")");
        append(builder, " ");
        append(builder, operation.name);
        append(builder, " ");
        if do_parenthesize_right  append(builder, "(");
        print_node(script, operation.right, builder, indent);
        if do_parenthesize_right  append(builder, ")");
        return true;
        
        
      case Node_Procedure_Call;
        procedure_call := node.(*Node_Procedure_Call);
        print_node(script, procedure_call.procedure_expression, builder, indent);
        append(builder, "(");
        for procedure_call.arguments {
            print_node_or_return(script, it, builder, indent);
            if it_index != procedure_call.arguments_count-1  append(builder, ", ");
        }
        append(builder, ")");
        return true;
        
        
      case Node_Dot;
        dot := node.(*Node_Dot);
        print_node_or_return(script, dot.left, builder, indent);
        if !(dot.right.node_type == Node_Literal) {// && dot.right.(*Node_Literal).literal_type == .STRUCT) { // @Hack
            append(builder, ".");
        }
        print_node_or_return(script, dot.right, builder, indent);
        return true;
        
        
      case Node_Subscript;
        subscript := node.(*Node_Subscript);
        print_node_or_return(script, subscript.base_expression, builder, indent);
        append(builder, "[");
        print_node_or_return(script, subscript.indexing_expression, builder, indent);
        append(builder, "]");
        return true;
        
        
      case Node_Declaration;
        declaration := node.(*Node_Declaration);
        
        // TODO: improve how this is done...
        literal_value_prev: Any;
        do_temp_literal_overwrite := declaration_is_malleable_literal(declaration);
        if do_temp_literal_overwrite {
            literal := declaration.init_expression.(*Node_Literal);
            assert(literal.value_type == declaration.value_type);
            
            literal_value := get_literal_value_as_any(literal);
            literal_value_prev = Dynamic_New(literal_value.type);
            memcpy(literal_value_prev.value_pointer, literal_value.value_pointer, literal_value.type.runtime_size); // copy current value as prev 
            
            memcpy(literal_value.value_pointer, declaration.value_pointer, literal_value.type.runtime_size); // overwrite current value temporarily with the value stored in declaration
        }
        defer if do_temp_literal_overwrite {
            literal := declaration.init_expression.(*Node_Literal);
            literal_value := get_literal_value_as_any(literal);
            memcpy(literal_value.value_pointer, literal_value_prev.value_pointer, literal_value.type.runtime_size); // restore original value in literal
        }
        
        append(builder, declaration.name);
        if declaration.init_expression  append(builder, " ");
        append(builder, ":");
        if declaration.type_expression {
            append(builder, " ");
            print_node_or_return(script, declaration.type_expression, builder, indent);
            
            if declaration.init_expression  append(builder, " ");
        }
        if declaration.init_expression {
            if declaration.flags & .IS_CONSTANT {
                append(builder, ": ");
            } else {
                append(builder, "= ");
            }
            print_node_or_return(script, declaration.init_expression, builder, indent);
        }
        return true;
        
        
      case Node_Assignment;
        assignment := node.(*Node_Assignment);
        print_node_or_return(script, assignment.left, builder, indent);
        append(builder, " = ");
        print_node_or_return(script, assignment.right, builder, indent);
        return true;
        
        
      case Node_Block;
        block := node.(*Node_Block);
        append(builder, "{\n");
        indent += INDENT_SIZE;
        for block.statements {
            append_spaces(builder, indent);
            print_node_or_return(script, it, builder, indent);
            if !should_skip_placing_semicolon(it) {
                append(builder, ";");
            } 
            if it.next != null {
                append(builder, "\n");
            }
        }
        indent -= INDENT_SIZE;
        append_spaces(builder, indent);
        append(builder, "\n}");
        return true;
        
        
      case Node_If_Statement;
        if_statement := node.(*Node_If_Statement);
        append(builder, "if ");
        print_node_or_return(script, if_statement.condition, builder, indent);
        append(builder, "  ");
        print_node_or_return(script, if_statement.statement, builder, indent);
        return true;
        
        
      case Node_While_Loop;
        while_loop := node.(*Node_While_Loop);
        append(builder, "while ");
        print_node_or_return(script, while_loop.condition, builder, indent);
        append(builder, "  ");
        print_node_or_return(script, while_loop.statement, builder, indent);
        return true;
        
        
      case Node_For_Loop;
        for_loop := node.(*Node_For_Loop);
        append(builder, "for ");
        if for_loop.control_type == {
          case .RANGE;
            print_node_or_return(script, for_loop.range.lower, builder, indent);
            append(builder, "..");
            print_node_or_return(script, for_loop.range.upper, builder, indent);
            
          case .ARRAY;
            print_node_or_return(script, for_loop.array_expression, builder, indent);
        }
        append(builder, "  ");
        print_node_or_return(script, for_loop.statement, builder, indent);
        return true;
    }
    
    log("Error: Invalid node type '%' in print_node().", node.node_type);
    return false;
}


should_skip_placing_semicolon :: (node: *Node) -> bool {
    if node.node_type == {
      case Node_Block; 
        return true;
      case Node_If_Statement;
        return should_skip_placing_semicolon(node.(*Node_If_Statement).statement);
      case Node_While_Loop;
        return should_skip_placing_semicolon(node.(*Node_While_Loop).statement);
      case Node_For_Loop;
        return should_skip_placing_semicolon(node.(*Node_For_Loop).statement);
    }
    return false;
}
