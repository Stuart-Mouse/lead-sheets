
/*
    Serialization procedures will allow you to print out the AST so you can save it back to a file after modifying it internally.

    Should be structure almost the exact same as the typecheck_node or execute_node procs, except that we will be appending shit to a string builder
    
    TODO: 
        make it not ugly
        preserve whitespace in some manner, preserve comments in file as well
            probably won't preseve whitespace on a given line, but will preseve blank lines between lines with content
        keeping line numbers of all statements shoudl work, but we will also want to track line number that we are currently on, so that we know we are accurate
            otherwise I could see a situation where we end up overshooting where we should be in the file and things get fugged in some manner
        remove extra space before `{` in loops, but keep that extra space for single-line statements
        
*/


SPACES_STRING :: "                "; // 16 characters long
INDENT_SIZE :: 4;

append_spaces :: (builder: *String_Builder, spaces: int) {
    for 1..spaces/SPACES_STRING.count append(builder, SPACES_STRING);
    append(builder, slice(SPACES_STRING, 0, spaces%SPACES_STRING.count));
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
        // if it_index != ast_root.statements.count-1 then append(builder, "\n");
    }
    return true;
}

sprint_node :: (script: *Script, node: *Node) -> string {
    builder: String_Builder;
    print_node(script, node, *builder, 0);
    return builder_to_string(*builder);
}

append_trivia :: (builder: *String_Builder, node: *Node) {
    append(builder, ifx node.trivia);
}

print_node_or_return :: (
    using script:   *Script, 
    node:           *Node, 
    builder:        *String_Builder,
    indent:         int
) #expand {
    if !print_node(script, node, builder, indent)  `return false;
}

// TODO: make sure this works when node has not passed typechecking, so that we can at least still print parts of AST in error messages
print_node :: (
    using script:   *Script, 
    node:           *Node, 
    builder:        *String_Builder,
    indent:         int
) -> bool {
    if node == null {
        log("Tried to read from null pointer in print_node().");
        print_stack_trace(context.stack_trace);
        return false;
    }
    
    // TODO: why do we have this exception?
    if node.node_type != Node_Operation {
        append_trivia(builder, node);
    }
    
    
    // wrap expression in parenthesized if explicitly flagged as such
    if       node.flags & .PARENTHESIZED  append(builder, "(");
    defer if node.flags & .PARENTHESIZED  append(builder, ")");
    
    if node.node_type == {
      case Node_Directive;
        directive := node.(*Node_Directive);
        print_to_builder(builder, "#%(", directive.name);
        for directive.arguments {
            print_node_or_return(script, it, builder, indent);
            if it_index != directive.arguments.count-1 then append(builder, ",");
        }
        print_to_builder(builder, ")", );
        return true;
        
        
      case Node_Literal;
        literal := node.(*Node_Literal);
        
        if #complete literal.literal_type == {
          case .ANY;
            print_to_builder(builder, "<literal ANY of type %>", as_type(literal.value_type));
        
          case .STRING;
            print_to_builder(builder, "\"%\"", literal.text);
            
          case .NUMBER;
            print_to_builder(builder, "%", to_any(*literal.number));
            
          case .BOOLEAN;
            print_to_builder(builder, "%", to_any(*literal.number));
            
          case .STRUCT;
            ti_struct := literal.value_type.(*Type_Info_Struct);
            append(builder, ".{");
            
            // NOTE: we only want to serialize a struct using its current value if it is a malleable literal
            if (node.flags & .TYPECHECKED) && (node.flags & .IS_MALLEABLE) {
                log("serializing struct literal with value: %", Any.{ node.value_type, literal.aggr.value_pointer });
                
                // ignoring #place members. Wish there were a better way to do this.
                last_member_offset := -1;
                for ti_struct.members {
                    if it.flags & .CONSTANT  continue;
                    if it.offset_in_bytes < last_member_offset  break;
                    
                    if last_member_offset != -1  append(builder, ",");
                    last_member_offset = it.offset_in_bytes;
                    
                    if it_index < literal.aggr.expressions.count {
                        append_trivia(builder, literal.aggr.expressions[it_index]);
                    }
                    print_to_builder(builder, "%", Any.{ it.type, literal.aggr.value_pointer + it.offset_in_bytes });
                }
            } else {
                for literal.aggr.expressions {
                    print_node(script, it, builder, indent);
                    if it_index != literal.aggr.expressions.count-1  append(builder, ",");
                }
            }
            
            append(builder, " }");   
        }
        
        if literal.flags & .IS_MALLEABLE {
            append(builder, "?");
        }
        
        return true;
        
        
      case Node_Identifier;
        name := get_identifier_name(script, xx node);
        do_backtick := !is_legal_identifier(name);
        if do_backtick  append(builder, "`");
        append(builder, name);
        if do_backtick  append(builder, "`");
        return true;
        
        
      case Node_Operation;
        operation := node.(*Node_Operation);
        _operator := get_operator(operation);
        
        if _operator.kind == {
          case .PREFIX;
            append(builder, operation.name);
            print_node(script, operation.left, builder, indent);
            return true;
            
          case .POSTFIX;
            print_node(script, operation.left, builder, indent);
            append(builder, operation.name);
            return true;
        }
        
        // we only parenthesize expressions implicitly here if the precedence requires it
        // and the subexpressions weren't already explicitly flagged as parenthesized
        node_prec := _operator.precedence;
        do_parenthesize_left, do_parenthesize_right := false;
        if operation.left.node_type == Node_Operation {
            left_prec := get_precedence(xx operation.left);
            do_parenthesize_left = left_prec < node_prec && !(operation.left.flags & .PARENTHESIZED);
        }
        if operation.right.node_type == Node_Operation {
            right_prec := get_precedence(xx operation.right);
            do_parenthesize_right = right_prec < node_prec && !(operation.right.flags & .PARENTHESIZED);
        }
        
        if do_parenthesize_left  append(builder, "(");
        print_node(script, operation.left, builder, indent);
        if do_parenthesize_left  append(builder, ")");
        // append(builder, " ");
        append_trivia(builder, node);
        append(builder, operation.name);
        // append(builder, " ");
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
            if it_index != procedure_call.arguments.count-1  append(builder, ",");
        }
        append(builder, ")");
        return true;
        
      
      case Node_Cast;
        _cast := node.(*Node_Cast);
        print_node_or_return(script, _cast.value, builder, indent);
        if !(_cast.flags & .IMPLICIT) {
            append(builder, ".(");
            print_node_or_return(script, _cast.type_expression, builder, indent);
            append(builder, ")");
        }
        return true;
        
        
      case Node_Dot;
        dot := node.(*Node_Dot);
        if dot.left  print_node_or_return(script, dot.left, builder, indent);
        if !(dot.right.node_type == Node_Literal && dot.right.(*Node_Literal).literal_type == .STRUCT) { // @Hack
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
        
        do_backtick := !is_legal_identifier(declaration.name);
        if do_backtick  append(builder, "`");
        append(builder, declaration.name);
        if do_backtick  append(builder, "`");
        
        if !declaration.type_expression  append(builder, " ");
        append(builder, ":");
        if declaration.type_expression {
            // append(builder, " ");
            print_node_or_return(script, declaration.type_expression, builder, indent);
            if declaration.init_expression  append(builder, " ");
        }
        if declaration.init_expression {
            if declaration.flags & .MACRO {
                append(builder, ":");
            } else {
                append(builder, "=");
            }
            print_node_or_return(script, declaration.init_expression, builder, indent);
        }
        return true;
        
        
      case Node_Block;
        block := node.(*Node_Block);
        if block.name {
            print_to_builder(builder, "%: ", block.name);
        }
        append(builder, "{");
        indent += INDENT_SIZE;
        for block.statements {
            // append_spaces(builder, indent);
            print_node_or_return(script, it, builder, indent);
            if !should_skip_placing_semicolon(it) {
                append(builder, ";");
            } 
            // if it_index != block.statements.count-1 then append(builder, "\n");
        }
        indent -= INDENT_SIZE;
        append(builder, "\n");
        append_spaces(builder, indent);
        append(builder, "}\n");
        return true;
        
        
      case Node_If_Statement;
        if_statement := node.(*Node_If_Statement);
        append(builder, "if");
        print_node_or_return(script, if_statement.condition, builder, indent);
        // append(builder, "  ");
        print_node_or_return(script, if_statement.statement, builder, indent);
        return true;
        
        
      case Node_While_Loop;
        while_loop := node.(*Node_While_Loop);
        append(builder, "while");
        print_node_or_return(script, while_loop.condition, builder, indent);
        // append(builder, "  ");
        print_node_or_return(script, while_loop.statement, builder, indent);
        return true;
        
        
      case Node_For_Loop;
        for_loop := node.(*Node_For_Loop);
        if for_loop.control_type == {
          case .RANGE;
            append(builder, "for");
            print_node_or_return(script, for_loop.range.lower, builder, indent);
            append(builder, "..");
            print_node_or_return(script, for_loop.range.upper, builder, indent);
            
          case .ARRAY;
            append(builder, "for");
            print_node_or_return(script, for_loop.array_expression, builder, indent);
            
          case .LIST;
            append(builder, "foreach");
            append(builder, "(");
            for for_loop.list {
                print_node_or_return(script, it, builder, indent);
                if it_index != for_loop.list.count-1 then append(builder, ",");
            }
            append(builder, ")");
        }
        // append(builder, "  ");
        print_node_or_return(script, for_loop.statement, builder, indent);
        return true;
    }
    
    assert(false, "Invalid node type '%' in print_node().", node.node_type);
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
