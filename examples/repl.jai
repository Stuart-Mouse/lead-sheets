
LS :: #import "Lead_Sheets";

#load "imgui_sdl_gl_docking.jai";
#load "imgui_extras.jai";
imgui :: ImGui;

script: LS.Script;

input_buffer: [..] u8;

// IMPORTANT NOTE: right now this also serves as the owner for 'source strings' that are referenced by the script!
prev_commands:          [..] struct { statement: string; result: string; }; 
prev_commands_index:    int;

enable_modifying_declarations := false; // badly named. not modifying the decl, just its current value

main :: () {
    UA :: #import "Unmapping_Allocator";
    context.allocator = UA.get_unmapping_allocator();
    
    SDL_Init(SDL_INIT_VIDEO);

    window := SDL_CreateWindow("Lead Sheets ImGui Example", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 1280, 720, SDL_WINDOW_OPENGL|SDL_WINDOW_RESIZABLE);

    // Check that the window was successfully created
    if window == null {
        // In the case that the window could not be made...
        log_error("Could not create window: %", to_string(SDL_GetError()));
        exit(1);
    }

    log("%", to_string(imgui.GetVersion()));

    //SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
    //SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, xx SDL_GL_CONTEXT_PROFILE_CORE);

    gl_context := SDL_GL_CreateContext(window);
    if !gl_context {
        log_error("Could not create OpenGL context: %", to_string(SDL_GetError()));
        exit(1);
    }

    gl_load(*gl, SDL_GL_GetProcAddress);

    imgui_context := imgui.CreateContext();

    // Setup imgui binding
    ImGui_ImplSdl_Init(window);

    // Setup style
    imgui.StyleColorsClassic();

    io := imgui.GetIO();
    io.ConfigFlags_ |= .DockingEnable;

    show_demo_window := true;
    show_another_window := false;
    clear_color := Vector4.{0.45, 0.55, 0.60, 1.00};
    
    array_reserve(*input_buffer, 8);
    
    LS.init_script(*script);
    defer LS.free_script(*script);
    
    LS.register_common_types(*script);
    LS.register_type(*script, "Vector2", type_info(Vector2));
    
    LS.register_procedure(*script, "pow", pow);
    LS.register_procedure(*script, "sin", sin);
    LS.register_procedure(*script, "cos", cos);
    
    echo :: (value: $T) -> T { print("%\n", value); return value; };
    LS.register_procedure(*script, "echo", (value: float) -> float { print("%\n", value); return value; });
    LS.register_procedure(*script, "echo", (value: int) -> int { print("%\n", value); return value; });
    LS.register_procedure(*script, "echo", #procedure_of_call echo(Vector2.{}));
    
    LS.register_procedure(*script, "length", #procedure_of_call length(Vector2.{}));
    LS.register_procedure(*script, "scale", (vec: *Vector2, scale: float) { vec.* *= scale; });
    LS.register_procedure(*script, "print", (value: string) { print("%\n", value); });

    a := 3;
    b := 5;
    LS.set_variable(*script, "a", a);
    LS.set_variable(*script, "b", b);
    
    // Main loop
    done := false;
    while !done {
        // You can read the io.WantCaptureMouse, io.WantCaptureKeyboard flags to tell if dear imgui wants to use your inputs.
        // - When io.WantCaptureMouse is true, do not dispatch mouse input data to your main application.
        // - When io.WantCaptureKeyboard is true, do not dispatch keyboard input data to your main application.
        // Generally you may always pass all inputs to dear imgui, and hide them from your application based on those two flags.
        event: SDL_Event;
        while SDL_PollEvent(*event) {
            ImGui_ImplSdl_ProcessEvent(*event);
            if event.type == SDL_QUIT   done = true;
            if event.type == SDL_WINDOWEVENT && event.window.event == SDL_WINDOWEVENT_CLOSE && event.window.windowID == SDL_GetWindowID(window) {
                done = true;
            }
        }
        ImGui_ImplSdl_NewFrame(window);
        // Start the frame. This call will update the io.WantCaptureMouse, io.WantCaptureKeyboard flag that you can use to dispatch inputs (or not) to your application.
        imgui.NewFrame();
        
        wvp := imgui.GetWindowViewport();
        imgui.DockSpaceOverViewport(wvp, .PassthruCentralNode);
        
        imgui.ShowDemoWindow();
        
        /* 
            TODO: 
            
            clear input on execute
            allow use of enter to execute
            allow multi-line input
            make input textbox size up to fit input?
            allow user to hit up to get previous executed statement
            probably reference the example console from imgui demo
        */
        
        imgui.Begin("Console", *show_another_window);
        for prev_commands {
            imgui.Text(it.statement);
            imgui.TextColored(.{0.5, 0.5, 0.5, 1.0}, "-> %", it.result);
        }
        // do_execute := imgui.Button("execute");
        // imgui.SameLine();
        imgui.SetNextItemWidth(-FLOAT32_MIN);
        do_execute := InputTextDynamic("input", *input_buffer, .EnterReturnsTrue);
        if do_execute {
            str := to_string(input_buffer.data);
            if str {
                // NOTE: string must be copied here because LS uses view onto source file...
                //       we should probably have some flag that make LS copy the strings it needs from tokens into its pool!
                str = copy_string(str);
                value, ok := LS.evaluate_statement(*script, str, append_to_script_root = true);
                if !ok {
                    log("\tFailed to evaluate statement.");
                } else {
                    result: string;
                    if value.value_pointer  result = sprint("%", value);
                    array_add(*prev_commands, .{ str, result });
                    array_reset_keeping_memory(*input_buffer);
                }
            }
        }
        imgui.End();
        
        if imgui.Begin("Declarations") {
            imgui.Checkbox("Modify", *enable_modifying_declarations);
            
            if enable_modifying_declarations {
                // TODO: this is probably slow because we walk the entire ast to show the declarations instead of storing them in some array.
                //       but not really worried about that at the moment since scripts are intended to be small anyhow.
                walk_proc :: (script: *LS.Script, node: *LS.Node, data: *void) -> (bool, *LS.Node) {
                    if node.node_type == LS.Node_Declaration {
                        decl := node.(*LS.Node_Declaration);
                        decl_value := Any.{ decl.value_type, decl.value_pointer };
                        TreeNodeAny(temp_c_string(decl.name), decl_value);
                    }
                    return true, null;
                }
                LS.walk_nodes(*script, xx *script.ast_root, walk_proc, null);
            } else {
                table_flags := imgui.TableFlags.SizingFixedFit | .BordersV | .BordersOuterH | .RowBg;
                if imgui.BeginTable("###Declarations", 3, .BordersV | .BordersOuterH | .RowBg) {
                    imgui.TableSetupColumn("Name",  .NoHide);
                    imgui.TableSetupColumn("Value", .NoHide);
                    imgui.TableSetupColumn("Type",  .NoHide);
                    // TODO: maybe flags to show if constant / malleable?
                    imgui.TableHeadersRow();
                    
                    // TODO: this is probably slow because we walk the entire ast to show the declarations instead of storing them in some array.
                    //       but not really worried about that at the moment since scripts are intended to be small anyhow.
                    walk_proc :: (script: *LS.Script, node: *LS.Node, data: *void) -> (bool, *LS.Node) {
                        if node.node_type == LS.Node_Declaration {
                            decl := node.(*LS.Node_Declaration);
                            decl_value := Any.{ decl.value_type, decl.value_pointer };
                            
                            imgui.TableNextRow(); imgui.TableNextColumn();
                            imgui.Text(decl.name);
                            if !decl.name  print("{% %} %", decl.name.data, decl.name.count, string.{ 1, decl.name.data });
                            
                            imgui.TableNextColumn();
                            imgui.Text("%", decl_value);
                            
                            imgui.TableNextColumn();
                            imgui.Text("%", (*node.value_type).(*Type).*);
                        }
                        return true, null;
                    }
                    LS.walk_nodes(*script, xx *script.ast_root, walk_proc, null);
                    
                    imgui.EndTable();
                }
            }
        }
        imgui.End();
        
        if imgui.Begin("External Variables") {
            table_flags := imgui.TableFlags.SizingFixedFit | .BordersV | .BordersOuterH | .RowBg;
            if imgui.BeginTable("###Variables", 3, .BordersV | .BordersOuterH | .RowBg) {
                imgui.TableSetupColumn("Name",  .NoHide);
                imgui.TableSetupColumn("Value", .NoHide);
                imgui.TableSetupColumn("Type",  .NoHide);
                imgui.TableHeadersRow();
                
                for script.variables {
                    imgui.TableNextRow(); imgui.TableNextColumn();
                    imgui.Text(it.name);
                    
                    imgui.TableNextColumn();
                    imgui.Text("%", it.binding);
                    
                    imgui.TableNextColumn();
                    imgui.Text("%", (*it.binding.type).(*Type).*);
                }
                imgui.EndTable();
            }
        }
        imgui.End();
        
        if imgui.GetMouseCursor() == {
            case .Arrow;         SDL_SetCursor(cursor_arrow);
            case .TextInput;     SDL_SetCursor(cursor_text_input);
            case .Hand;          SDL_SetCursor(cursor_move);
            case .ResizeNS;      SDL_SetCursor(cursor_resize_ns);
            case .ResizeEW;      SDL_SetCursor(cursor_resize_ew);
            case .ResizeNESW;    SDL_SetCursor(cursor_resize_nesw);
            case .ResizeNWSE;    SDL_SetCursor(cursor_resize_nwse);
        }

        // Rendering
        imgui.Render();
        glViewport(0, 0, xx io.DisplaySize.x, xx io.DisplaySize.y);
        glClearColor(clear_color.x, clear_color.y, clear_color.z, clear_color.w);
        glClear(GL_COLOR_BUFFER_BIT);
        ImGui_ImplSdl_RenderDrawLists(imgui.GetDrawData());
        SDL_GL_SwapWindow(window);
    }

    // Cleanup
    ImGui_ImplSdl_Shutdown();
    imgui.DestroyContext(imgui_context);
    SDL_GL_DeleteContext(gl_context);
    SDL_DestroyWindow(window);
    SDL_Quit();
}

