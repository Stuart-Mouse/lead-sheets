
LS :: #import "Lead_Sheets"(DEBUG = true);

#load "imgui_sdl_gl_docking.jai";
#load "imgui_extras.jai";
imgui :: ImGui;

FRAMES_PER_SECOND  :: 60.0;
TICKS_PER_FRAME    :: 1000.0 / FRAMES_PER_SECOND;

script: LS.Script;

input_buffer: [..] u8;

// IMPORTANT NOTE: right now this also serves as the owner for 'source strings' that are referenced by the script!
prev_commands:          [..] struct { statement: string; result: string; }; 
prev_commands_index:    int;

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
        frame_start := SDL_GetTicks();
        reset_temporary_storage();
        
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
        
        if imgui.Begin("Console", *show_another_window) {
            // Reserve enough left-over height for 1 separator + 1 input text
            footer_height_to_reserve := imgui.GetStyle().ItemSpacing.y + imgui.GetFrameHeightWithSpacing();
            if (imgui.BeginChild("ScrollingRegion", imgui.ImVec2.{0, -footer_height_to_reserve}, .None, .HorizontalScrollbar))
            {
                // if (imgui.BeginPopupContextWindow())
                // {
                //     if (imgui.Selectable("Clear")) ClearLog();
                //     imgui.EndPopup();
                // }
    
                // Display every line as a separate entry so we can change their color or add custom widgets.
                // If you only want raw text you can use imgui.TextUnformatted(log.begin(), log.end());
                // NB- if you have thousands of entries this approach may be too inefficient and may require user-side clipping
                // to only process visible items. The clipper will automatically measure the height of your first item and then
                // "seek" to display only items in the visible area.
                // To use the clipper we can replace your standard loop:
                //      for (int i = 0; i < Items.Size; i++)
                //   With:
                //      ImGuiListClipper clipper;
                //      clipper.Begin(Items.Size);
                //      while (clipper.Step())
                //         for (int i = clipper.DisplayStart; i < clipper.DisplayEnd; i++)
                // - That your items are evenly spaced (same height)
                // - That you have cheap random access to your elements (you can access them given their index,
                //   without processing all the ones before)
                // You cannot this code as-is if a filter is active because it breaks the 'cheap random-access' property.
                // We would need random-access on the post-filtered list.
                // A typical application wanting coarse clipping and filtering may want to pre-compute an array of indices
                // or offsets of items that passed the filtering test, recomputing this array when user changes the filter,
                // and appending newly elements as they are inserted. This is left as a task to the user until we can manage
                // to improve this example code!
                // If your items are of variable height:
                // - Split them into same height items would be simpler and facilitate random-seeking into your list.
                // - Consider using manual call to IsRectVisible() and skipping extraneous decoration from your items.
                imgui.PushStyleVar(.ItemSpacing, imgui.ImVec2.{4, 1}); // Tighten spacing
                // if (copy_to_clipboard)
                //     imgui.LogToClipboard();
                // for (const char* item : Items)
                // {
                //     if (!Filter.PassFilter(item))
                //         continue;
    
                //     // Normally you would store more information in your item than just a string.
                //     // (e.g. make Items[] an array of structure, store color/type etc.)
                //     ImVec4 color;
                //     bool has_color = false;
                //     if (strstr(item, "[error]")) { color = ImVec4(1.0f, 0.4f, 0.4f, 1.0f); has_color = true; }
                //     else if (strncmp(item, "# ", 2) == 0) { color = ImVec4(1.0f, 0.8f, 0.6f, 1.0f); has_color = true; }
                //     if (has_color)
                //         imgui.PushStyleColor(ImGuiCol_Text, color);
                //     imgui.TextUnformatted(item);
                //     if (has_color)
                //         imgui.PopStyleColor();
                // }
                // if (copy_to_clipboard)
                //     imgui.LogFinish();
                
                for prev_commands {
                    imgui.Text(it.statement);
                    imgui.TextColored(.{0.5, 0.5, 0.5, 1.0}, "-> %", it.result);
                }
    
                // Keep up at the bottom of the scroll region if we were already at the bottom at the beginning of the frame.
                // Using a scrollbar or mouse-wheel will take away from the bottom edge.
                // if (ScrollToBottom || (AutoScroll && imgui.GetScrollY() >= imgui.GetScrollMaxY()))
                //     imgui.SetScrollHereY(1.0f);
                // ScrollToBottom = false;
    
                imgui.PopStyleVar();
            }
            imgui.EndChild();
            imgui.Separator();
            
            // input buffer
            imgui.SetNextItemWidth(-FLOAT32_MIN);
            do_execute := InputTextDynamic("input", *input_buffer, .EnterReturnsTrue);
            imgui.SetItemDefaultFocus();
            if do_execute {
                ImGui.SetKeyboardFocusHere(-1); // keep focus on text box after hitting enter
                str := to_string(input_buffer.data);
                if str {
                    // NOTE: string must be copied here because LS uses view onto source file...
                    //       we should probably have some flag that make LS copy the strings it needs from tokens into its pool!
                    str = copy_string(str);
                    value, ok := LS.evaluate_statement(*script, str, store_global_declarations = true);
                    if !ok {
                        log("\tFailed to evaluate statement.");
                    } else {
                        result: string;
                        if value.value_pointer then result = sprint("%", value);
                        array_add(*prev_commands, .{ str, result });
                        array_reset_keeping_memory(*input_buffer);
                    }
                }
            }
        }
        imgui.End();
        
        if imgui.Begin("Declarations") {
            table_flags := imgui.TableFlags.BordersV | .BordersOuterH | .RowBg;
            if imgui.BeginTable("###Declarations", 3, table_flags) {
                imgui.TableSetupColumn("Name",  .NoHide);
                imgui.TableSetupColumn("Value", .NoHide);
                imgui.TableSetupColumn("Type",  .NoHide);
                // TODO: maybe flags to show if constant / malleable?
                imgui.TableHeadersRow();
                
                for script.global_declarations {
                    decl_value := Any.{ it.value_type, it.value_pointer };
                    TableTreeNodeAny(it.name, decl_value);
                }
                
                // TODO: this is probably slow because we walk the entire ast to show the declarations instead of storing them in some array.
                //       but not really worried about that at the moment since scripts are intended to be small anyhow.
                walk_proc :: (script: *LS.Script, node: *LS.Node, data: *void) -> (bool, *LS.Node) {
                    if node.node_type == LS.Node_Declaration {
                        decl := node.(*LS.Node_Declaration);
                        decl_value := Any.{ decl.value_type, decl.value_pointer };
                        TableTreeNodeAny(decl.name, decl_value);
                    }
                    return true, null;
                }
                LS.walk_nodes(*script, xx *script.ast_root, walk_proc, null);
                
                imgui.EndTable();
            }
        }
        imgui.End();
        
        if imgui.Begin("External Variables") {
            table_flags := imgui.TableFlags.BordersV | .BordersOuterH | .RowBg;
            if imgui.BeginTable("###Variables", 3, table_flags) {
                imgui.TableSetupColumn("Name",  .NoHide);
                imgui.TableSetupColumn("Value", .NoHide);
                imgui.TableSetupColumn("Type",  .NoHide);
                imgui.TableHeadersRow();
                
                for script.variables {
                    TableTreeNodeAny(it.name, it.binding);
                }
                imgui.EndTable();
            }
        }
        imgui.End();
        
        
        if imgui.Begin("External Procedures") {
            table_flags := imgui.TableFlags.SizingStretchProp | .BordersV | .BordersOuterH | .RowBg;
            if imgui.BeginTable("###Procedures", 3, table_flags) {
                imgui.TableSetupColumn("Name",  .NoHide);
                imgui.TableSetupColumn("Address", .NoHide);
                imgui.TableSetupColumn("Type",  .NoHide);
                imgui.TableHeadersRow();
                
                for script.procedures {
                    imgui.TableNextRow(); imgui.TableNextColumn();
                    imgui.Text(it.name);
                    
                    imgui.TableNextColumn();
                    imgui.Text("%", it.ptr);
                    
                    imgui.TableNextColumn();
                    imgui.Text("%", (*it.type).(*Type).*);
                }
                imgui.EndTable();
            }
        }
        imgui.End();
        
        if imgui.GetMouseCursor() == {
            case .Arrow;       SDL_SetCursor(cursor_arrow);
            case .TextInput;   SDL_SetCursor(cursor_text_input);
            case .Hand;        SDL_SetCursor(cursor_move);
            case .ResizeNS;    SDL_SetCursor(cursor_resize_ns);
            case .ResizeEW;    SDL_SetCursor(cursor_resize_ew);
            case .ResizeNESW;  SDL_SetCursor(cursor_resize_nesw);
            case .ResizeNWSE;  SDL_SetCursor(cursor_resize_nwse);
        }

        // Rendering
        imgui.Render();
        glViewport(0, 0, xx io.DisplaySize.x, xx io.DisplaySize.y);
        glClearColor(clear_color.x, clear_color.y, clear_color.z, clear_color.w);
        glClear(GL_COLOR_BUFFER_BIT);
        ImGui_ImplSdl_RenderDrawLists(imgui.GetDrawData());
        
        frame_time := SDL_GetTicks() - frame_start;
        sleep_time := TICKS_PER_FRAME - frame_time;
        if sleep_time > 0 {
            SDL_Delay(xx sleep_time);
        }
        
        SDL_GL_SwapWindow(window);
    }

    // Cleanup
    ImGui_ImplSdl_Shutdown();
    imgui.DestroyContext(imgui_context);
    SDL_GL_DeleteContext(gl_context);
    SDL_DestroyWindow(window);
    SDL_Quit();
}

