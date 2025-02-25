
LS :: #import "Lead_Sheets";

#load "imgui_sdl_gl_docking.jai";
#load "imgui_extras.jai";
imgui :: ImGui;

script: LS.Script;

input_buffer: [..] u8;

// prev_commands:          [..] struct { statement: string, result: string };
prev_commands:          [..] string;
prev_commands_index:    int;

main :: () {
    SDL_Init(SDL_INIT_VIDEO);

    window := SDL_CreateWindow("imgui SDL2+OpenGL example", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 1280, 720, SDL_WINDOW_OPENGL|SDL_WINDOW_RESIZABLE);

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

    f: float = 0.0;

    LS.init_script(*script);
    defer LS.free_script(*script);

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
        
        /* 
            TODO: 
            
            clear input on execute
            allow use of enter to execute
            allow multi-line input
            make input textbox size up to fit input?
            allow user to hit up to get previous executed statement
            probably reference the example console from imgui demo
            
            figure out why dockspace is not happening
        */
        
        imgui.Begin("Lead Sheets Console", *show_another_window);
        for prev_commands {
            imgui.Text(it);
        }
        InputTextDynamic("input statement or expression", *input_buffer);
        if imgui.Button("execute") {
            statement := to_string(input_buffer.data);
            value, ok := LS.evaluate_statement(*script, statement, append_to_script_root = true);
            if !ok {
                log("\tFailed to evaluate statement.");
            } else {
                array_add(*prev_commands, sprint("% -> %", statement, value));
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

