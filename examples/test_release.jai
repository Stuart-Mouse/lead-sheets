





/* 

  Earlier examples showed how a metaprogram can create workspaces and make target programs
  compile in those workspaces.

  When you do this, you can read a stream of messages to see how compilation is going,
  and use those messages in constructive ways. The ability to do this is one of the most
  powerful things about this programming language, so you really want to know it. But
  for now we are keeping the example simple and not too fancy.

*/

build :: () {
    // 
    // Create a second workspace to contain the program we want to compile.
    // We can pass a name to compiler_create_workspace that gets reported
    // back to us in error messages:
    //
    w := compiler_create_workspace("Target Program");
    if !w {
        print("Workspace creation failed.\n");
        return;
    }

    target_options := get_build_options(w);
    target_options.output_executable_name = "test";
    set_build_options(target_options, w);
    set_optimization(*target_options, .VERY_OPTIMIZED);
    

    compiler_begin_intercept(w);

    add_build_file("test.jai", w);  // @Cleanup: Fix this to not require #filepath.

    compiler_end_intercept(w);

    print("\nDone!\n\n");

    set_build_options_dc(.{do_output=false});
}

#run build();


#import "Basic";
#import "Compiler";

