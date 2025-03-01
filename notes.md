
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
    we can't know return type of indexing operator (or which one to apply, i.e. `[]` vs `*[]`) until we know whether we are being used in a context wehere it needs to be an lvalue
    perhaps we can just assume `*[]` and then simply deref in the case that we need to value itself?
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

```
    $main_cycle    = time % 5;
    $main_cycle_pi = main_cycle * PI;
    
    tilemaps[1].offset = Vec2.{ 3 * cos(main_cycle), 5 * sin(main_cycle) };
``` 
    

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
```
    $main_cycle = time % 5;
    tilemaps[1].offset = ellipse(main_cycle, 1, 5);
    tilemaps[2].offset = figure_eight(main_cycle, 3, 5);
```
in above example, we could just pull out values from parameters and use to generate music elements


```
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
```





In regards to usage in rosebud:

the general idea for level scripts is that they will be one script for the entire level
like a better version of the gon files I used for defining movement components in OE

these scripts should all be one file so that the designer can get the overview of all components and how they relate in a single place
because there may be some "local variables" that we want to declare in each level/script,
    I would like to add an #init section at the top of the script that will only be run once.
    alternatively, I could just add if statements and have that be managed separately
    but I sort of want to keep the scripts as basic as possible
    then again, if statements could also be very useful for things like switches and timers
        I am really not sure about this though, because the levels should aslo be basically discretely evaluated
        and having any real state in a scripts breaks this
            but maybe that is thinking too specifically about my use case for that one game




another idea to document here before I forget:
    attach callbacks to nodes to run before/after node is evaluated
    this could be useful for the music system so that it can get live input as the values of certain expressions change
        e.g., we could alter the pitch/pan of some sound channel as a tile layer moves through its cycle

lots of the musical elements of lead sheets would be good to first try out in a more general visualizer program, 
    based just around making movement components and listening to them interact



if and for statements
    will require a hierarchical structure to statements
    we can probably get by with keeping a float array of statements and change method of iteration (e.g. C gon parser)
    but if we do this, we should try to make it easy to refactor later on if need be
    so probably use an interface proc to add sub-statements to a block-type statement
    
    will require creation of block scopes of different types
        I don't want to have scoping on declarations (only thing to declare right now are subexpressions)
            though we may also want to have variables that are parameters to script that are declared within script itself, but that can be modified outside script after its compiled
                we will have generated imgui menus to display and modify these sorts of variables
        different scope type for struct literals, declaration scope
    





arena allocator for all script data
    we need to allocate contiguous space for Any's or at least pointers to all arguments to procedure call
        but if we use an arena allocator, it won't work very nicely to use a resizable array
        but we also cannot get the argument count before actually parsing each argument expression
        maybe we could do a sort of temporary linked-list thing where we store index to next argument's node on previous argument's node
            this would be pretty low cost and we could then allocate space for all argument Any's after we know size
    
    procedure call arguments
        arg nodes connected in linked list sort of way during parsing
        for typechecking, we will either need to create temp array view for arg types/anys
            or we we could write our own typechecking routine which just iterates args in linked list way
        for the actual execution, we do want all args in contiguous memory though
        
    what things to allocate and where
        numeric constants don't need to be allocated, since value is stored directly on node
            if we later go to a bytecode, these will need to be copied
        all other literals will all go in a section of 'constant storage' during first pass of typechecking
        
        as we typecheck the script, keep track of how much stack space we will need for intermediate values, and where each node's intermediate value will be on the stack
        after typechecking, allocate exact amount of stack space needed and + room to place some sentinal bytes after stack end, so that we can make sure our logic is correct 
            problem, we don't know how much stackspace we will need on first pass of typechecking, becuase we don't know for certin the type of parent node until child is checked
                so, we will have to just make a second pass for now, I guess
        
        how to layout arguments for proc calls...
            ideally, these would all be contiguous, then we could use only one pointer to point directly to values of arguments
                then when we do dyncall we just bump this pointer as we push args
            so for each argument, which needs to do its own work on stack, it needs to know where to place return value
        
        for each node, we get one offset into stack
        at bottom of stack we are to place the return value
        then above that we may have other data that node needs in order to complete its operations
            for operators, this will be it two operands (by value)
            for procedures, this will be the arguments (also by value)
                doing it by value here should be fine, since in this case we will want an rvalue
        
        `[ constants | stack space ... sentinel bytes ]`
        
    flat pool usage
        since we are using a large flat pool for much of script data, we can probably just allocate all nodes in here as well and then use pointers between nodes
        this will make some sections of code much nicer than using indices, as we have been doing until now
        (side note: this is probably also something we should do for GON parser as well)
        But there are still many things that I would like to allocate contiguously, which means we need to think about steps
        
        `[ nodes | constants | stack space ... sentinel bytes ]`
        
        can we get all nodes allocated before doing constants? probably.
            nodes are created during parse/ast construction
            constants and variables are allocated during typechecking first pass
            stack space calculation done during typecheck second pass
            

now that we have cleaned up the parser a bit
    would be useful to have a mode of expression parsing that does not even go to ast
        would have to resolve identifiers on the spot
            could do in callback
        would have to lean heavily on temp storage or pool to allocate intermediate values
        would have to do all type checking entirely dynamically
    this would be very handy for the gon expression parsing though
        since then we can just use a callback that identifies and evaluates node references
        problem though, we can't know what all nodes are referenced in order to check for circular dependencies until after we parse out expressions
        
first order of business should be just adding indexers

indexers will encompass both struct member access and array indexing
because both of these can return valid lvalues, and are essentially the same operations of offsetting a pointer by some amount

how that pointer is offset and where that base pointer is located depends on base type
so for a struct, it just means offsetting from base value_pointer
for an array, it depends further on array type


lvalue vs rvalue
for lvalue, indexer will result in different type coming out than going in
we could optimize out indexers on constants / literals, but we will need to keep them on variables, since base pointer of variable is liable to change

we will do arrays after struct members, bc arrays will bring all kinds of complications
for example, will we allow appending to arrays?
    probably, yes
        but only dynamic arrays
will we allow access to array members as if we are accessing Array_View_64
    almost certainly not
how will we handle user passing arrays to jai procs?
    if we want to be able to pass e.g. a resizable array to an array_add in script
    then we will have to be able to treat the any as an lvalue
        also woul dhave to propogate up the typechecking that we need an lvalue,
        but that is not something we do!
        not only that, but we actually cant even handle polymorphic procedures as script procs
        so we would have to have a builtin proc for array add
        then maybe we could do the whole thing up propogating up that we need an lvalue
    this will require changes to dyncall, maybe?
        nah, just alterations to our custom typechecking
        we will actually have to entirely rewrite the typechecking to take the ast nodes instead of just `Any`s or `*Type_Info`s
        but that was needed anyhow


in order to be able to do member accesses on structs properly, we need to stop pushing structs themselves onto the stack as values
    instead, store struct values in constant section of script before stack space
    then only push and pop pointers to structs on the script stack

when we do a member access, we will pop a struct pointer, and place back down either a register-sized primitive value or a pointer to a different struct 


`[ nodes | variables and constants (values) | stack space ... sentinel bytes ]`
need somewhere to place an actual array of Any's metadata for variables and constants in script
could do another pass of ast to allocate after we have all values down
```
nodes               : [] AST.Node;
struct/array values : [] u8;
stack               : [] u8;
externals           : [] Script_Variable;
locals              : [] Local_Variable;
```

Maybe we should have a value union on node for all register-size values?

also, maybe we should just make push/pop on stack work explicitly only for register-size values
    that way we can just be sure that alignment of values is not sucky
    we probably also want to do some alignment on values allocated in local variables segment
    this would elminate need for the get_stack_required proc, since all items would use exactly 8 bytes of stack

if everything is register size, our binary op proc also gets simpler

one issue though, is that for binops and procs we either have to push empty space, or we have to push a pointer to dst
and the pointer given to the binary op execution proc mus tbe given either the stack dst or the value dst
we can't do a pop-then-push thing for binary op or proc call, since there's no way around providing ptr to return val in struct case
    
    this complication makes it where we can simply use a push

if all items on stack are the same size, then we easily can know how far to set back stack pointer for proc call case
    arg_count + 1 (for return value);


Unfortunately, its not completely trivial to desugar overloaded operators directly into procedure calls
we should be able to do this no problem once we are in bytecode, but for executing from AST it creates problems
    not that we couldn't do it, but it seems like it would create some unnecessary complexity

...unless we just decide to put operator overlaods into the same array as script procedures
    maybe this is the way to go since it would allow us to make this transformation in the typechecking step
        and this would allow us to simplify code in several places

also, since we will need to change how procedures are typechecked anywyas, this may be an even bigger help to simplify in the first place
typechecking on procedure arguments should look at the nodes themselves for the arguments so that we can coerce/cast values in a similar way to what jai does
    this is not urgent by any means but it would be quite useful 



        

TODO: 
    better typechecking on procedure arguments
        will require type casting
    remove need for variables and procedures resizable arrays in script struct
        move to constructor
        allocate fixed array view for these (only those variables/procedures that actually get used)
        add additional flags for variables (internal/external, constant, intermediate, subexpression value)
    add subexpression declarations


    procedures as variables
        currently, parsing and typecheckin of procedures is completely separate from variables
        but variables could be procedures, and an expression can evaluate to a procedure pointer that maybe we want to be able to call

    
iterators are very much hardcoded right now
also for loops only work on arrays, not ranges
    we will probably want to be able to do for loops on numeric ranges, or at least slice arrays and iterateresulting slice
    

whe resolving an identifier for a procedure, we should first search all variables for a match
failing that, we search the procedures array, find all candidate procedures, and then resolve the best overload



make management of scopes better
resolve declarations within nested scopes
node needs pointer to prev and parent in addition to next







IDEAS FOR LATER

named blocks that can be called like funcitons, but don't take arguments
these named blocks can then have new statements compiled ad added to them dynamically
this would be useful for some application(s) maybe





Problem:
    we have two conflicting design issues to deal with
    
    resolving identifiers 
    identifier node contains resolved declaration details
        this was much nicer than having to point to a separate node for resolved declaration because that would requires us to make nodes for external variable and proceudres 
        but now this is a problem for iterators, because we need a flag that notifies a node that it's variable_ptr has an additional indirection on it
            this is because the iterator needs to update a pointer to the `it` value
            but because there's also no real concept of pointers in the scripts, indirections have to basically be handled automatically
            and this extra indirection is a problem basically
            i am not explaining this well
            in any case, this would not be an actual problem if we instead have a pointer to a single resolved declaration
            maybe we can still get the best of both worlds though by just making a Node_Declaration again which can be for any of the resolved types that are currently on the Node_Identifier
             
    


TODO:
    
    use linked list for things that acutally need to have next/prev nodes instead of just having that on every node type
    probably make it so that local variables get pushed on scope entry and popped on scope exit
        this may require some refactoring like collecting all declarations in a scope and putting them in one spot
        but this will be needed if we do the callable blocks thing, or ever do proper functions
    add lexer callback or some means to insert declarations while parsing
    it may be good to actually store the source location on nodes again, so that we can use to check if an identifier is used before its declaration in a more robust way
    actually try to use scripts to make squares move around and change color
    declare and call named blocks
        not really proper preocedures, since they won't take parameters, just subroutines or way of splitting up code into sections
    
    for strings, we will probably want to make it so that we can push larger things than single-register values
    also, I'm not sure I actually made dyncall work with strings in the first place (lol, lmao even)

need to write more notes and formalize rules about compilation pipeline
    
    can add new declarations of external variables through parsing directives
    but cannot add or remove external variables once typechecking begins
    
    parsing basically produces the full ast
        we probably should not need to be creating new nodes during the typechecking phase
            maybe some fringe cases for things like iterators or other such declarations that need to get automatically inserted
    
    what if we want to parse thigns from multiple files and insert them into the same script?
        maybe these should be considered separate named blocks?
            then how to link and call between blocks?
    
If I can keep the script syntactically and semantically very close to native Jai code, then it should be a trivial conversion int he case that I ever want to simply convert soem of my scripts into real jai code.



Directives:

for now, we will jsut evaluate all arguments in parameter list and pass them to directive procedure straightforwardly
for now, we will only accept a single bool return as a signifier of whether an error has occured

in future, we should allow a directive to return values
    probably only one value though
    if directive returns a `*Node`, we insert that node in the place where the directive was
    if directive returns a different type, we will insert that value as a literal
    
also, in the future we should definitely just pass the raw `*Node` if that is the argument type that a directive accepts






Going to probably branch further off of gon and overhaul the syntax, focus more on the lead sheets integration
    and probably remove weird inconsistencies between how field refs and code expressions work
    
but first step is changing some things here in lead sheets
particularly, I want to stop passing around the script to everything and make parsing/typechecking/execution more individually usable

disentangle execution/evaluation from allocation in pool
make it easier to swap the allocator used for nodes, 

we will always have to allocate nodes, since the grammar is too complex to do everything purely on the stack
    plus we already need temp storage for intermediates, so may as well just allocate the nodes there and make life easier
    


    
first
    write evaluate_node proc
        thought this would requre factoring all the nodes out into their own typecheck routines, but the thing is, its just too damn recursive
        we can't really use the existing typechecking proc or factoring it any way that allows us to use it piecemeal in the way we would need to to implement an evaluate_node proc
        and besides, I suppose it only makes sense that typechecking is all done at the same time, rather than interleaving it with execution or evaluation
        
        so wat we really want is just an execute_temp proc that uses temp storage for the results instead of the stack
        
        
    test this with existing framework
second
    figure out how to nicely swap node allocator in script

test run
    switch to using evaluate and temp storage for directives 
        and just throw away the node afterwards



script state used during...
    parsing
        directives
            
        pool / allocator
            can probably just push this as context.allocator
            but need to be careful about that...
            also a bit tricky maybe to disentangle this from the stack that we allocate in the pool before the nodes?
        
        lexer
        
        current_parent_node
            this can be passed as parameter, but this is cumbersome
    

both typechecking and execution need access to the the variables and procedures arrays
    for procedures, we really could just store the fn ptr, 
    but for variables we have to be abel to sub them out, so there will always be a need for an additional indirection





the biggest thing preventing just having a totally general 'evaluate' or 'interpret' proc that just takes a string of text input as expression and then evaluates it is that we have tied our allocation and execution (due to using the stack)
    we could certainly just set our script's allocator to temp in order to interpret throwaway expressions, but then we still need the stack to execute those
    so, we should also have the option to use temp storage for our intermediate values
    and maybe we can even clean uip after ourselves with the whole set_watermark thing, assuming that's still in the language



we do want to put ref to token on node after all if we do end up wanting source info for the node
this will keep our nodes slimmer and the tokens are honestly not too much to hold on to
    this would also mean we could just tokenize the entire script at once if we wanted to, which would give better locality to both our tokens and nodes
    
likewise for bytecode instructions in the future, we should hold on to a pointer to the node (only in debug) so that we can, well, debug.

regardless of whether we want to keep nodes or throw them away, we should run all expressions through the same basic pipeline of parse/typecheck/eval


evaluate doing its own typecheck may actually work out
    and maybe it really is better to do everything in a single traversal of the AST
because we basically have either leaf nodes
    literals, identifiers don't call into recursive typechecking and may benefit from special coercions
    then there are just structural nodes like block, if, for, while
    and then misc like cast, subscript, dot
    the real tricky part is primarily gonna be the big messy nodes like operation and procedure
    but maybe we can break these up in a nicer way?

random
    maybe we don't do declarations in eval?
    malleable literals don't make sense in eval
    neither do directives, really. but that's harmless I guess
    tbh, maybe even assignments don't make sense in an eval?
        nvm, can make sense if lvalue is external variable







thinking about use of script as a context for evaluating scripts for a builtin console
we could use the stack for executing statements / expressions on if we really wanted to, but eval + temp storage works just as well for that
a more interesting idea is being able to play with declarations and scope in the console/repl
user could get and store variables in the console's script ctxt and then use those later
and if we wanted to just scope some stuff for a short while, we could have some directive to push/pop a scope as a little throwaway workspace
or, we could even save the current scope/workspace and its variables to a file, since we can serialize our scripts pretty well
if we add named blocks, you could also write and run blocks by name

```
#block(block_name)
do_stuff(a, b, c)
do_stuff(x, y, z)
#end_block()

#run_block(block_name)
```


named blocks will essentially be like functions in the script
they wont' be run like everything else in the global scope if you jsut call execute_script()
they can be called by name with #run(block_name)
more like subroutines that functions, and they maintain their own state just like things in the global scope can
don't take parameters or return values (just write a proc in jai and call that)

I could write an init script for my game that would restore the state or current level and such or set certain conditions 
just simple things like:
```
debug_init: {
    load_level("platform.lvl");
    set_player_powerup_state(.HAMMER);
    debug_rendering = .PLAYER_VECTORS | .TILEMAP_COLLISION;
}
```
then any time i build in debug I just call the debug_init block on startup



default procedure parameters
    because type info for procedures doesn't contain info about default parameter values, we end up having to be pretty explicit in the scripts
    though tbh I've never had a procedure take more than like 2 parameters yet
    though I'm sure I will have that happen with some of the editro ui handles stuff coming up
    so one thought is to give the user the option for certain procedures to get default values for stuff that we would like to just be contextual to the script
    we can already insert a sort of explicit context for the script in the form of external variables and external procedures, but this is more like a contextual variable that gets filled in automatically for a specific procedure
    
    idk.. I tried to write out an exampel and couldnt even think of a good contrived one
    so this felt like a good idea for 1 minute and now I'm not sure if it would ever be useful tbhj





TODO: need to slim down our base node a bit if possible
parent (scope) can probably become implicit, being passed as param in parsing and typechecking procedures
then we only need to store the parent block on any other nodes which introduce scope
    which, we should probably factor out to a common node type like Code_Scope and simplify resolution of declarations
    or maybe we just do that with a procedure?

maybe we don't need source location on base node, especially if we hold on to source tokens and link back to those for the purposes of whitespace/comments preservation
    those tokens will probably use a Source_Code_Range instead of location

but, we do want to be able to attach a source code location to some jai code call site for nodes that are added to an AST programatically or from user input
    and we will need a flag on node to signify that node was inserted outside the context of parsing a larger file

also, storing next node is still somewhat questionable. It would be better if we had a real array of *Node, or maybe a real doubly-linked list structure for nodes
main issue is that we want to be able to replace nodes more easily
and the place wehere that is hard to do is in lists of nodes
because when we are iterating over the list we can't easily get a **Node, which is what we need to replace

step 1
    remove parent *Node from bade Node, move to a Node_Scope?
    fix up identifier resolution
OR  don't remove parent
    use proper doubly linked list for connecting nodes
    make it easy to replace nodes
    
step 2
    remove source location
    lex all tokens into one big array
    save tokens and refer back to them from nodes
    


constants expressions, constant declarations, and malleable literals
    these three are all sort of intereacting in a not-idela way at the moment
    one thing I owuld like to be able to do with constant declarations is change them so that they're really just AST node references
    but we mark these 'constant' declarations with the same flag that we use for saying that an expression is constant
    and really, we'd like to be able to use this AST reference semantics with non-constant expressions
    this would especially be useful in the context of a debug console, since we could basically use those 'constant' declarations' identifiers like macros for larger expressions
    for example `Entities :: get_active_entities(get_active_level());`
    
    
    maybe the solution is just to make a .MACRO flag that is used for declarations instead of the CONSTANT flag
    that we the two concepts are distinct
    then maybe we can even do some parameterized macros as a replacement for having procedure declarations in scripts
    I really don't want to have procedures if you can't tell
        for simle expressions that you may want to parameterize, we have macros
        then for organizational purposes we have named blocks, which don't take parameters but can have their own internal state through external variables or malleable literals
    
    for constant expressions then, we will be able to pre-evaluate those form thr root-most node and simplify them down into single literals
        this will be a good step to run before lowering to bytecode
        
    
    another consideration with constants is whether we even want to consider them at all in evaluation context
    for example, it is kind of neat to be able to just use a non-constant type expression in declaring a variable in the debug console
    this will probably just be some context flag in the script at a later point when we make certain aspects more configurable
        along with things like using remap_data for automatic coercions, making LS feel a bit more dynamically-typed while still not actually being so
            though we could also add some directive that would re-type a node
    
    
    ok, so the problem with the macros right now is that we don't yet have a deep copy for ast nodes 
        which we will need, since we actually need to typecheck different instanciations of the macro individually
        
    we probably want to change the way macros are parsed so that they can't get a type expression in the same way as normal declarations
    we want the macros to be a bit more polymorphic i guess, so that means 
    
    but maybe copying nodes in the macro way actually breaks the ability to name malleable literals
        since we would jsu tbe duplicating the literal node
        and we actually want to point to the same literal node, since that's what has the underlying value we care about...
    so im not sure if we can have these malleable literals and also macros that actually copy nodes
    
    we could make the macros simply be references to the nodes in question, but then they are not polymorphic, just shorthands
        i guess righ tnow I don't care to make the macros polymorphic and only want them for the shorthand aspect...
            BUT the problem is we really NEED to retypcheck on macro instanciation because the macro may be used as either an lvalue or rvalue
            and this is really where the main issue lies...
            
    so the only option lef tis the hard option, which is to do some fancy stuff when we clone nodes for a macro
    for the most part, we can probably just clone nodes as-is, but at least for malleable literals, we need to make sure that all instances share the same value_pointer and value_type
        which also means that we place some limits on how polymorphic a macro containin a malleable literal can be, since the malleable literal's type needs to stay the same across all instanciations
        that leaves a tiny bit of wiggle room, but not much 
        the other question is what to do about type inference on malleble literals? I guess we just have to be explicit about types there, or maybe the type gets dictated based on the 
        
    yeah, if we want macros to be polymorphic at all, then we need to take out the type expression and and malleable literals used therein need to be explicitly typed (or default to float or w/e)

    due to all the complexity that this sounds like, I will probably just leave the macro stuff for a later date, after I make some more real progress on the game
    it owuld be a good time to make sure everything still works properly, fix serialization, and maybe even do the whitepsace stuff first before adding such complex features
    once we do get the macro thing working though, we can definitely use that to make the foreach more interesting
    
    
    
    
    
directives improvements
    we really want to just use evaluate_directive and not execute_directive
    
    but perhaps better than that we just use the # character to denot that the following expression or statmeent should be run immediately
        sort of liek a #run in jai
        but with the added semantics that if the return value is a `*Node`, then that node gets appended in place of the 'directive'
    then our directives can just be arbitrary expressions or chains of expresisons that transform nodes and and not just procedure calls
    

