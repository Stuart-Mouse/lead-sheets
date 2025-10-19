
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
        
        current_scope
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
    



refactoring lexer to allow pre-tokenization of entire file
    will help with attaching token reference to ast nodes, while keeping some semblance of cache locality between nodes
    we actually have a major problem here, which is that this method of attaching tokens to nodes by pointer requires 
    that our get_token and peek_token procs now return tokens by pointer rather than by value
    which means that all our tokens need to be heap-allocated...
        well maybe not, since the tokens at least have valid storage until consumed, at which point they are replaced...
        we could make the token buffer a bit larger and shorten the max lookahead so that the tokens are at least valid until the *next* token is consumed
        but this is still a bit janky I think, since now we need to manually dereference the tokens in certain situations and AAHHHH
    but all this complication is just because we are trying to allow both the current model for lexing and the new pre-lexing model
    the pre-lexing thing also has an issue in that it requires the use of a dynamic array which will certainly realloc and waste a bunch of space either in our pool or in the temp allocator
    
    So it seems the best solution right now will just be to allocate space for the tokens we need at the time when we attach the token to the nodes
    
    
so we are gonna defer working on the pre-tokenization stuff for now and just store a single source token for the sake of the source location information.
   also, at that time it may not be a bad idea to also attach trivia from line comments to the preceding token if it is on the same line e.g.
   `x := 5; // some line comment` would have the comment attached to the semicolon (or declaration?) rather than token whatever comes next
   
get source location info on nodes and print that information in debug logging
    we need to just beef up logging anyhow, and maybe write some helper procs to capture the common logging patterns in each section of code

then finally, try to get trivia (whitespace/comments) working nicely in serialization. 
    unfortunately cannot do this all trivially since some nodes require more or variable numbers of tokens...

would be nice to not have to store allocator on script, but I don't want to make this change until I am confident 
    that all allocations the script does from some givne entry point can be done using a single allocator
    for now, it helps to know that we can have a context allocator and node/token allocator that are distinct
    in the future we should probbaly not have to do thigs this way as the need for multiple allocators seems 
        like a design flaw or at least indicative of things being overcomplicated


when we get back to developing that whole feature about identifier renaming, go back and make that work with declarations as well



## Improved Logging and Error Reporting

already beefed up logging in parsing step, now need to do typechecking and execution
    need to make sure we are properly getting source_token attached to nodes

we should make expect_token_type better and log an error message when type is not as expected

need to provide source file path when parsing from a source file, else provide source location of Jai file by default

we do now store source location info well and report it in most error logging statments
still probably want to do the thing with expect_token_type though, and also add jai location on manufactured nodes

### Result Objects

Result :: struct(T: Type) {
    success:    bool;
    union {
        error:  string;
        value:  T;
    }
}

would like to be able to append to error string, maybe
would also like to convert from one result type to another for errors, so we at least need some binary compatibility there

// only for errors, since we have binary compatibility in that case
recast_error :: (result: T/Result, to_type: R) -> Result(R) {
    assert(result.success == false);
    return .{ success = false, error = result.error };
}   

implementing usable result objects will require considerable rewriting, since it basically affects every single procedure call
maybe this will actaully just be too clunky to use, but will have to just try it and see

the main reason I want this is to be able to get better error reporting, or get error strings from evaluation procs
but maybe a better solution is just to set some global or context error string that the user can then manually check





## Identifier Renaming

need to make declarations use an identifier node instead of just a name string
(It's sort of silly that we don't just do this already, since the node is already allocated anyhow. I guess I was just trying to remove an extra indirection.)

## Macros and Malleable Literals

Not sure what to do here yet, will have to refer to my notes from above.

## Node Replacement

Node replacement is pretty doable now, not too much else to say. Things got nicer in that department once we stopped doing the linked-list thing for nodes and just used `[] Node` instead.
Maybe that change will come back to bite me in the future if I want to do more dynamically add nodes to blocks, but for now it seems like a good decision since it just reduces complexity.
I think that the one case where I was really considering doing somethign of the sort was also jsut a case where one could externally augment LS to acheive the same result
    e.g. maybe we have some user console with repl-y behavior that pushes all executed commands into a temporary buffer, then later allows the user to save that set of commands as a name block.


## Directives Improvements

We made major improvemts to directives recently, making them much more powerful and giving them a much more flexible interface.
The documentation is good enough in directives.jai that I would just point you to go read that, and maybe reference test.jai for an example usage.


## Ahead-of-Time Lexing 

Maybe come back to this later if we want to improve the trivia preservation further...

## Trivia Preservation and Serialization

Basic verison is working, but work more is needed in order to improve things.
Although, it's already pretty usable in the current state, so I may go ahead and start implement script modificaiton and re-serialization into my game engine.

Unable to save trivia on:
    before parenthesized expressions
    after final statement in block
    after final expression in struct literal
    after after type expression in declaration
    after final argument in procedure or directive call
    
    
in theory, we could save trivia before and after each node, then prevent appending pre-trivia if it is the same as last post-trivia
but that's an extra 16 bytes per node for the extra string

I think I may just acept the minor imperfections and live with it this way long-term
    because the alternative is probably doing the things where we keep all source tokens and map each node to a token range, then overwrite tokens for what has changed on AST
    and I dont' want to go down that rabbit hole right now...


## Clearing Up Allocator Usage

Maybe we don't need to store an allocator on the script and can instead just always use the pool / temp storage
I don't think the script ever allocates anything other than nodes and intermediate values
Need to check, but if this is actually the case then I can remove some logic around setting the script allocator, and just `push_allocator(get_pool_allocator(*script.pool));` at each main script entry point 

## Implementing #code and #insert operators as a test

Is there really a need for this? We can already do everything we would want to do with #code or #insert using directives...

## Compile-Time Operators

Or, operators as directives.
These operators will need to operate on a given node type rather than on a given value type.
For example, in GON, we will use this to implement custom identifier resolution for field references.
This will be done with special prefix operators that act on a Node_Identifier at typechecking time, replacing the identifier node with an Any literal.

In general for language design, it would probably not be a good idea to allow operator overload to create new compile-time operators, but for Lead Sheets, it makes sense, since this language is sort of meant more as a collection of simple utilities out of which to construct your own simple language.
So extending the language in this way seems very natural, in my opinion.
The approach will probably just be to extend the operator table to include directive-like callback functions for manipulating nodes in the same sort of way.

if an operator is compile-time, then it can't have multiple overloads based on type
    although, we could have differnt versions for prefix, postfix, binary

we will probably first want to refactor operators slightly to add postfix operators
    rename UNARY to PREFIX, then add POSTFIX as well

operator table defines what operators exist on a syntax level
    this is currently constant, but we could make this resizable so that we can expand the operator table at runtime
    

had some thought the other day about putting operators overloads in their own array, separate from procedure overloads... don't exactly remember that whole traing of thought











# A bunch of rambling

syntax suggestion:
    unary `?` as an insert operator for Code Nodes
    would also want a similar shorthand for basically #code
    or maybe prefix ? is the nodes of and postfix .? means insert the nodes
    could make macros much easier to write and use
    prefix double question mark means get tokens instead of get nodes
    can be inserted in the same manner
     



I've had enough drinks to make an out-there suggestion, but I was thinking the other day that it would be cool to have an operator with similar semantics to pointers for dealing with code nodes. So instead of 'address of' and 'dereference' operators you have 'nodes of' and 'insert' operators respectively. Say for instance we use prefix `&` for the former and postfix `.&` for the latter. 

Putting parameters on node insertion becomes very clean as well: 
    `code.?(scope=caller_code);`

maybe this operator could be used for macro calls as well since these insert code into the caller's scope
    `some_macro(..macro_arguments).?(..insert_parameters);`

If we just want to get Jai tokens, we can use this syntax:
    `??{ some 6 tokens , * % };


custom parsing with 'double question mark dot identifier' syntax
takes parameter list for called parse procedure and second parameter list for insert parameters

```
??.parse_proc(..proc_args)(..insert_parameters)
    ... arbitrary text here ...
```

```
parse_proc :: (file: string, args: ..Any) -> (to_insert: Code, remaining: string)
```

Code node can also represent some array of tokens, so that custom parse proc can return either Jai tokens to be inserted or actual AST nodes
parse proc actually cannot be a macro here, since we have no real AST yet in which the parse proc is being called,
this is a purely syntactic construct in which the parse_proc is called as soon as it is parsed, takes over parsing to generate a new stream of tokens or AST, and then returns control to the compiler

But maybe this is a less powerful version my directives in LS...?
    also since the file is not yet parsed, the parse proc obviously has to be resolved as some identifier in a different workspace.

Even if you don't like the idea of having custom parsing in principle, I think it is at least fair to say that this syntax makes it extremely clear when such custom parsing is occuring, so there should at leas tbe no confusion about what is going on
the only potential confusion would be telling where the custom parsing ends, but this should probably be quite apparent and with the help of editor tooling, syntax highlighting could make it even more obvious

If you editor supports using a dll or something in order to add syntax highlighting, it would be pretty trivial to write your custom parsing procs into a file that gets compiled to a dll for the editor to call
so that your custom parsing proc can also provide the syntax highlighting to the editor

It would honestly probably not be too hard to just add a compiler plugin that implements a demo of this custom parsing
it's really just essentially a pre-processing step
I assume we can just pre-proceess every new file that gets loaded/imported or added through an #insert
and while that's not ideal since it means probably a lot a duplicated memory, it could be worse
we just scan every file for `??.` and do the needful

now, of all the things I'm proposing here, this is the only one which would not actually require direct language support
since as mentioned, ir can really be done as a pre-processor step
It's essentailly syntax sugar for stuff that can already be done in the language (albeit much more verbosely)

but for things like actually passing tokens to macros, that would obviously require language-level support


other thoughts:
    remap identifiers exported form macro calls using the insert parameter list:
        `some_macro().?(new_name = old_name)`
        

    maybe there is some possible generalization on how for loops allow remapping control statements like break and continue
        the simple verison is probably just a token-level replacement
            which is essentially what the break/continue override is: a simple replacement of a token with a new AST expression
        the ultimate form of this would be a generalized AST find/replace mechanism
            how exactly to implement that recursive AST expression search is still a question
            and I will probably eventually work on that more with lead sheets
            because we need some syntax for capturing varibles in what the epression can be
            much like a regex for the AST
            I wonder if I still have my old notes on this somewhere...
            
        
        
I've had this idea floating around in my head for a while, but curious to see what others think. 
It's sort of related to the potential macro refactoring that iirc is still under consideration. 
Basically the idea is to make handling code nodes more intuitive with `#code` and `#insert` operators that are syntactically and semantically similar to `*` and `.*` respectively. 
Say just for example that we were to use `?` and `.?`. 
Then inserting a block of code with some parameters looks like `code.?(scope=caller_code);`, where the insert parameters are attached like so. 
I think this syntax could make it easier to add new parameters on code insertion over time. 

Maybe a trivial convenience, but using the `?` in place of `#code` could make calling many macros with Code arguments more compact:
```
elem := array_find_where(foo, ?{ it.bar == baz });
```

More importantly, this syntax could provide an intuitive way to attach a second parameter list to a macro call, which makes it clear that some code is being inserted at the callsite:
```
foo := some_macro().?;
```
This second parameter list could allow the user to do things like renaming backticked identifiers, 
which could potentially be useful in a case where one wants to call a macro which exports an identifier in this manner twice in the same block, 
but wants the exported identifier to be different in each case.
Contrived example incoming: 
```
declare_int :: (value: int) #expand {
    `number := value;
}

main :: () {
    declare_int(3).?(number=x);
    declare_int(5).?(number=y);
    print("%, %\n", x, y);  // "3, 5"
}
```

I think it would be quite in line with the language's design to make handling code as intuitive as dealing with pointers, and having some syntax similarity 
    
    
    
AST regex:
could use some such #code expression then provide a 'such that' clause defining constraints on variables
here, the arrow notation attached to the prior #code expression shows type constraints on the variables
this is sort of backwards to how a procedure declaration is structured I suppose
```
exprs := get_expressions_of_form(source_code, ?(x * y) -> (x: int, y: int, x != y));
```
This whole expression `?(x * y) -> (x: int, y: int, x != y)` would have to resolve to some structured type
so that the user can then manipulate it 


Iterating on the idea:
    make it polymorphic with some type restriction:
        `?(x * y) -> (x: $T, y: T, x != y, type_info(T).type == .INTEGER)`
    maybe we don't want to use declaraiton syntax here actually, only boolean expressions, so we reformulate as
        `?(x * y) -> (type_of(x) == type_of(y), x != y, type_info(T).type == .INTEGER)`
    since we are no longer using declaration syntax, maybe we can use dollar as a wildcard in this context
    or, we still allow declaration syntax but use it for the purpose of defining tokens that would otherwise be unparseable
    e.g., we want to replace `*` with a node representing an arbitrary operator
    well, nvm, I got a better idea there to just use the token 'operator' sine its already a keyword
        `?(x operator y) -> (type_of(x) == type_of(y), x != y, type_info(T).type == .INTEGER)`
    but maybe there's another example where we could use $ as some wildcard? maybe meaning wildard value?
    I think any identifiers used in some search expression should have a declared value type
        but then also, there's some question about whether the type in the decl should represent the type which the statement evaluates to, or the type of the statement itself
            sort of an lvalue vs ralue thing, but maybe not really
            what I mean is, say for example we have some procedure `(int) -> int`
            well then is the type that we consider the expression as `(int) -> int` or just `int`?
            in the context of looking for some expression like the above, we probably want to consider it just `int`
            because what we mean by the above search expression is to find all instances of operators where the operands are both integers
            but then how would we be able to detect the other case?
            an attempt: `?( (($) -> x) * y ) -> (...)`
                this doe snot work because x here is presumably then a type expression
            here we use that wildcard to represent that parameters to proc can be anything
            then we catch x as the return type, so that it can be checked against the same constraints
            so this search expression would find any case where a procedure returns an integer and  the result by another integer
            
            crap -->   (x: T = ($) -> )  i dunno anymore

the difficulty is that we want to be able to bind certain subexpressions to variables, but those subexpressions may themselves have their own variables which we want to pull out

`?(x * y) -> (x: ($T) -> int), y: int)`
    this search would yeild the type info for T in addition to providing the values for x and y


```
Code_Search_Expression :: struct {
    expression_form: *Code_Node;
    constraints: [] Expression_Constraint;
}
Expression_Constraint :: struct {
    constraint_type: enum { SUBEXPRESSION_TYPE; 
    ... more stuff to figure out later
}
```

will need some Code_Wildcard type to represent arbitrary subexpressions in search expression



go back to the examples where i was trying to insert an Any returned tby a #run as a statically typed variables
I feel like this is something I should be abel to do but I remember it being nearly impossible to do tersely
and there was also some related issue there with pointers not being able to have a null value i think


for typechecking, we want to be able to return an error enum to denote the reason for failure, which should be helpful in certain cases which are currently somewhat ambiguous
we also want to be able to return an error for the sake of user callbacks

multiple returns is nicer from user side though, so we will just adapt with wrapper procs

simplify error messages, don't format with location in error string, just attach location to Error object.
if user then prints error later, then format location with error message



## Removing dependency on dyncall

Using dyncall in lead sheets is now optional, and there are both benefits and drawbacks that the user should be aware of when choosing whether or not to use dyncall.
By not using dyncall, you lose a little bit of functionality / flexibility, but gain the ability to compile to targets that dyncall does not support.

Calling Jai procedures with dyncall currently only works with the LLVM backend 
    this is because the calling convention for Jai is not precisely defined, and so it really only works as a sort of hack
    Hopefully when Jai gets an official release, it will also have a well-defined calling convention so that I can properly support it going forward.

dyncall does not work at compile-time
    There may be some way around this if I could only compile a dyncall dll, but I haven't put in the time to figure that out quite yet.
    Now of course, there's probably no reasonable use case for running dyncall at compile-time, but I still feel this is a restriction worth mentioning.
    
dyncall does not work when compiling to web assembly
    If you want to target wasm, then you will have to disable use of dyncall.
    There are also other platforms that dyncall does not support, which you can just check on their website.
    It *could* be possible to simulate dyncall-like functionality on the web by doing something crazy like generating JavaScript that calls back into Jai,
    but that is not very practical nor is it something I want to figure out at the moment.
    
without dyncall, function pointers are mostly unusable inside of scripts
    The user does have the option to manually register proceudre types so that wrappers can be generated,
    however this obviously requires that the user know the types of the function pointers that may be used in a script ahead of time.
    
without dyncall, the wrapper procedures which are generated to marshal arguments and return values
    will probably add a littel bit of code bloat, though it should not be too severe.



TODO:
    replace #procedure_of_call with #bake_arguments, add type as separate parameter like before
    check what the function signature looks like after baking
        maybe we can remove the need for the cast when storing proc ptr to procedure_wrappers?
    
    make c call procedures work 
    
    in dyncall
        replace MAKE_C_CALL with something more like call_procedure
            try to make it so that we can separately bake procedure type and specific procedure pointer
        



### Dynamic Members / Namespacing

Got this new idea, not sure how best to make it work though.
The basic idea is that we can use some special syntax to access dynamically-added members on some identifiers.

For example:
```
entity->cycle_offset = 0.3;
```

cycle offset is not a real member of the entity struct, but we can treat it like a sort of virtual member that is stored like any other script variable
so we are basically just using the entity as a sort of namespace to access what is really an internal script variable

the big issue I have right off the bat is that we can't really just bind this to an identifier itself, we need to bind the member to some other particualr variable
e.g., `it` is an identifier in a for loop that will refer to several different instances of the same type
    and we don't want the virtual member to be bound to `it`, but to each individual instances that `it` points to
    but unless the bounds of iteration are known ahead of time, 

alternatively, we can just make the virtual member lookup and a completely dytnamic operation
    i.e. the user needs ot implement some virtual member lookup procedure that returns an any for the value
    
    In this scheme, above example would just desugar to something like
    `virtual_member_lookup(entity, "cycle_lookup")`
    
    I don't like this because it pushes typechecking to execution-time
    and this isnt just trivially slower due to the additional typechecking, it would mean that we would need to type hint every single spot where virtual members are used as a rvalue
    
ideally, we can find a way to make virtual members statically typed, apply to specific instances rather than types, and 
    will require that we in some way declare the members on each instance before using them
    coudl use regular declaration syntax for this, but the semantics are quite different, since we 
    

Any procedures concerining virtual members should be function pointers that we can override, just like the parsing procedures.
```
add_virtual_member :: (owner: Any, name: string, type: *Type_Info) -> ok: bool, value: Any {
    // how to add virtual members will actually depend very much on the data type we are dealing with, probably
    // because in many cases we cannot assume that pointers will be stable across different executions of the script
    // so we need some user-level owner resolution
    // but really, all we will ever be able to give to the user here is the Any, since this is particular to some instance variable, not to an identifier or some node
    // (we can't use an external variable node due to the iterator problem)
    
    so the user will just have to use the pointer + type as a handle, and perform any of their own validation as necessary...
    
    we will probably want to first do some internal checks to see if the virtual member is owned by anything within the script itself, then hand it off to the user afterwards
    
    
}
get_virtual_member :: (owner: Any, name: string, type: *Type_Info) -> ok: bool, value: Any {
    ok, value := internal_get_virtual_member(owner, name);
    ok ||= value.type == type
    return ok, value;
}
```

Doing this in a statically typed way should actually be relatively simple now that I think about it.
There's just an added wrinkle to the semantics of declarations now such that we may change the value pointer each time we execute the declaration (if the declaration is for a virtual member).
This will mean that we can just resolve the usage sites as per usual, no changes necessary.
And we will only need to do any kind of type assertion once, at the declaration site.

Because we need to declare virtual members before we can use them in any case, this does mean that we will have to do some extra work to declare virtual members even if we don't set their values.
    Will require either adding `---` keyword to mean non-initialized in declaration or just not zero-ing virtual members in by default.


Another thing to consider about virtual members is that we want them to be easy to look up or access from outside the script
that way we can display them inside things like imgui menus, e.g. 'Entity Details' panel in my game



I am now noticing that we have a major problem with my plan
And that problem is having the ability to support any kind of complex expression on the left side of an arrow
because we are essentially just using the left side as a namespace for the declaration,
we cannot compare any complex expressions or use those expressions as a namespace

we could go to an almost totally dynamic way of dealing with virtual members, but that has a lot of problems, of course
    (because then we're just back in the dynamic typing boat)

we *could* to the dynamic version and then just use casts as a sort of type assertion
or insert auto casts in 

if we go the totally dynamic route, then we don't need to change how declarations work
we would only have to change assignments or add some implicit


OK, I think for now what I will do is just implement the more constrained version with static typing, where we only allow simple identifiers on the left side of an arrow

we could also potentially do something like, the virtual members do get associated with a type, and must be declared for that type, but they can be null on any individual instance of that type
and if they are null that's just a runtime error

that's probably the way to go...

```
// declare virtual member for a type
Entity->cycle_time:     float;
Entity->cycle_lerp:     float;

Entity_Group->tempo:    float;

// using virtual members
for entity_group("orbiters") {
    entity->cycle_lerp = cycle_over(time * orbiters->tempo, entity->cycle_time);
    set_next_offset(entity, circle(cycle_lerp));
}
```

if we declare virtual members as being owned by some type rather than by some identifier
then we get the benefit of being able to access virtual members through arbitrarily complex expressions
which would be very helpful if we have, for example, some kind of lookup procedures that return some entity/entity group
on the downside, we cannot use the same virtual member identiifers for different data types on differnet instances of the same owning type
for example, we could not have one entity that uses 'range' to identify a virtual float member while another uses it for a vector2
    but maybe his limitation is not that big of a deal...

also, perhaps we should allow declaring a virtual member using an instance of some type on the left side
this could be semantically the exact same as the usual case where the left side is a type, but we just get the type for the user implicitly
    this could be convenient since we don't have anything like type_of() in LS
        not that this would be hard to add though

NOTE: 
    as an aside, perhaps we should sort all entity groups each frame, getting a temp array of the members
    and pass the entity groups as external variables to the levle script
    so then you can just iterate orbiters.members
    and can attach virtual members to the groups themselves
        


The first, minimal implementation of virtual members seems to be kinda working now
but there is a lot of cleanup that will be needed in order to make things nice again

firstly, we may actually want to consider making the arrow its own node type, even it is lexically similar to the dot

secondly, we should make resolving declarations a lot cleaner
    instead of using two separate procedures for regular and virtual declarations, 
        we should just pass the resolved node for the virtual member owner

thirdly, we should clarify what parts of an arrow node get flagged as typechecked and under what circumstances



declaration
    left is either:
        identifier
            name (acts as declaration nameb)
        arrow
            left identifier  
                (acts as a sort of namespace for the virtual member, matters what we resolved to)
                    we need to be able to compare two identifiers to see if they resolve to the same underlying thing
            right identifier
                name (acts as declaration name, sorta)


TODO: need to introduce some procedure to check if an identifier can be used as a namespace
      we don't want the user to be able to add virtual members on top of other virtaul members for th time being. that sounds like poopoo doodoo (as is the technical term)


TODO: we should note somewhere in the code that we leave identifiers in the unresolved state when they are the terminal/primary identifier for some declaration


implementing virtual member declarations on a per-type basis rather than a per-identifier basis

restrict the types of identifier which can be used as a declaration namespace to only type for the time being
when a virtual member is created on a type, we need to somehow attach that information to the type registered on the script
    so External_Type may need to become a new struct type like External_Procedure or External_Variable

remove the logic to execute virtual member declarations, since virtual member declarations on types would be a purely compile-time thing
    unless we somehow set a default value for virtual members that get registered on types, then provide that virtual member default value when we call get_virtual_member




Syntax possibilities

declaring virtual members for a type, using some keyword or directive
```
#attach(Entity, {
    cycle_lerp: float;
    cycle_time: float = 5;
    range := Vec2f.{ 1, 1 };
});

@Entity {
    cycle_lerp: float;
    cycle_time: float = 5;
    range := Vec2f.{ 1, 1 };
};
```

accessing virtual members totally dynamically, with type assertion on accessor expression
```
// right side of arrow is declaration expression inside of parens
entity -> (cycle_lerp: float);

// if we had proper macro stamping, we could do something like this
// which would then allow us to use cycle lerp to refer to this virtual member in a 'statically typed' way
cycle_lerp :: entity -> (cycle_lerp: float);
```

If we go the route of needing to declare virtual members on a type, 
    it should be noted that each instance may not actually contain all of the declared virtual members
    or, the instance may also have virtual members that are not declared on the type


type cast after totally dynamic virtual member access:
```
cycle_time := entity->cycle_time.(float);
```
imporantly, for the above case we would need to make sure that the arrow has higher precedence than the dot cast
    I don't think we actually have a systematic way to handle precedence for the builtin binary-operator-esque nodes like dot and cast
    it wasn't an issue before, but now it would be
        so we need some get_precedence proc that takes any node type and returns some precedence value
        on second thought, I think this should actually already work as needed, so maybe this is a non-issue


ok, new idea
what if we just try to have out cake and eat it too
```
// get virtual member directly, as an Any
// NOTE: assignment always gets right type!
entity->range = 5;  

// when used as rvalue, we need either a type cast to explicitly assert the type, or we will do an implicit conversion to the hint type
// if we cannot get a hint type, we can't compile the virtual member access expression
// my hunch is that we may not actually need to explicit cast all too often
entity.position = circle(time, entity->range.(float));


// options number two, secondary syntax
// if right side of -> is a parenthesis, then we parse a declartion inside the parens
entity->(cycle_time: float);

// this above expression also just returns the value of cycle lerp, 
//   but if cycle lerp does not exist on entity yet, then it will be declared with the default value of float (which is zero)
//   it also inserts the proper type assertion, so this expression could be used as follows
cycle_time := entity->(cycle_time: float = 5);

// even though we're accessing a virtual member in the above statment, the additional type assertion means we can do type inference for the internal variable on the left side
// the major downside of this syntax is that it's a lot 
// but we do need to have the semantics to properly get the virtual member, cast the type, and allocate the thing if need be
// and this just takes a bit of typing
// so, a way to make this a bit nicer may be to be able to bind the virtual member declaration to a shorter name
// and hey, it just so happens we already kind of have a mechanism for that (though an incomplete one)
// we can use a static assignment like we use for malleable literals
cycle_time :: entity->(cycle_time: float = 5);

// now when we use `cycle_time`, we will instead be essentially pasting that whole virtaul member access expression (with type assertion and default value) at the site where we use `cyce_time`

```


NEW PLAN TODO: 
    support type assertion on casting an Any to some static type
        could still just use Convert.any_to_any, but we should probably disable all remappings beyond simple numeric types in the script's conversion context
        as opposed to Jai, we will have implicit casting from Any to other types as well as in the other direction
        maybe this will turn out to be problematic or maybe it'll be useful, i dunno yet!
    while we're at it, also add support for coercing any type to an Any in assignments and in procedure arguments
        the procedure argument case may actually be more trivial
            in the case of a general assignment/declaration to Any, we actually need to think about the storage duration of the rvalue
            since the rvalue may be some expression that produces an intermediate vlaue that just lives on the stack
            so this is a more complex case...
    change the semantics of get_virtual_member
        allow providing a default value


We could potentially do something really crazy where we allow the use of the arrow operator to create a sort of code block that is associated to a particular declaration/identifier/value whatever
And then we can evaluate any arbitrary expressions or statements within this context and return those results back into the global script context

then the idea of virtual members emerges for free as a sort of natural consequence
this contextual block can override how identifier resolution occurs and how declaration values get stored
for identifier resolution, we could basically 'using' the context value so that if, for example, the context value is a struct, we get access to the members directly as if they were declared like external variables
    could do this very simply through a get_context_identifier callback on script. context scope could be between local block scope and 
secondly, the declarations within a contextual block would naturally get duplicated
    (we would have to duplicate and specialize the block at runtime, since we may 'call' the block with statically unknown contexts)
        still though, once the block has been specialized for a particular context, we can execute it as per usual



Problems with doing contextual blocks:
    primary issue is that we may have multiple contextual blocks that use the same context
    this means that we can't use the context item itself as a namespace to access items within any particular contextual block
    since we can't know for sure which one you're intending to reference
    
    not a problem: polymorphism, macro stamping 
        I was previously thinking we would need to do some sort of AST-copying macro business for these kind of blocks since we are specializing the block for  a particular context
        but this is not necessarily the case, actually, since we can probably just make the assumption that context variable will always be of the same type for a given block
        this is because we don't have any kind of polymorphism in our for loops (yet?) and no other way to call blocks with varying types of parameters
        
        so instead of having to copy and specialize entire chunks of AST, we can just have some context structure the holds the static declarations for the block, which we retrieve via some callback for an id from the script owner
        because this mechanism is inherently less concerned with the particular type being used as context, we can probably just implement this all without the need for user callbacks and management
        and we can prevent the need for runtime errors if some item is used for context and no context has yet been created. we just make a new one with all necessary declarations, and set the up withi their correct intial values
        
    One option we may have afforded by not doing the AST stamping thing, is to simply expose only static `::` declarations as accessible across multiple blocks with the same context
    or we could just go so far as to assert that all contextual blocks with the same context item share the same scope, but that seems problematic for a variety of reasons
    one potential problem with this idea though, is that we will either have to restrict these kinds of static declarations to the root block of a contextual block for scoping reasons, or we will have to implement some kind of hoisting to push the declarations into the root contextual blocks' scope.
    
    not needing to do anything polymorphic actually does have me considering the possibility of bringing back in the idea of declaring virtual/static members on each type
    then these and only these values can be resolved globally within any contextual blocks using that type
    so they are properly enumerated at compile-time and can be used throughout the script
    the downside of this is that we would then actually allocate space for all of these virtaul members in every single instance of the type
        although maybe we could finagle this and only allocate for these declarations on the first time they are used in each instance (but then maybe that's bad data locality? not like that matters much in a scripting language tbh)
    

when we are inside a contextual block, declarations and identifier resolution change a bit
static declarations's values are scoped to the nearest block context

the root block of our script will now also need to have some block context, and any static declarations will get hoisted up into that block context
that does not mean the declarations will be accessible outside of their local scope, just that their storage duration is defined by the block context

any block that contains a block context now essentially becomes an execution context that we can overload
so we could, for instance simply create a new block context entry for an existing script's root block and then execute the same script with different static values
or we could save just the block context for a script and that allow us to restore all modified static declaration values
    this could even be used as an alternative to entirely re-serializing scripts, and would probably be better for saving persistent state that's not lexically explicit



```
Block_Context :: struct {
    context_for:            *Node_Block;
    context_expression:     *Node;                      // maybe this is null for named blocks and root block
    current_instance:       *Block_Context.Instance;    // only presumed valid during single execution of the block
    static_declarations:    [] *Node_Declaration;       // maybe just hold all declarations? worth considering... in which case we should probably move to Node_Block
    
    Instance :: struct {
        instance_of:                *Block_Context;
        static_declaration_values:  *void;              // pointer to single allocation for all static declarations. we will offset by offset value defined in the declaration node
    }
}

// allow user to reject unsupported types during typechecking, rather then getting a runtime error
can_type_be_used_as_block_context_instance :: (script: *Script, type: *Type_Info) -> bool;

get_block_context_instance :: (script: *Script, block_context: *Block_Context) -> *Block_Context.Instance;

```

how to resolve context element ids? something akin to get_virtual_member should work for user ids
if we allow the context expression to be arbitrary, then the only thing we have to go off is a type and value pointer
    this is problematic for being able to handle it all internally because we never assume value pointers are some stable identifier of anything
if we onyl allow the context expression to be an identifer, then at least we can internally link back to some declaration
    but this does not help any because we have the iterator problem
so we may as well let the expresison be arbitrary
so now our only recourse is to request the user creates some Block_Context.Instance (matching a particular Block_Context) and return us that context entry pointer


Actually, we could manage block context internally for named blocks and the global block
    then the question is whether to (and how to) support overloaded entries for these block contexts


I am starting to think it's just fine if we can't really communicate too much between different contextual blocks
or to be able to attach declarations to entities per se
we could still do some kind of thing with static declarations in context blocks where we sort of aggregate all the context blocks that have a particular value type in the context expression
and then make sure that across all those blocks, we don't have any redeclarations of static decls
if we were going to do this though, we would really want to hoist the static declarations so that they can just be referred to form anywhere
    and we would have to evaluate their init expressions in some deterministic order, probably during typechekcing pass
    idk, it just sounds too complicated to do this, since we can't really close a block context when the block ends, and that sounds like a problem
OR we can have all the globally accessible statics (virtual members) be defined ahead of time as I've played with the idea of for a while now
so we still get block-scoped statics per instance, and we get globally scoped statics per instance
    I think I just like the idea of having the globally scoped statics (virtual members) in theory, but they may not really be all that useful in practice.

in any case, implementing the block context stuff is probably more of a feature win overall, since that will also solve my problem of preserving static declarations
    and potentially frees me up from needing to worry about using re-serialization of scripts as the primary way of saving modifications to those scripts
    
    we will still need to be able to reserialize back to source text if we want to support things like changing identifiers dynamically, though
    




## Static Assignments / Macros

I need to do some thinking about how I want to fully implement the `::` operator since at the moment it is restricted to literally only being used for malleable literals
There are a few very similar, but importantly distinct things I would like to be bale to express using as little syntax as possible on declarations

The current semantics of `::` are simply to make the declaration value pointer point directly to the malleable literal on the RHS of the assignment statement.
What this affords the user is the ability to modify the value of the literal on the RHS in multiple places by using the identifier of the declaration.
A side effect of these semantics is that the init expression of the declaration only gets run once, at compile-time, and then never againduring routine execution.

but we would like to separately be able to do a sort of #once on the evaluation of a declaration's init expression
and separately, to do the sort of direct binding of the value pointer
    (and the direct value pointer binding was really just a shortcut for properly duplicating/referencing the literal node)

the other dimension on which we may want to disambiguate is in what exactly a macro assignment entails
in theory this would mean just copying the nodes of the RHS and pasting them at each place where the identifier is used

Aside:
    I think probably have a sort of #once assignment will be more easy-to-use and useful in many cases than requiring the use of an init block in scripts
        but probbaly only if we have the contextual block stuff


`#once` covers the case that we only want to evaluate an expression a single time, and then that value essentially becomes static
        this prevents the need to do a lot of setup tasks in the init block, and is something we could potentially override or reset in the editor

`::`    the static assignment operator covers the very specific case that we want to bind the value pointer of the RHS to the LHS
        this really only makes sense when the RHS is a malleable literal, hence why that is currently the only suported use case
        we could potentially use the same syntax for some kind of macro thing later on, but its an open question how useful that is
        and then maybe the ? syntax only mean something like 'this literal keeps the same underlying value pointer when its node gets copied'
            which maybe is a weird thing for the question mark to mean, so perhaps at that point we should consider changing it
            and also changing the flag MALLEABLE to be something like, SINGLE_UNDERLYING_VALUE
                (this way of handling malleable literals will require allocating some separate value even for small types, though)
                (and it won't play as nicely with type inference or implicit casts/coercion)


## Error-Tolerant Execution and Evaluation

TODO: we should add a mechanism to evaluate a script in a such a way that it is tolerant to errors
      this could be useful when identifiers may change or become invalidated, such as when using the scripts in the context of a level editor




## Expanded Procedure_Info Structures

I now have some basic stuff in place to get info about procedure arguments which we could integrate with external procedures.
    - still need to collect info about varargs parameters, I overlooked that before

However there are some other things that need to be put in place first:
    - Parse struct contents with comma-delimited `name = value` syntax
        - same procedure will apply for parsing named procedure argument expressions
            - slightly different since we can begin with unnamed arguments, then begin using named arguments (cannot do this in structs)
    - typechecking changes (duh)
        - will complicate process of matching arguments and overload resolution
            - how to rate conflicting overloads where one has some default argument provided implicitly and one does not?
    - execution changes
        - how to denote that some argument is filled by default value, which can be non-constant (e.g. context values)?
            - do we need a Node_Argument can either point to another node or be null to indicate default value should be used?





## Fixing some things with malleable literals

We should check that the literal expression being used as a malleable literal is actually constant
    since we can have struct literals that are not constants


## Improving Handling of Any in Scripts

need to be able to push an Any or pop an Any from the stack

when casting to an Any, need to make sure intermediate value will stay valid
    should be fine for larger values, but will be a problem for smaller values, since those are always on the stack
    so we will need to catch the case where a small value is assigned to an Any and stash those values in some small value storage, elsewhere
when casting an Any to something else, need to make sure we can still use that as an lvalue
    e.g., the cast node should be a valid lvalue when the value expression's value type is Any

we need to overload the assignment operator for Any to Any case
    needs to perform a check to assert that right type is coercible to left
    if not, we get a runtime error
    
for unary operators, we could dynamically typecheck and resolve the operator to use, then apply it
for binary operators, 

Yet another choice to make:
    either we can allow assignment to change the type of a virtual member, 
    or we can assert that the right type coerces to what the virtual member type currently is
        then we can only change the virtual member's type by redeclaring it
        this seems like the way to go...

now that I think about it, for something to be a valid lvalue essentially just means that the address of the value is the same as some declaration's value pointer
so if in any situation we copy some value (and thus change the memory location being passed down the tree) then the value is no longer a valid lvalue


If we have some value of type Any that we want to pass around within a script
or for, example we have somethign declared as type Any
then when we pass that thing up, do we need to have an Any that points to another Any?
that's not really ideal!

that extra indirection is sort of problematic and since we can't really make an any of an any without really forcing it

any_any := Any.{ type = xx Any, value = *any };

i mean, I suppose we could do this, but maybe while we're at it we should just make our own result type again
one that is essentailly just an Any, but with some kind of small value optimization, like Any_Number

Then we can just turn our stack into a `[] Value` or something

Maybe having this standardized value type thing will also make it easier to swap between execution and evaluation?
    could we use eval logic for handling Any's in execute?
        probably not, thats not how evaluate works
        we actually typecheck fully before evaluating, so uh, yeah..


Consider creating some conditional assignment operator like `?=` for assigning to / declaring  an Any
    main reason for such a thing would be to make it easier to use some arbitrary expression on LHS, such as virtual members
    and, so that we can declare something with the implicit understanding that its type could change later
    if we did this, would need to come up with some other way of marking malleable literals



## Implementing builtin casts

There's no reason we should be using some ridiculous dynamic cast for all casts in lead sheets
we should at least have builtin casts for the numeric types, I think
This is not a major concern at the moment, but it something we probabyl should take care of soner or later
The only benefit of staying with the current way of doing casts is that we can catch and report failed casts as runtime errors, and choose to ignore them.



## Things we need to be more rigorous about

passing check flags up the AST

checking for whether things are valid lvalues
    maybe we should move this back into individual typechecking cases

preventing re-typechecking on nodes where doing so is problematic
    need to create a thorough list of cases where re-typechecking is ok and those where it is not
    maybe we should refactor things so that it is always ok to re-typecheck?

allowing for node replacement
    we really should have some systematic way to replace a node by returning some replacement form typecheck_node


## Bugs and minor fixes

we should really stamp serial numbers onto all nodes like the Jai compiler does
    since if we decide to do stuff liek storing block contexts or preserving malleable literals in some nontextual way, we need to be able to patch scripts reliably
    and the only way to do this would be to keep serial numbers for the nodes we modified so that we have a real 
    actually even just having a serial number wouldn't solve it, but maybe it would help


prevent user from adding operator overload for `=`, probably



## Simplifying Identifiers


TODO: strongly consider offloading complexity of identifier node onto declaration
      and make declaration nodes for all external variables, procedures, and types
      this alone might make it much more feasible to start thinking about really implementing some of my other crazy ideas
      

One reason I cited in the my previous musings about why I did the current thing, is that we may want to allocate nodes wtih temp if we are doing a really simple eval
but TBH I don't think this is a valid reason anymore
there's just no use case for allocating nodes with temp any more than there's really a use case for allocting temporary intermediate values in evaluate_node with temp

Maybe the most real, practical concern is for resolving to literals and struct members
These are not something for which we could even realistically create a declaration node


ok wait, so what's the benefit of having declarations for external variables and procedures?
i dont remember what my train of thought was for why to do this, tbh...

it doesnt solve anything wrt virtual members
it only slightly simplifies the procedure for identifier resolution
it slightly complicates the process of updating the value pointers for external variables and procedures
and it uses a bit more memory for the declaration nodes
it only slightly reduces the complexity of getting an identifier's name / backing value

## Unifying Execute and Evaluate

this distinction makes it harder to maintian the codebase
also, the real primary difference is only in how results get passed back up
in either case, we are trypechecking fully before doing any execution / evaluation
so it's really just a difference of 
    in eval: using temp storage and returning values as Anys
    in exec: using a stack and getting return values by popping from the stack

I suppose the original purpose of the evaluation stuff was to prevent the need to use the stack, but like
    we already have the pool allocated for the ast nodes, so having some small stack for intermediate values is not any extra cost
  
so either eval needs to justify its existince by being more fully dynamic and doing *more* dynamic typechecking
or we need to just marry the two in whatever way proves best

TODO: check where all that we use dynamic new in both exec and eval
    think about how that plays with current state of hint storage

we can probably make our current eval stuff just use the stack (or just use its own temp storage)
    and then use that for everything we currently use execute for

then we can use execution to refer to running a script as bytecode


Firstly, we should simplify the stack operations and make a stack.jai
then we just init the stack allocator with memoery allocated by some other allocator

when we go to set up an execution / evaluation context, we provide a stack 
    maybe later this idea integrates with the idea of block contexts


evaluate.jai has been removed

Now that it's gone there's still a few loose ends to tie up
firstly, we will probably bring it back, in a sense, because we will just rename all the execute stuff to evaluate, since that's kinda more accurate for an AST-based execution model
then we cna use the execute name when it comes time to do bytecode

secondly, I would like to fix up the interface and error handling of execute_node so that it is as easy to use and extend as evaluate_node was
improvements to make:
    execute_node could return a `*void` or `Any` as a second return value
        this will have to be a peeked value and not a popped value, which segues into the next thing...
    smoother handling for common stack pushing/popping patterns
        in trivial cases, we jsut push_result and return true
        in larger cases, we really just want to have the stack to play around with during the call to execute_node, and then manually reset it to where it should be so that it's correct for the caller
            what this looks like is basically just calling get_result_storage at the top, doing what we need to do with whatever gets pushed to the stack,
            and then resetting the stack pointer back to where it should be before we leave
            we basically just want to use it like we would use temp storage if we were setting the watermark on entry and resetting it on exit
            this is probably the endgame pattern to use, since it means we will almost never have to worry about manually popping many small values like we do, for instance, in executing a node_procedure_call
            all that matters is where we leave the stack.top when we leave, so what happens in the middle is up to us


### Fixing Stack Operations

Before I can really reconcile all the differences between exec and eval, I reall yneed to have a rigorous 
    model for how different values are stored, and how memory is allocated within scripts
For instance, in eval, we just always allocate for the result value unless we have some hint storage
    (I think we are even doing this when the returned value is some small literal, which is really dumb since those backing values are stored on the literal node itself)

And while we are improving our allocation strategy, we may as well refactor non-static declarations to use some stack offset rather than a value pointer
    this will make it possible for use to theoretically implement functions or at least callable blocks in the future

NOTE: one nice part about having declarations that have their own separately allocated values is that we can validly read these declarations values and display them in the ui even while the script is not running
      but maybe its fine that we won't be able to do this for normal declarations anymore, since we can't modify those values anyhow
      and we can still show and modify static declarations in any case

It's not exaclty rigorous, but we could potentially just give eval a different temp allocator that uses our pre-allocated stack space as the backing storage
    and then we can simply reset the watermark after each statement that executes
    
    It will be better if we just make our stack structure reliable and use that instead of using an allocator
    the allocator method may be faster to implement, but it won't work well (if at all) for bytecode execution 
    


Stack Improvements Part 2:
    Make sure that Stack base is aligned to 8 bytes (probably already should be)
    -- push bytes for all declarations in scope at beginning of scope
    -- non-static declarations get some stack offset relative to stack base pointer for parent Node_Block
    -- When declaration executes, we return either value_pointer (static decl) or scope.stack_pointer + stack_offset

For any node whose value type is not <= 8 bytes, and where there is no hint storage, we need to also allocate stack space for those that will get pushed on scope entry, just like for non-static declarations

to summarize:
    any large values (declarations or temporaries) get put into the 'stack frame' allocation, and then word-sized allocations just get pushed and popped from the stack freely on top of that
    maybe this is a bit non-standard, but I think this model should work well for LS

in theory, we could probably just push and pop even larger temporaries on the stack freely like we will do for small values
    however, we then need to make it a special case when we coerce to an Any
        actually, this will need to be a special case anyhow, since we will need persistent storage in any case, even for small intermediates


About Stack Frames:
    Currently, each stack frame is associated with a named block, or with the root block of the script.
    Until we introduce some way to call blocks from within scripts (and maybe pass parameters to those blocks, since we may as well just support function declarations at that point),
        stack frames don't make a whole lot of practical difference to how we were allocating things before (using Dynamic_New with th script's pool allocator).

Handling temporary values:
    At current, we are allocating space in the base stack frame for all temporaries that are aggregates.
    This is bad, and really these things don't even need to be pre-allocated when we can just push and pop them like anything else.
    However, the main issue with doing so is that we can't peek a value on the top of the stack unless we know the size/type.
        Which, maybe is not a problem at all, but it's something we have to be aware of now.
    It also ---could--- would complicate the logic for builtin operations if we now no longer are guaranteed that all stack elements are the same size
        So, we need to investigate this before making the change.


For what nodes can we hint storage?
    members of struct literals
    rhs of assignment, declaration


Stack improvements part 3
    add ability to push aggregates to stack by value
        what changes are required for builtin operations?
        
    reducing allocations in stack frame for temporary values
        still require space in stack frame when we coerce to an Any in an assignment or declaration


Summary of some of the things I did:

The stack structure is just a lot more robust now, with a pretty good interface for doing all the things one may want to do.
Values of any size can now be pushed to or popped from the stack, with the small caveat that all pushes and pops are aligned to 8 bytes.
In theory, we could even return things like strings on the stack, since we basically alloca any intermediate values we need space for.
I probably should put some upper bound on the things we will actually push to the stack, though. (But that shall be a problem for another day.)

In any case, we now also probably use a lot less stack space for intermediate values anyhow, since I finally implemented a sort of hint_storage equivalent for execute_node.
Now, in execute_node there is a provided_storage parameter that must be used in place of a stack push if it is non-null.
This means that a lot of nodes which previously would've had their intermediate values allocated up front or placed on the stack, they will now just write directly to the proper destination.


## IMGUI menu for showing AST node structure

this would actually be pretty easy to do, and would allow for messing with the AST a bit more directly, which could be useful in some circumstances
could be very useful for logging/debugging if we could have the debug printing and error message be appended directly to each node


## Running Scripts in Separate Stack Frame

For as long as we are running scripts by walking the AST, we will probably need some kind of coroutine thing so that we can yeild after executing each node
We can probably use the osor_coroutine module for this.


## random thoughts

smarter iterators that can use offset semantics similar to pointers?
    or some other way to say `previous` and `next` in a for loop
    this is a very low priority thing, but could be a fun idea to try out
    maybe best to first try implementing it with a directive, since I'd like to have more things to show off what those can do

## Debug things

procedure calls
    try_calling_procedure_with_wrapper
        make `_checked` and `_unchecked` variants
        pick which to use based on whether debug flag is set
    do_dyncall
        also use unchecked variant for this when not in debug mode
        

## TODO List

allow subscript operator on non-lvalues, similar to what we did for dot member access on structs
split up parsing of particular constructs more, so that user can more easily duplicate or leave out parts in my_parse_x procs
extended procedure info
    default arguments
    passing arguments by name
    varargs arguments?

make renaming enemies in the game work properly
once the referent is fixed for some declaration (e.g. to an external variable)
we should have an unbreakable link there
and the only other complication is that we should check that identifiers will not conflict before saving the script back
but as of right now changing an entity's name does not work, because we are using the name to identify the external variable that the entity is linked to
and we really should be using some index to the list of variable, which probably ought to be stored on the entity itself
    what if entity is deleted?
    what if entity name is empty string?
        this is only a problem when we go to serialize. and we could just notify the user of the error when they go to save the level/script

    we should implement user error types for incorrect use of external variables, procedures, and declarations
    That way we can prevent the running of the script while such errors remain unresolved

create some better flags, system for specifying how node's value is stored
    for example, for loop declarations should not need to be marked as a macro in order that caller knows to use value_pointer rather than stack offset

when declaration is used as an lvalue, we should flag it as non-constant
