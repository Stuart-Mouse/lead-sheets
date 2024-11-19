
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
    

