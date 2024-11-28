
# Lead Sheets

A very simple scripting language for Jai.


## Overview

This scripting language is solely intended for use within Jai, as it leans heavily on the language's types and type info.
Like their musical namesake, lead sheets are intended to be a very simple way of communicating essential high-level information, while leaving certain details up to interpretation.
I plan to introduce some sort of metaprogramming facilities that will facilitate analysis of the expressions used in a script, allowing the main program to "fill-in-the-gaps" or stylize a lead sheet in the same way a musician would do. 

Though simple, these scripts aim to be extremely powerful, expecially when combined with immediate-mode UIs.


### Syntax and Semantics

The syntax is basically identical to Jai's, but some of the high-level semantics will be a bit different.
This makes it trivial to refactor portions of scripts into real Jai code when desired.
Lead sheets are intended to be primarily declarative, setting some externally-provided variables based on simple expressions.
I was even wary of adding local variable declarations intially, but decided that they are just far too useful for extracting out and naming common subexpressions.
The main program provides the bulk of the declarations that a script will use, passing in variables, procedures, and types as context for a script.


## Current Features

Lead sheets can already do most of the simple things one would expect from a scripting language.
Declarations, assignments, procedure calls (with overload resolution), and operator overloading are all fully implemented.
Lead sheets naturally support all data types in Jai, though pointers are a bit of an exception, since we don't currently support explicitly taking the addresses of variables in scripts.

Thanks to the magic of Dyncall, lead sheets' interop with Jai code is extremely simple, and Jai procedures can be added to a script's context with only a single line of code.

Custom user 'compiletime' directives can be defined that will execute immediately after parsing and can insert new code nodes in their place, or insert declarations into the global scope.
Because we use dyncall here, we can make the interface relatively lax, accepting variable amounts and types of parameters (but still with proper runtime typechecking).


## Roadmap

Firstly, I'll state that I do not currently plan to introduce procedure declarations, as I think anything that is doing that much work should instead be implemented in the main program.
I also do not plan to add true support for pointers or dynamic allocations. These are outside of the scope of what a lead sheet should do in my opinion.

After completing the basic functionality and doing a cleanup/improvement pass, I want to start on the metaprogramming facilities.
The idea is to have something as simple and flexible as a regex that will instead operate on the AST to find certain forms of expressions.
I have some ideas about how to make this analysis more powerful while also maintaining flexibility and performance, but for now I will defer writing about those ideas in more detail until they've been implemented and tested.

I plan to eventually work on a bytecode VM that will run the 'compiled' scripts more efficiently, but that is currently on the back burner while I get the basic feature set nailed down. 

Currently the entire system is designed to operate in three discrete phases: parsing, typechecking, and execution. 
But, I have considered also supporting a more REPL sort of usage where new statements can be parsed, typechecked, and executed after the initial construction of the script.
Seeing as this is not the main use case I personally have in mind though, it's not a top priority.


## Technical Details

### Parsing

Lead sheets are parsed using a recursive-descent parser and dead-simple lexer with a single-token memory.
The lexer could probably use a buff eventually, but it has done its job well so far.
There's really not much of anything fancy going on here now, but I do plan to make the directives more powerful so that some parsing capabilities can be extended.

### Typechecking

Typechecking is also very straightforward, with types flowing from the leaves down to root nodes. 
There is some type hinting that gets passed up the tree in certain circumstances, but this is never authoritative.
The most complex case in the typechecker currently is procedure calls, since we have to do overload resolution. (And this will only need to get more complex soon, so that we cna do some coercion of arguments...)

### Execution

The current script execution model is a sort of AST-walking stack machine.
We just walk the AST, pushing and popping values as required. 
My hunch is that, so far as tree-walking interpreters go, lead sheets are still relatively fast.
Since the scripts are essentially statically typed, we only have to typecheck them once, instead of every single time we visit a node.
We also allocate all AST nodes and the script's stack space in a common pool, so memory locality is proably decent.

