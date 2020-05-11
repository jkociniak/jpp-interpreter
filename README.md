## Interpreter of modified Latte language for JPP course at MIMUW

### Language description

The language is a modification of a subset of language described at https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2019/Latte/.

A program is a list of function and variable definitions. Function definition consists of return type, unique name, args list and body (block). Passing arguments to functions is possible either by value or reference (taken from C++, so '&' after argument's type in function definition). Program must have function with name 'main', int return type and no args - it is the start point of the program. Non-void return type functions must return a value with 'return' instruction. Functions can be recursive and nested. 

Statements consist of empty statement, block, assignment, return, if (or if + else), while, break, continue, expression call (to allow function calls) and print. Inside blocks, function and variable declarations must be placed at the beginning of block (declarations are not statements!). Declared variables are initialized with default value - 0/""/false for corresponding types. Inside block, variables must have unique names. The binding is static. Variables declared top-level are global and other variables have scope of the block where they were declared and they override variables with the same name declared outside block.

We provide types of int, string, bool, void. We do not allow types conversion.

Expressions consist of identifiers, literals, function calls and arithmetic/logical operators.

### Solution description

First of all, in current version of interpreter, there are couple of things that are not fully implemented:
- there is no handling of break/continue
- in couple of cases regarding wrong return type static check doesn't show position of error in the message
- static check doesn't check if expression in function call is an lvalue, currently this is a runtime error
I would like to correct these things and submit full implementation in June.

The project can be built using `make` command in folder with all `.hs` files.
