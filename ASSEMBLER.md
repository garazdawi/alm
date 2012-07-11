# Assembler code

This document describes the different assembler code and what they do. This document is inspired from [Ruslan's Blog][1], [LUA 5.0][2] and [Erlang/OTP][3].

# Constants

There are two types of constants, `Numbers` and `nil`. `Numbers` are any float or integer which can be represented by a double. `nil` is the end of a list.

# Instructions 

## {func, Name, Arity}

Describes the start of a new function with the given `Name` and `Arity`.

## {call, Name, Arity, LiveVars}

Call the function specified by `Name` and `Arity`. It is assumed that all `LiveVars` are pushed to the apporiate y registers before this instruction is issued. The return value will always be in the x(0) register.

## {tailcall, Name, Arity}

Do a tailcall of the function specified by `Name` and `Arity`.

## {return}

Return from a function call. 

## {label, Name}

A jump label which a test intruction can just to.

## {brt, Test, FalseLabel}

A branching test instruction which jumps to `FalseLabel` if `Test` is 0, and continues executing the next instruction otherwise.

## {gt, Left, Right, Dest}

Stores 1 in `Dest` if `Left` > `Right`, 0 otherwise.

## {lt, Left, Right, Dest}

Stores 1 in `Dest` if `Left` < `Right`, 0 otherwise.

## {eq, Left, Right, Dest}

Stores 1 in `Dest` if `Left` == `Right`, 0 otherwise.

## {move, Source, Dest}

Copies the value of register `Source` to `Dest`. Both `Source` and `Dest` can be an x or y register.

## {load, Constant, Dest}

Loads `Constant` into `Dest`

## {add|sub|mul|div, Left, Right, Dest}

Add, substract, multiply or divide `Left` to `Right` and store result in `Dest`. Checks using `is_number` have to be made to make sure Both `Left` and `Right` are numbers. `Dest` will always be a number. The reason for putting not putting the checks in the instruction is so the compiler is able to optimize away some checks!

## {cons, Head, Tail, Dest}

Create a new list element from `Head` and `Tail` and store result in `Dest`. The `Tail` can be a non-cons/nil element, though this will result in a non-proper list.

## {is_number, Source, Dest}

Stores 1 in `Dest` if the `Source` is a number.

## {is_nil, Source, Dest}

Stores 1 in `Dest` if the `Source` is a nil.

## {throw, Type}

Throw an error. This will print the current stacktrace until the outmost function exits.


   [1]: http://ruslanspivak.com/2011/02/08/register-based-virtual-machine-for-tinypie/
   [2]: http://www.lua.org/doc/jucs05.pdf
   [3]: http://github.com/erlang/otp
