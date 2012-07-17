# Assembler code

This document describes the different assembler code and what they do. This document is inspired from [Ruslan's Blog][1], [LUA 5.0][2] and [Erlang/OTP][3].

# Constants

There are two types of constants, `Numbers` and `nil`. `Numbers` are any float or integer which can be represented by a double. `nil` is the end of a list.

# Encoding

Each instruction is encoded into a 32 bit unsigned integer.

There are two ways which an instruction can be encoded:

    CCCCCCCC CBBBBBBB BBAAAAAA AAIIIIII  (iABC)

    BBBBBBBB BBBBBBBB BBAAAAAA AAIIIIII  (iABx)

When implementing it, each instruction will be 64 bits with the trailing 32 bytes being 0. This could be optmized later on.

# Instructions

## {func, Name, Arity}

Describes the start of a new function with the given `Name` and `Arity`.

Enc: This instruction gets subsituted with a label when loading.

## {call, Name, Arity, LiveVars}

Call the function specified by `Name` and `Arity`. It is assumed that all `LiveVars` are pushed to the apporiate y registers before this instruction is issued. The return value will always be in the x(0) register.

Enc: iABC

## {tailcall, Name, Arity}

Do a tailcall of the function specified by `Name` and `Arity`.

Enc: iABC

## {return}

Return from a function call. 

Enc: iABC

## {label, Name}

A jump label which a test intruction can jump to.

Enc: iABx, 

## {brt, Test, FalseLabel}

A branching test instruction which jumps to `FalseLabel` if `Test` is 0, and continues executing the next instruction otherwise.

Enc: iABx, Bx contains a signed PC offset

## {jump, Label}

An unconditional jump to the specified label. This instruction could be deleted and `brt` with Test set to 0 could be used.

Enc: iABx, Bx contains a signed PC offset

## {gt, Left, Right, Dest}

Stores 1 in `Dest` if `Left` > `Right`, 0 otherwise.

Enc: iABC

## {lt, Left, Right, Dest}

Stores 1 in `Dest` if `Left` < `Right`, 0 otherwise.

Enc: iABC

## {eq, Left, Right, Dest}

Stores 1 in `Dest` if `Left` == `Right`, 0 otherwise.

## {neq, Left, Right, Dest}

Stores 1 in `Dest` if `Left` != `Right`, 0 otherwise.

Enc: iABC

## {move_xx, Source, Dest}

Copies the value of x register `Source` to x register `Dest`. 

Enc: iABC

## {move_xy, Source, Dest}

Copies the value of x register `Source` to y register `Dest`. 

Enc: iABC

## {move_yx, Source, Dest}

Copies the value of y register `Source` to x register `Dest`. 

Enc: iABC

## {load, Constant, Dest}

Loads `Constant` into `Dest`

Enc: iABC

## {add|sub|mul|div, Left, Right, Dest}

Add, substract, multiply or divide `Left` to `Right` and store result in `Dest`. Checks using `is_number` have to be made to make sure Both `Left` and `Right` are numbers. `Dest` will always be a number. The reason for putting not putting the checks in the instruction is so the compiler is able to optimize away some checks!

Enc: iABC

## {cons, Head, Tail, Dest}

Create a new list element from `Head` and `Tail` and store result in `Dest`. The `Tail` can be a non-cons/nil element, though this will result in a non-proper list.

Enc: iABC

## {is_number, Source, Dest}

Stores 1 in `Dest` if the `Source` is a number.

Enc: iABC

## {is_nil, Source, Dest}

Stores 1 in `Dest` if the `Source` is a nil.

Enc: iABC

## {throw, Type}

Throw an error. This will print the current stacktrace until the outmost function exits.

Enc: iABC

   [1]: http://ruslanspivak.com/2011/02/08/register-based-virtual-machine-for-tinypie/
   [2]: http://www.lua.org/doc/jucs05.pdf
   [3]: http://github.com/erlang/otp
