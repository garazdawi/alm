# Assembler code

This document describes the different assembler code and what they do.

# Constants

There are two types of constants, `Numbers` and `nil`. `Numbers` are any float or integer which can be represented by a double. `nil` is the end of a list.

# Instructions 

## {func, Name, Arity}

Describes the start of a new function with the given `Name` and `Arity`.

## {call, Name, Arity, LiveVars}

Call the function specified by `Name` and `Arity`. It is assumed that all `LiveVars` are pushed to the apporiate y registers before this instruction is issued. The return value will always be in the x(0) register.

If LiveVars is 0 and the instruction is followed by a return instruction, a tail call is made.

## {return}

Return from a function call. 

## {label, Name}

A jump label which a test intruction can just to.

## {test, Test, TrueLabel, FalseLabel}

A branching test instruction which jump to `FalseLabel` if `Test` is 0, and `TrueLabel` otherwise.

## {gt, Left, Right, Dest}

Stores 1 in `Dest` if `Left` > `Right`, 0 otherwise.

## {lt, Left, Right, Dest}

Stores 1 in `Dest` if `Left` < `Right`, 0 otherwise.

## {eq, Left, Right, Dest}

Stores 1 in `Dest` if `Left` == `Right`, 0 otherwise.

## {move, Source, Dest}

Copies the value of register `Source` to `Dest`.

## {add, Left, Right, Dest}

Add `Left` to `Right` and store result in `Dest`

## {sub, Left, Right, Dest}

Subtract `Left` from `Right` and store result in `Dest`

## {mul, Left, Right, Dest}

Multiply `Left` with `Right` and store result in `Dest`

## {div, Left, Right, Dest}

Divide `Left` with `Right` and store result in `Dest`

## {cons, Head, Tail, Dest}

Create a new list element from `Head` and `Tail` and store result in `Dest`