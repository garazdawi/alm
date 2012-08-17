Nonterminals Functions Function Statement Expression Cmp Params Param uminus add_op mul_op.
Terminals def identifier '(' ')' '{' '}' '+' '-' '*' '/' ';' ',' integer 'if' else eq lt gt neq '[' '|' ']'.
Rootsymbol Functions.

Left 100 add_op.
Left 200 mul_op.
Unary 500 uminus.

Functions -> Function : ['$1'].
Functions -> Function Functions : ['$1'|'$2'].

Function -> def identifier '(' Params ')' '{' Statement '}'
    : {func,unwrap('$2'),'$4','$7'}.
Function -> def identifier '(' ')' '{' Statement '}'
    : {func,unwrap('$2'),[],'$6'}.

Params -> Param : ['$1'].
Params -> Param ',' Params : ['$1'|'$3'].

Param -> Expression : '$1'.

Statement -> Expression ';' : '$1'.

Expression -> '[' Expression '|' Expression ']' : {cons, '$2','$4'}.
Expression -> '[' ']' : nil.
Expression -> Expression add_op Expression : {'$2','$1','$3'}.
Expression -> Expression mul_op Expression : {'$2','$1','$3'}.
Expression -> Expression Cmp Expression : {'$2','$1','$3'}.
Expression -> '(' Expression ')' : '$2'.
Expression -> 'if' '(' Expression ')' Expression ';' else Expression : 
		 {'if','$3','$5','$8'}.
Expression -> integer : {integer,list_to_integer(unwrap('$1'))}.
Expression -> uminus integer : {integer,-list_to_integer(unwrap('$2'))}.
Expression -> identifier : {variable,unwrap('$1')}.
Expression -> identifier '(' Params ')' : {call, unwrap('$1'),'$3'}.

uminus -> '-' : '$1'.

Cmp -> eq : eq.
Cmp -> neq : neq.
Cmp -> lt : lt.
Cmp -> gt : gt.
add_op -> '+' : add.
add_op -> '-' : subtract.
mul_op -> '*' : multiply.
mul_op -> '/' : divide.

Erlang code.

-export([tokens/1]).

unwrap({_,_,Value}) -> Value.

tokens(Tokens) ->
    {ok, AST} = parse(Tokens),
    AST.
