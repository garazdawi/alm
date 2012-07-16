Nonterminals Functions Function Statement Expression Operator Params Param.
Terminals def identifier '(' ')' '{' '}' '+' '-' '*' '/' ';' ',' integer 'if' else eq lt gt neq.
Rootsymbol Functions.

Left 100 '-'.
Left 200 '+'.
Left 300 '*'.
Left 400 '/'.

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

Expression -> 'if' '(' Expression ')' Expression ';' else Expression : 
		 {'if','$3','$5','$8'}.
Expression -> 'if' '(' Expression ')' Expression : {'if','$3','$5'}.
Expression -> '(' Expression ')' : '$2'.
Expression -> Expression Operator Expression : {'$2','$1','$3'}.
Expression -> integer : {integer,list_to_integer(unwrap('$1'))}.
Expression -> identifier : {variable,unwrap('$1')}.
Expression -> identifier '(' Params ')' : {call, unwrap('$1'),'$3'}.

Operator -> '+' : add.
Operator -> '-' : subtract.
Operator -> '*' : multiply.
Operator -> '/' : divide.
Operator -> eq : eq.
Operator -> neq : neq.
Operator -> lt : lt.
Operator -> gt : gt.

Erlang code.

-export([tokens/1]).

unwrap({_,_,Value}) -> Value.

tokens(Tokens) ->
    {ok, AST} = parse(Tokens),
    AST.
