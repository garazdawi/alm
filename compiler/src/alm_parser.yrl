Nonterminals Function Statement Expression Operator Params Param.
Terminals def identifier '(' ')' '{' '}' '+' '-' '*' '/' ';' ',' integer.
Rootsymbol Function.

Left 100 '-'.
Left 200 '+'.
Left 300 '*'.
Left 400 '/'.

Function -> def identifier '(' Params ')' '{' Statement '}'
    : {func,unwrap('$2'),'$4','$7'}.
Function -> def identifier '(' ')' '{' Statement '}'
    : {func,unwrap('$2'),[],'$6'}.

Params -> Param : ['$1'].
Params -> Param ',' Params : ['$1'|'$3'].

Param -> identifier : unwrap('$1').

Statement -> Expression ';' : '$1'.

Expression -> '(' Expression ')' : '$2'.
Expression -> Expression Operator Expression : {'$2','$1','$3'}.
Expression -> integer : {integer,list_to_integer(unwrap('$1'))}.
Expression -> identifier : {variable,unwrap('$1')}.

Operator -> '+' : add.
Operator -> '-' : subtract.
Operator -> '*' : multiply.
Operator -> '/' : divide.

Erlang code.

-export([tokens/1]).

unwrap({_,_,Value}) -> Value.

tokens(Tokens) ->
    {ok, AST} = parse(Tokens),
    AST.
