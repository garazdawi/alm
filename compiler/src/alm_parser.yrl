Nonterminals Function Statement Expression Operator.
Terminals def identifier '(' ')' '{' '}' '+' '-' '*' '/' ';' integer.
Rootsymbol Function.

Left 100 '-'.
Left 200 '+'.
Left 300 '*'.
Left 400 '/'.

Function -> def identifier '(' ')' '{' Statement '}'
    : {func,unwrap('$2'),'$6'}.

Statement -> Expression ';' : '$1'.

Expression -> '(' Expression ')' : '$2'.
Expression -> Expression Operator Expression : {'$2','$1','$3'}.
Expression -> integer : {integer,list_to_integer(unwrap('$1'))}.

Operator -> '+' : add.
Operator -> '-' : subtract.
Operator -> '*' : multiply.
Operator -> '/' : divide.

Erlang code.

unwrap({_,_,Value}) -> Value.
