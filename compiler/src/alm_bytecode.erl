-module(alm_bytecode).

-export([generate/1]).

-define(HEADER,<<"alm">>).
-define(VSN,<<0:16,1:16>>).

generate(ASM) ->
    Constants = lists:usort(get_constants(ASM)),
    [?HEADER,?VSN,<<(length(Constants)):32>>,
     [gen_constant(Const) || Const <- Constants],
     <<(length(ASM)):32>>,
     gen_code(ASM,Constants)].

get_constants([{load,Const,_}|T]) ->
    [Const|get_constants(T)];
get_constants([{func,Name,_}|T]) ->
    [Name|get_constants(T)];
get_constants([_|T]) ->
    get_constants(T);
get_constants([]) ->
    [].

gen_constant(Int) when is_integer(Int) ->
    <<0,4,Int:32>>;
gen_constant(Str) when length(Str) < 255 ->
    Len = length(Str),
    Bin = list_to_binary(Str),
    <<1,Len:8,Bin/binary>>.

-define(MOVE,     0).
-define(LOAD,     1).
-define(FUNC,     2).
-define(ADD,      3).
-define(DIVIDE,   4).
-define(MULTIPLY, 5).
-define(SUBTRACT, 6).
-define(RETURN,   7).
-define(JUMP,     8).
-define(TEST,     9).
-define(CALL,     10).
-define(LABEL,    11).
-define(EQ,       20).
-define(NEQ,      21).
-define(LT,       22).
-define(GT,       23).

-define(iABC(I,A,B,C),<<(I):6,(A):8,(B):9,(C):9>>).
-define(iABx(I,A,B),  <<(I):6,(A):8,(B):18>>).
-define(X(R),(element(2,R))).
-define(C(C),(index(C,Constants))).
-define(gen_code(M,E),gen_code([M|T],Constants) -> [E | gen_code(T,Constants)]).

?gen_code({load,C,D},         ?iABC(?LOAD,    ?C(C),   ?X(D),   0     ));
?gen_code({move,S,D},         ?iABC(?MOVE,    ?X(S),   ?X(D),   0     ));
?gen_code({return},           ?iABC(?RETURN,  0,       0,       0     ));
?gen_code({func,N,A},         ?iABC(?FUNC,    ?C(N),   A,       0     ));
?gen_code({add,L,R,D},        ?iABC(?ADD,     ?X(L),   ?X(R),   ?X(D) ));
?gen_code({multiply,L,R,D},   ?iABC(?MULTIPLY,?X(L),   ?X(R),   ?X(D) ));
?gen_code({subtract,L,R,D},   ?iABC(?SUBTRACT,?X(L),   ?X(R),   ?X(D) ));
?gen_code({divide,L,R,D},     ?iABC(?DIVIDE,  ?X(L),   ?X(R),   ?X(D) ));
?gen_code({jump,Lbl},         ?iABx(?JUMP,    0,       Lbl            ));
?gen_code({test,Test,Lbl},    ?iABx(?TEST,    ?X(Test),?C(Lbl)        ));
?gen_code({call,N,A},         ?iABC(?TEST,    ?C(N),   A,       0     ));
?gen_code({label,Lbl},        ?iABx(?LABEL,   0,       Lbl            ));
?gen_code({eq,L,R,D},         ?iABC(?EQ,      ?X(L),   ?X(R),   ?X(D) ));
?gen_code({neq,L,R,D},        ?iABC(?NEQ,     ?X(L),   ?X(R),   ?X(D) ));
?gen_code({lt,L,R,D},         ?iABC(?LT,      ?X(L),   ?X(R),   ?X(D) ));
?gen_code({gt,L,R,D},         ?iABC(?GT,      ?X(L),   ?X(R),   ?X(D) ));
gen_code([],_) ->
    [].

index(Elem,List) ->
    index(Elem,List,0).
index(Elem,[Elem|_],Index) ->
    Index;
index(Elem,[_|T],Index) ->
    index(Elem,T,Index+1).
