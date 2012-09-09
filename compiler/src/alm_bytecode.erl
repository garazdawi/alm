-module(alm_bytecode).

-export([generate/1]).

-define(HEADER,<<"alm">>).
-define(VSN,<<0:16,1:16>>).

generate(ASM) ->
    Constants = lists:usort(get_constants(ASM)),
    iolist_to_binary(
      [?HEADER,?VSN,<<(length(Constants)):32>>,
       [gen_constant(Const) || Const <- Constants],
       <<(length(ASM)):32>>,
       gen_code(ASM,Constants)]).

get_constants([{load,Const,_}|T]) ->
    [Const|get_constants(T)];
get_constants([{func,Name,_}|T]) ->
    [Name|get_constants(T)];
get_constants([_|T]) ->
    get_constants(T);
get_constants([]) ->
    [].

-define(INTEGER, 0).
-define(STRING, 1).
-define(NIL, 2).

gen_constant(nil) ->
    <<?NIL,0>>;
gen_constant(Int) when is_integer(Int) ->
    <<?INTEGER,4,Int:32>>;
gen_constant(Str) when length(Str) < 255 ->
    Len = length(Str),
    Bin = list_to_binary(Str),
    <<?STRING,Len:8,Bin/binary>>.

-define(MOVE_XX,  0).
-define(MOVE_XY,  1).
-define(MOVE_YX,  2).
-define(LOAD,     3).
-define(FUNC,     4).
-define(ADD,      5).
-define(DIVIDE,   6).
-define(MULTIPLY, 7).
-define(SUBTRACT, 8).
-define(RETURN,   9).
-define(JUMP,     10).
-define(BRT,      11).
-define(CALL,     12).
-define(LABEL,    13).
-define(CONS,     14).
-define(GC,       15).
-define(EQ,       20).
-define(NEQ,      21).
-define(LT,       22).
-define(GT,       23).

-define(iABC(I,A,B,C),<<(I):6,(A):8,(B):9,(C):9>>).
-define(iABx(I,A,B),  <<(I):6,(A):8,(B):18>>).
-define(X(R),(element(2,R))).
-define(C(C),(index(C,Constants))).
-define(gen_code(M,E),gen_code([M|T],Constants) -> [E | gen_code(T,Constants)]).

?gen_code({move,{x,S},{x,D}}, ?iABC(?MOVE_XX, S,       D,       0     ));
?gen_code({move,{x,S},{y,D}}, ?iABC(?MOVE_XY, S,       D,       0     ));
?gen_code({move,{y,S},{x,D}}, ?iABC(?MOVE_YX, S,       D,       0     ));
?gen_code({load,C,D},         ?iABC(?LOAD,    ?C(C),   ?X(D),   0     ));
?gen_code({func,N,A},         ?iABC(?FUNC,    ?C(N),   A,       0     ));
?gen_code({add,L,R,D},        ?iABC(?ADD,     ?X(L),   ?X(R),   ?X(D) ));
?gen_code({divide,L,R,D},     ?iABC(?DIVIDE,  ?X(L),   ?X(R),   ?X(D) ));
?gen_code({multiply,L,R,D},   ?iABC(?MULTIPLY,?X(L),   ?X(R),   ?X(D) ));
?gen_code({subtract,L,R,D},   ?iABC(?SUBTRACT,?X(L),   ?X(R),   ?X(D) ));
?gen_code({return},           ?iABC(?RETURN,  0,       0,       0     ));
?gen_code({jump,Lbl},         ?iABx(?JUMP,    0,       Lbl            ));
?gen_code({brt,Test,Lbl},     ?iABx(?BRT,     ?X(Test),Lbl            ));
?gen_code({call,N,A,L},       ?iABC(?CALL,    ?C(N),   A,       L     ));
?gen_code({label,Lbl},        ?iABx(?LABEL,   0,       Lbl            ));
?gen_code({cons,L,R,D},       ?iABC(?CONS,    ?X(L),   ?X(R),   ?X(D) ));
?gen_code({eq,L,R,D},         ?iABC(?EQ,      ?X(L),   ?X(R),   ?X(D) ));
?gen_code({gc,X,Y,Needed},    ?iABC(?GC,      X,       Y,       Needed));
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
