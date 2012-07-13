-module(alm_bytecode).

-export([generate/1]).

-define(HEADER,<<"alm">>).
-define(VSN,<<0:16,1:16>>).

generate(ASM) ->
    Constants = lists:usort(get_constants(ASM)),
    [?HEADER,?VSN,<<(length(Constants)):32>>,
     [gen_constant(Const) || Const <- Constants],
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
    <<4:8,Int:32>>;
gen_constant(Str) when length(Str) < 255 ->
    Len = length(Str),
    Bin = list_to_binary(Str),
    <<Len:8,Bin/binary>>.

-define(MOVE,     0).
-define(LOAD,     1).
-define(FUNC,     2).
-define(ADD,      3).
-define(DIVIDE,   4).
-define(MULTIPLY, 5).
-define(SUBTRACT, 6).
-define(RETURN,   7).

-define(iABC(I,A,B,C),<<I:6,A:8,B:9,C:9>>).
-define(X(R),(element(2,R))).

gen_code([{load,Const,Reg}|T],Constants) ->
    N = index(Const,Constants),
    [?iABC(?LOAD,N,?X(Reg),0) | gen_code(T,Constants)];
gen_code([{move,Source,Dest}|T],Constants) ->
    [?iABC(?LOAD,?X(Source),?X(Dest),0) | gen_code(T,Constants)];
gen_code([{return}|T],Constants) ->
    [?iABC(?RETURN,0,0,0) | gen_code(T,Constants)];
gen_code([{func,Name,Arity}|T],Constants) ->
    N = index(Name,Constants),
    [?iABC(?FUNC,N,Arity,0) | gen_code(T,Constants)];
gen_code([{add,Left,Right,Dest}|T],Constants) ->
    [?iABC(?ADD,?X(Left),?X(Right),?X(Dest)) | gen_code(T,Constants)];
gen_code([{multiply,Left,Right,Dest}|T],Constants) ->
    [?iABC(?MULTIPLY,?X(Left),?X(Right),?X(Dest)) | gen_code(T,Constants)];
gen_code([{subtract,Left,Right,Dest}|T],Constants) ->
    [?iABC(?SUBTRACT,?X(Left),?X(Right),?X(Dest)) | gen_code(T,Constants)];
gen_code([{divide,Left,Right,Dest}|T],Constants) ->
    [?iABC(?DIVIDE,?X(Left),?X(Right),?X(Dest)) | gen_code(T,Constants)];
gen_code([],_) ->
    [].

index(Elem,List) ->
    index(Elem,List,0).
index(Elem,[Elem|_],Index) ->
    Index;
index(Elem,[_|T],Index) ->
    index(Elem,T,Index+1).
