-module(alm_opt_gc).

%
% This pass looks for instructions which do heap allocation and
% insert the appropriate gc check instructions.
%

-export([run/1]).

run(OrigCode) ->
    run(lists:reverse(OrigCode),0,[],[]).

run([{return} = I|T],_,Lbls,Acc) ->
    run(T,0,Lbls,[I|Acc]);
run([{call,_,_,LiveY} = I|T],Alloc,Lbls,Acc) when Alloc > 0 ->
    run(T,1,Lbls,[I,{gc,1,LiveY,Alloc}|Acc]);
run([{call,_,_,_} = I|T],Alloc,Lbls,Acc)  ->
    run(T,Alloc+1,Lbls,[I|Acc]);
run([{func,_,Arity} = I|T],Alloc,Lbls,Acc) when Alloc > 0 ->
    run(T,0,Lbls,[I,{gc,Arity,0,Alloc}|Acc]);
run([{cons,_,_,_} = I|T],Alloc,Lbls,Acc) ->
    run(T,Alloc+2,Lbls,[I|Acc]);
run([{move,_,{y,_}} = I|T],Alloc,Lbls,Acc) ->
    run(T,Alloc+1,Lbls,[I|Acc]);
run([{label,N} = I|T],Alloc,Lbls,Acc) ->
    run(T,Alloc,[{N,Alloc}|Lbls],[I|Acc]);
run([{jump,N} = I|T],_Alloc,Lbls,Acc) ->
    run(T,proplists:get_value(N,Lbls),Lbls,[I|Acc]);
run([{brt,_,N} = I|T],Alloc,Lbls,Acc) ->
    case proplists:get_value(N,Lbls) of
	NAlloc when NAlloc > Alloc -> 
	    run(T,NAlloc,Lbls,[I|Acc]);
	_ ->
	    run(T,Alloc,Lbls,[I|Acc])
    end;	    
run([I|T],Alloc,Lbls,Acc) ->
    run(T,Alloc,Lbls,[I|Acc]);
run([],_,_,Acc) ->
    Acc.
