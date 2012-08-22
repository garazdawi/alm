-module(alm_opt_liveness_analysis).

-export([run/1]).

-record(liveness,{ alive_regs = [] }).
-define(l(I,AliveRegs),{I,#liveness{ alive_regs = AliveRegs }}).
-define(l(I),?l(I,UsedRegs)).

run(Code) ->
    run(lists:reverse(Code),[],[],[]).

run([{return} = I|T],_,_,Acc) ->
    run(T,[{x,0}],[],[?l(I,[{x,0}])|Acc]);
run([{move,From,To} = I|T],UsedRegs,Lbls,Acc) ->
    case lists:member(To,UsedRegs) of
	true ->
	    run(T,[From|UsedRegs--[To]],Lbls,[?l(I)|Acc]);
	false ->
	    run(T,UsedRegs,Lbls,[?l(I)|Acc])
    end;
run([{call,_,Args,_} = I|T],UsedRegs,Lbls,Acc) ->
    YRegs = [R || {y,_} = R <- UsedRegs],
    NewUsedRegs = YRegs ++ [{x,R}||R<-lists:seq(0,Args-1)],
    run(T,NewUsedRegs,Lbls,[?l(I)|Acc]);
run([{_,LH,RH,To} = I|T],UsedRegs,Lbls,Acc) ->
    run(T,[LH,RH|UsedRegs--[To]],Lbls,[?l(I)|Acc]);
run([{load,_,To} = I|T],UsedRegs,Lbls,Acc) ->
    run(T,UsedRegs -- [To], Lbls,[?l(I)|Acc]);
run([{brt,Reg,Lbl} = I|T],UsedRegs,Lbls,Acc) ->
    run(T,[Reg|UsedRegs]++proplists:get_value(Lbl,Lbls),Lbls,[?l(I)|Acc]);
run([{jump,Lbl} = I|T],_UsedRegs,Lbls,Acc) ->
    run(T,proplists:get_value(Lbl,Lbls),Lbls,[?l(I,[])|Acc]);
run([{label,Lbl} = I|T],UsedRegs,Lbls,Acc) ->
    run(T,UsedRegs,[{Lbl,UsedRegs}|Lbls],[?l(I)|Acc]);
run([I|T],UsedRegs,Lbls,Acc) when element(1,I) == func ->
    run(T,UsedRegs,Lbls,[I|Acc]);
run([],_UsedRegs,_Lbls,Acc) ->
    Acc.

