-module(alm_opt_dead_move_elim).

%
% This pass tracks registers an deleted any moves which move values 
% which are never used.
%
% Also is a move is detected just after an instruction on the result
% register of the instructions, the move is deleted and the instruction
% updated.
%
% Future improvements:
%  * track moves across instructions and jumps when combining with instructions
%  * 
%

-export([run/1]).

run(OrigCode) ->
    run(lists:reverse(OrigCode),[{x,0}],[],[]).


run([{move,To,NewTo},{I,LHS,RHS,To}|T],UsedRegs,Lbls,Acc) when I =/= call ->
    run([{I,LHS,RHS,NewTo}|T],UsedRegs,Lbls,Acc);
run([{move,To,NewTo},{I,From,To}|T],UsedRegs,Lbls,Acc) when I =/= func ->
    run([{I,From,NewTo}|T],UsedRegs,Lbls,Acc);
run([{func,_,_} = I|T],_,_,Acc) ->
    run(T,[{x,0}],[],[I|Acc]);
run([{move,From,To} = I|T],UsedRegs,Lbls,Acc) ->
    case lists:member(To,UsedRegs) of
	true ->
	    run(T,[From|UsedRegs--[To]],Lbls,[I|Acc]);
	false ->
	    run(T,UsedRegs,Lbls,Acc)
    end;
run([{call,Fun,Args,_}|T],UsedRegs,Lbls,Acc) ->
    YRegs = [R || {y,_} = R <- UsedRegs],
    NewUsedRegs = YRegs ++ [{x,R}||R<-lists:seq(0,Args-1)],
    MaxLiveY = if YRegs == [] -> 0;
		  true -> element(2,lists:max(YRegs))+1
	       end,
    run(T,NewUsedRegs,Lbls,[{call,Fun,Args,MaxLiveY}|Acc]);
run([{_,LH,RH,To} = I|T],UsedRegs,Lbls,Acc) ->
    run(T,[LH,RH|UsedRegs--[To]],Lbls,[I|Acc]);
run([{brt,Reg,Lbl} = I|T],UsedRegs,Lbls,Acc) ->
    run(T,[Reg|UsedRegs]++proplists:get_value(Lbl,Lbls),Lbls,[I|Acc]);
run([{jump,Lbl} = I|T],_UsedRegs,Lbls,Acc) ->
    run(T,proplists:get_value(Lbl,Lbls),Lbls,[I|Acc]);
run([{label,Lbl} = I|T],UsedRegs,Lbls,Acc) ->
    run(T,UsedRegs,[{Lbl,UsedRegs}|Lbls],[I|Acc]);
run([I|T],UsedRegs,Lbls,Acc) ->
    run(T,UsedRegs,Lbls,[I|Acc]);
run([],_UsedRegs,_Lbls,Acc) ->
    compact_y_registers(Acc,0,[],[]).

compact_y_registers([{move,{x,X},{y,Y}}|T],trans,_,Code) ->
    compact_y_registers(T,1,[{Y,0}],[{move,{x,X},{y,0}}|Code]);
compact_y_registers([{move,{x,X},{y,Y}}|T],R,Trans,Code) ->
    compact_y_registers(T,R+1,[{Y,R}|Trans],[{move,{x,X},{y,R}}|Code]);
compact_y_registers([{move,{y,Y},{x,X}}|T],_,Trans,Code) ->
    compact_y_registers(T,trans,Trans,[{move,{y,proplists:get_value(Y,Trans)},
					{x,X}}|Code]);
compact_y_registers([{call,Fun,Args,_}|T],R,Trans,Code) ->
    compact_y_registers(T,R,Trans,[{call,Fun,Args,length(Trans)}|Code]);
compact_y_registers([I|T],R,Trans,Code) ->
    compact_y_registers(T,R,Trans,[I|Code]);
compact_y_registers([],_,_,Code) ->
    lists:reverse(Code).
