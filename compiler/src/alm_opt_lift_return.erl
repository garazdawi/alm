-module(alm_opt_lift_return).

%
% This pass looks for label,move,return instructions and lifts move+return
% if a jump instruction is used.
%

-export([run/1]).

run(OrigCode) ->
    run(lists:reverse(OrigCode),[],[]).

run([{return} = I|T],_,Acc) ->
    io:format("Found ~p~n",[I]),
    run(T,[I],[I|Acc]);
run([{move,_,_} = I|T],Is,Acc) when is_list(Is) ->
    io:format("Found ~p~n",[I]),
    run(T,[I|Is],[I|Acc]);
run([{label,Lbl} = I|T],Is,Acc) when is_list(Is) ->
    io:format("Found ~p~n",[I]),
    run(T,{Lbl,Is},Acc);
run([{jump,Lbl}|T],{Lbl,Is},Acc) ->
    io:format("Found ~p~n",[{jump,Lbl}]),
    run(T,{Lbl,Is},Is++Acc);
run([I|T],Is,Acc) when is_list(Is) ->
    run(T,undefined,[I|Acc]);
run([I|T],Is,Acc) ->
    run(T,Is,[I|Acc]);
run([],_,Acc) ->
    Acc.
