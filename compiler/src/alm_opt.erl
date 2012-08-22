-module(alm_opt).

-export([run/1,run/2]).

run(Code) ->
    run(Code,[{"liveness_analysis",1},"lift_return","dead_move_elim"]).

run(OrigCode, Passes) ->
    lists:foldl(fun run_int/2,OrigCode,Passes).

run_int({Pass,N},Code) when is_list(Pass) ->
    run_int({list_to_atom("alm_opt_"++Pass),N},Code);
run_int({_M,0},Code) ->
    Code;
run_int({M,N},Code) ->
    case M:run(Code) of
	Code ->
	    Code;
	NewCode ->
	    run_int({M,N-1},NewCode)
    end;
run_int(Pass,Code) ->
    run_int({Pass,3},Code).

    
