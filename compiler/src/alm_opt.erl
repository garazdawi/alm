-module(alm_opt).

-export([run/1,run/2]).

run(Code) ->
    run(Code,[lift_return,dead_move_elim,dead_move_elim]).

run(OrigCode, Passes) ->
    lists:foldl(
      fun(Pass,Code) ->
	      (list_to_atom("alm_opt_"++atom_to_list(Pass))):run(Code)
      end,OrigCode,Passes).
