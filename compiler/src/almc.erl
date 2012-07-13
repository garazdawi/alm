-module(almc).

-export([main/1]).

main(Args) ->
    AST = scan_and_parse(hd(Args)),
    io:format("~p~n", [AST]).

scan_and_parse(String) ->
    Tokens = alm_scanner:scan(String),
    io:format("DEBUG: ~p~n", [Tokens]),
    {ok, AST} = alm_parser:parse(Tokens),
    AST.
