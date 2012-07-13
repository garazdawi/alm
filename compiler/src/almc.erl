-module(almc).

-export([main/1]).

main(Files) ->
    [compile_file(File) || File <- Files].

compile_file(File) ->
    {ok, Bin} = file:read_file(File),
    AST = scan_and_parse(binary_to_list(Bin)),
    io:format("~p~n", [AST]).

scan_and_parse(String) ->
    Tokens = alm_scanner:scan(String),
    io:format("DEBUG: ~p~n", [Tokens]),
    {ok, AST} = alm_parser:parse(Tokens),
    AST.
