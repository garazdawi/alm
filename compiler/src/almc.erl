-module(almc).

-export([main/1]).

main(Args) ->
    {ok, {Options, Files}} = getopt:parse(options(), Args),
    case proplists:get_bool(help, Options) of
        true -> print_help();
        _    -> ok
    end,
    case proplists:get_value(expression, Options) of
        undefined -> [compile_file(Options, File) || File <- Files];
        Exp       -> io:format("~p~n", [scan_and_parse(Options, Exp)])
    end.

print_help() -> getopt:usage(options(), atom_to_list(?MODULE)), halt(0).

options() ->
    [{scanner_only, $S, "scanner-only", undefined,
      "Print scanner output and exit"},
     {parser_only, $P, "parser-only", undefined,
      "Print parser output and exit"},
     {expression, $e, "expression", string,
      "Compile expression"},
     {help, $h, "help", undefined,
      "Print this help and exit"}].

compile_file(Options, File) ->
    {ok, Bin} = file:read_file(File),
    Result = scan_and_parse(Options, binary_to_list(Bin)),
    io:format("~p~n", [Result]).

scan_and_parse(Options, String) ->
    try
        Tokens = run(Options, scanner_only,
                     fun() -> alm_scanner:scan(String) end),
        AST    = run(Options, parser_only,
                     fun() -> alm_parser:tokens(Tokens) end),
        AST
    catch
        throw:Result ->
            Result
    end.


run(Options, Option, Func) ->
    case proplists:get_bool(Option, Options) of
        true  -> throw(Func());
        false -> Func()
    end.
