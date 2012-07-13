-module(almc).

-export([main/1]).

main(Args) ->
    {ok, {Options, Files}} = getopt:parse(options(), Args),
    case proplists:get_bool(help, Options) of
        true -> getopt:usage(options(), atom_to_list(?MODULE));
        _    -> [compile_file(Options, File) || File <- Files]
    end.

options() ->
    [{scanner_only, $S, "scanner-only", undefined,
      "Print scanner output and exit"},
     {help, $h, "help", undefined,
      "Print this help and exit"}].

compile_file(Options, File) ->
    {ok, Bin} = file:read_file(File),
    Result = scan_and_parse(Options, binary_to_list(Bin)),
    io:format("~p~n", [Result]).

scan_and_parse(Options, String) ->
    Tokens = alm_scanner:scan(String),
    case proplists:get_bool(scanner_only, Options) of
        true -> Tokens;
        _    -> {ok, AST} = alm_parser:parse(Tokens), AST
    end.
