-module(almc).

-export([main/1]).

main(Args) ->
    case getopt:parse(options(), Args) of
        {ok, {Options, Files}} ->
            case proplists:get_bool(help, Options) of
                true -> print_help();
                _    -> ok
            end,
            case proplists:get_value(expression, Options) of
                undefined ->
                    [compile_file(Options, File) || File <- Files];
                Exp ->
                    {_, Result} = scan_and_parse(Options, Exp),
                    io:format("~p~n", [Result])
            end;
        {error, {invalid_option, Option}} ->
            io:format("error: invalid option: ~s~n", [Option]), halt(1)
    end.

print_help() -> getopt:usage(options(), atom_to_list(?MODULE)), halt(0).

options() ->
    [{scanner_only, $S, "scanner-only", undefined,
      "Print scanner output and exit"},
     {parser_only, $P, "parser-only", undefined,
      "Print parser output and exit"},
     {assembler_only, $G, "assembler-only", undefined,
      "Print assembler generate and exit"},
     {optimizations_only, $O, "optmizations-only", undefined, 
      "Print optimized assembler and exit"},
     {bytecode_only, $B, "bytecode-only", undefined,
      "Print bytecode and exit"},
     {expression, $e, "expression", string,
      "Compile expression"},
     {help, $h, "help", undefined,
      "Print this help and exit"}].

compile_file(Options, File) ->
    {ok, Bin} = file:read_file(File),
    case scan_and_parse(Options, binary_to_list(Bin)) of
        {bytecode,ByteCode} ->
	    O = filename:rootname(File)++".alb",
	    file:delete(O),
            file:write_file(O,ByteCode);
        {_,Result} ->
            io:format("~p~n", [Result])
    end.

scan_and_parse(Options, String) ->
    try
        Tokens = run(Options, scanner_only,
                     fun() -> alm_scanner:scan(String) end),
        AST    = run(Options, parser_only,
                     fun() -> alm_parser:tokens(Tokens) end),
        ASM    = run(Options, assembler_only,
                     fun() -> alm_compiler:generate(AST) end),
	OASM    = run(Options, optimizations_only,
                     fun() -> alm_opt:run(ASM) end),
        BC     = run(Options, bytecode_only,
                     fun() -> alm_bytecode:generate(OASM) end),
        {bytecode,BC}
    catch
        throw:Result ->
            Result
    end.


run(Options, Option, Func) ->
    case proplists:get_bool(Option, Options) of
        true  -> throw({Option,Func()});
        false -> Func()
    end.
