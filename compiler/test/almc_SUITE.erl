-module(almc_SUITE).

-compile(export_all).

all() -> 
    [scanner, parser, compiler].

test() ->
    "def add() {
        1+3*30/(95-5);
    }".

-define(RUN_SCAN_TEST(Test,Flag,Program),
	Result = run_and_scan(Flag,Program),
	try
	    Test = Result
	catch _E:_R ->
		ct:pal("~p",[??Test]),
		ct:fail(Result)
	end).

scanner(_Config) ->
    ?RUN_SCAN_TEST([{def,_},
		    {identifier,_,"add"},
		    {'(',_},
		    {')',_},
		    {'{',_},
		    {integer,_,"1"},
		    {'+',_},
		    {integer,_,"3"},
		    {'*',_},
		    {integer,_,"30"},
		    {'/',_},
		    {'(',_},
		    {integer,_,"95"},
		    {'-',_},
		    {integer,_,"5"},
		    {')',_},
		    {';',_},
		    {'}',_}],"-S",test()).
    
parser(_Config) ->
    ?RUN_SCAN_TEST({func,"add",
		    {add,{integer,1},
		     {multiply,{integer,3},
		      {divide,{integer,30},
		       {subtract,{integer,95},{integer,5}}}}}},
		   "-P",test()).

compiler(_Config) ->
    run_scan_test([{func,"add",0},
		    {load,5,{x,0}},
		    {load,95,{x,1}},
		    {subtract,{x,1},{x,0},{x,1}},
		    {load,30,{x,0}},
		    {divide,{x,0},{x,1},{x,0}},
		    {load,3,{x,1}},
		    {multiply,{x,1},{x,0},{x,1}},
		    {load,1,{x,0}},
		    {add,{x,0},{x,1},{x,0}},
		    {return}],"-G",test()).


run_scan_test(Test,Flag,String) ->
    Result = run_and_scan(Flag,String),
    try
	Test = Result
    catch _E:_R ->
	    ct:pal("~p = ~p",[Test,Result]),
	    ct:fail(Result)
    end.
		    
run_and_scan(Flag,String) ->
    Res = os:cmd("almc "++Flag++" -e \""++String++"\""),
    {ok, Toks, _} = erl_scan:string(Res++"."),
    case erl_parse:parse_term(Toks) of
	{ok,Term} -> Term;
	_E -> ct:fail(Res)
    end.
	    
	    
