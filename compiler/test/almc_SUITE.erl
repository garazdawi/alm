-module(almc_SUITE).

-compile(export_all).

all() -> 
    [scanner, parser, compiler].

test() ->
    "def add() {
        1+2*3/(5+91);
    }".

-define(RUN_SCAN_TEST(Test,Flag,Program),
	Result = run_and_scan(Flag,Program),
	try
	    Test = Result
	catch _E:_R ->
		ct:pal("~p = ~p",[??Test,Result]),
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
		    {integer,_,"2"},
		    {'*',_},
		    {integer,_,"3"},
		    {'/',_},
		    {'(',_},
		    {integer,_,"5"},
		    {'+',_},
		    {integer,_,"91"},
		    {')',_},
		    {';',_},
		    {'}',_}],"-S",test()).
    
parser(_Config) ->
    ?RUN_SCAN_TEST({func,"add",
		    {add,{integer,1},
		     {multiply,{integer,2},
		      {divide,{integer,3},
		       {add,{integer,5},{integer,91}}}}}},
		   "-P",test()).

compiler(_Config) ->
    ?RUN_SCAN_TEST([{func,"add",0},
		   {load,1,{x,1}},
		   {load,2,{x,2}},
		   {load,3,{x,3}},
		   {load,5,{x,4}},
		   {load,91,{x,5}},
		   {add,{x,4},{x,5},{x,6}},
		   {divide,{x,3},{x,6},{x,7}},
		   {multiply,{x,2},{x,7},{x,8}},
		   {add,{x,1},{x,8},{x,9}},
		   {move,{x,9},{x,0}},
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
	    
	    
