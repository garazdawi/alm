-module(almc_SUITE).

-compile(export_all).

all() -> 
    [scanner, parser, compiler].

test() ->
    "def add() {
        1+2*3/(5+91);
    }".

scanner(_Config) ->
    Res = os:cmd("almc -S -e \""++test()++"\""),
    {ok, Toks, _} = erl_scan:string(Res++"."),
    {ok, [{def,_},
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
	  {'}',_}]} = erl_parse:parse_term(Toks).
    
parser(_Config) ->
    Res = os:cmd("almc -P -e \""++test()++"\""),
    {ok, Toks, _} = erl_scan:string(Res++"."),
    {ok, {func,"add",
	  {add,{number,1},
	   {multiply,{number,2},
	    {divide,{number,3},
	     {add,{number,5},
	      {number,91}}}}}}}
	= erl_parse:parse_term(Toks).    

compiler(_Config) ->
    Res = os:cmd("almc -G -e \""++test()++"\""),
    {ok, Toks, _} = erl_scan:string(Res++"."),
    {ok,[{func,"add",0},
	 {load,1,{x,1}},
	 {load,2,{x,2}},
	 {load,3,{x,3}},
	 {load,2,{x,4}},
	 {load,95,{x,5}},
	 {add,{x,4},{x,5},{x,6}},
	 {divide,{x,3},{x,6},{x,7}},
	 {multiply,{x,2},{x,7},{x,8}},
	 {add,{x,1},{x,8},{x,9}},
	 {move,{x,9},{x,0}},
	 {return}]} = erl_parse:parse_term(Toks).    
