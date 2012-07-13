-module(almc_SUITE).

-compile(export_all).

all() -> 
    [scanner,parser].

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
    {ok, {func,"add",{add,1,{multiply,2,{divide,3,{add,5,91}}}}}} 
	= erl_parse:parse_term(Toks).    
