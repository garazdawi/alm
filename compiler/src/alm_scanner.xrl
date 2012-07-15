Definitions.

D  = [0-9]
L  = [a-z]
S  = [\000-\s]
O  = (\(|\)|\{|\}|\+|-|\*|/|;)

Rules.

def  : {token,{def,TokenLine}}.
{L}+ : {token,{identifier,TokenLine,TokenChars}}.
{O}  : {token,{list_to_atom(TokenChars),TokenLine}}.
{D}+ : {token,{integer,TokenLine,TokenChars}}.
,    : {token,{',',TokenLine}}.
{S}+ : skip_token.

Erlang code.

-export([scan/1]).

scan(String) -> {ok, Tokens, _EndLine} = ?MODULE:string(String), Tokens.
