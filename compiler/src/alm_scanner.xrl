Definitions.

D  = [0-9]
L  = [a-z]
LD = [a-z0-9_]
S  = [\000-\s]
O  = (\(|\)|\{|\}|\+|-|\*|/|;)

Rules.

def  : {token,{def,TokenLine}}.
if   : {token,{'if',TokenLine}}.
else : {token,{else,TokenLine}}.
==   : {token,{eq,TokenLine}}.
<    : {token,{lt,TokenLine}}.
>    : {token,{gt,TokenLine}}.
!=   : {token,{neq,TokenLine}}.
{L}{LD}* : {token,{identifier,TokenLine,TokenChars}}.
{O}  : {token,{list_to_atom(TokenChars),TokenLine}}.
{D}+ : {token,{integer,TokenLine,TokenChars}}.
,    : {token,{',',TokenLine}}.
\[    : {token,{'[',TokenLine}}.
\]    : {token,{']',TokenLine}}.
\|    : {token,{'|',TokenLine}}.
{S}+ : skip_token.

Erlang code.

-export([scan/1]).

scan(String) -> {ok, Tokens, _EndLine} = ?MODULE:string(String), Tokens.
