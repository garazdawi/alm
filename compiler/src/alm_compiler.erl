-module(alm_compiler).

-export([generate/1]).

generate(AST) ->
    {ASM,_LastReg} = generate(AST,0),
    lists:flatten(ASM).

generate({func,Name,Body},CurrReg) ->
    {BodyGen,BodyReg} = generate(Body,CurrReg),
    {[{func,Name, 0},BodyGen,{move,{x,BodyReg},{x,0}},{return}],CurrReg};
generate({integer,Num},CurrReg) ->
    {[{load,Num,{x,CurrReg+1}}],CurrReg+1};
generate({Op,R,L},CurrReg) when Op == add; 
				Op == divide; 
				Op == multiply; 
				Op == subtract->
    {LHS,LHSReg} = generate(R,CurrReg),
    {RHS,RHSReg} = generate(L,LHSReg),
    {[LHS,RHS,{Op,{x,LHSReg},{x,RHSReg},{x,RHSReg+1}}],RHSReg+1}.
    

     



