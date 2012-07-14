-module(alm_compiler).

-export([generate/1]).

generate(AST) ->
    {ASM,_LastReg,_Reg} = generate(AST,[]),
    lists:flatten(ASM).

generate({func,Name,Body},Regs) ->
    {BodyGen,BodyRegs,Reg} = generate(Body,Regs),
    % Only generate move if last operation was not to {x,0}
    Move = if Reg == 0 -> []; true -> [{move,{x,Reg},{x,0}}] end,
    {[{func,Name, 0},BodyGen,Move,{return}],BodyRegs,Reg};
generate({integer,Num},Regs) ->
    Reg = next_reg(Regs),
    {[{load,Num,{x,Reg}}],[Reg|Regs],Reg};
generate({Op,L,R},Regs) 
  when Op == add;
       Op == divide;
       Op == multiply;
       Op == subtract->
    {RHS,RHSRegs,RHSReg} = generate(R,Regs),
    {LHS,LHSRegs,LHSReg} = generate(L,RHSRegs),
    Reg = next_reg(RHSRegs),
    %% Remove unused registers
    AvailRegs = LHSRegs -- [LHSReg,RHSReg],
    %% RHS before LHS in order to not load all constants first
    {[RHS,LHS,{Op,{x,LHSReg},{x,RHSReg},{x,Reg}}],[Reg|AvailRegs],Reg}.
    
next_reg(Regs) ->
    next_reg(lists:sort(Regs),0).
next_reg([N|T],N) ->
    next_reg(T,N+1);
next_reg(_,N) ->
    N.
