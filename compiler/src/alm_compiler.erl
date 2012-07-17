-module(alm_compiler).

-export([generate/1]).

-record(s,{ regs = [], vars = [], funcname, labelcnt = 0 }).

generate(AST) ->
    dbg:tracer(),dbg:p(all,c),
    %dbg:tpl(?MODULE,x),
    lists:flatten(generate(AST,#s{})).

generate([Func|T],S) ->
    %% We carry labelcnt beteen functions, the others are deleted in {func}
    {ASM,NewS} = generate(Func,S),
    [ASM|generate(T,NewS)];
generate([],_) ->
    [];
generate({func,Name,Args,Body},S) ->
    {ArgMap,ArgRegs} = lists:mapfoldl(fun({variable,Arg},RegAcc) ->
					      N = next_reg(RegAcc),
					      {{Arg,N},[N|RegAcc]}
				      end,[],Args),
    {BodyGen,BodyS,Reg} = generate(Body,S#s{funcname = Name,
					     vars = ArgMap,
					     regs = ArgRegs}),
    % Only generate move if last operation was not to {x,0}
    Move = if Reg == 0 -> []; true -> [{move,{x,Reg},{x,0}}] end,
    {[{func,Name, length(Args)},BodyGen,Move,{return}],BodyS};
generate({integer,Num},S) ->
    {Reg,NewS} = next_reg(S),
    {[{load,Num,{x,Reg}}],NewS,Reg};
generate({call,Name,Params},S) ->
    %% Push all live vars to stack
    YMoves = [{move,{x,R},{y,R}} || {_,R} <- S#s.vars],
    %% Generate all in parameter calculations/calls
    {PHS,_PHSS} = lists:mapfoldl(
		   fun(P,PS) ->
			   {PHS,PHSS,PHSReg} = generate(P,PS),
			   {{PHS,PHSReg},PHSS}
		   end,S,Params),
    %% Get return registers from in parameter calculations/calls
    %% and move them to correct register
    {Regs,_} = lists:mapfoldl(
		 fun({_,Reg},Reg) ->
			 {[],Reg+1};
		    ({_,Reg},N) ->
			 {[{move,{x,Reg},{x,N}}],N+1}
		 end,0,PHS),
    %% Pop all live vars from stack at previous position+1
    YRestores = [{move,{y,R},{x,R+1}} || {_,R} <- S#s.vars],
    %% Update variable references
    NewVars = [{VName,R+1} || {VName,R} <- S#s.vars],
    {[YMoves,[PHS || {PHS,_} <- PHS],
      Regs,{call,Name,length(Params),length(YMoves)},YRestores],
     S#s{ vars = NewVars },0};
generate({variable,V},S) ->
    {[],S,proplists:get_value(V,S#s.vars)};
generate({'if',Test,T,F},S) ->
    %% Generate test instructions
    {TestHS,TestHSS,TestHSReg} = generate(Test,S),
    %% Create the label which to jump at false
    {FalseLabel,LabelS} = next_label(TestHSS),
    %% Generate body for true
    {THS,_THSS,THSReg} = generate(T,LabelS),
    %% Generate body for false
    {FHS,_FHSS,FHSReg} = generate(F,LabelS),
    %% Create label where true and false branch will join again
    {JoinLabel,JoinLabelS} = next_label(LabelS),
    {[TestHS,{brt,{x,TestHSReg},FalseLabel},
      THS,{move,{x,THSReg},{x,TestHSReg}},{jump,JoinLabel},
      {label,FalseLabel},FHS,{move,{x,FHSReg},{x,TestHSReg}},
      {label,JoinLabel}],
     JoinLabelS,TestHSReg};
generate({Op,L,R},S) 
  when Op == add; Op == divide; Op == multiply; Op == subtract; 
       Op == eq; Op == neq; Op == lt; Op == gt ->
    {RHS,RHSS,RHSReg} = generate(R,S),
    {LHS,LHSS,LHSReg} = generate(L,RHSS),
    {Reg,NewS} = next_reg(LHSS),
    %% Remove unused registers
    PrunedS = del_regs(NewS,[LHSReg,RHSReg]),
    %% RHS before LHS in order to not load all constants first
    {[RHS,LHS,{Op,{x,LHSReg},{x,RHSReg},{x,Reg}}],PrunedS,Reg}.
    
next_reg(S = #s{ regs = Regs }) ->
    N = next_reg(Regs),
    {N,S#s{ regs = [N|Regs] }};
next_reg(Regs) ->
    next_reg(lists:sort(Regs),0).
next_reg([N|T],N) ->
    next_reg(T,N+1);
next_reg(_,N) ->
    N.

del_regs(S = #s{ regs = Regs, vars = Vars },RegsToDel) ->
    FilteredRegs = [R || R <- RegsToDel,proplists:get_value(R,Vars,false)],
    S#s{ regs = Regs -- FilteredRegs }.

next_label(S = #s{ labelcnt = N }) ->
    {N,S#s{ labelcnt = N + 1}}.
