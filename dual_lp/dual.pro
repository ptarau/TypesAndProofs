:-op(1111,xfx,-:).
:-op(500,fx,~).

/*
buy -: up ; down.

up -: sell_up ; wait_up.
up -: false.

down-: sell_down ; wait_up.

sell_down -: false.
wait_up -: false.

g1:-disprove(buy).
*/


% dual Horn logic interpreter
disprove(false).
disprove((X ; Y)) :- !, disprove(X) , disprove(Y).
disprove(X) :- (X -: Cs), disprove(Cs).

% dual Horn logic interpreter with trace
disprove_(false, []).
disprove_((X ; Y),Fs) :- !, disprove_(X,Xs), disprove_(Y,Ys), append(Xs,Ys,Fs).
disprove_(X,[X|Fs]) :- (X -: Cs), disprove_(Cs,Fs).


% example of dual logic program
is_solvent -: has_cash.

has_cash -: has_sales ; has_investors ; has_credit.

has_investors -: has_venture_investors ; has_institutional_investors.

has_credit -: from_banks ; from_subsidies ; from_founders.

has_venture_investors -: false.
has_institutional_investors -: false.
has_sales -: false.
from_banks -: false.
from_subsidies -: false.
from_founders -: false.

go1:-disprove(is_solvent).


go:-disprove_(is_solvent,Trace),write(disproved_with_trace=Trace),nl;fail.


/*

% TODO: example for security domain:

secure -: has_firewall ; is_disconnected.

p :-q,r.

p v ~q b ~r.


p -: q,r.

~p v q v r


p->q v r
|| classically AND intuitionistically
\/
~q  v ~r -> ~p

?-  T=(p -> q v r),TT=(~p <- ~q & ~r),iprover(T->TT).
?- T=((p->q)->(~q-> ~p)),iprover(T).
T=(~ (q v r)),TT=(~q & ~r),iprover(T<->TT).

% !!! not a BUG, same with fcube/1 and prove/2

?- T= ~(q v r),TT=(~q & ~r),iprover(T<-TT).
T = ~ (q v r),
TT = ~q& ~r.
*/
