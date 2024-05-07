metaint([]).        % no more goals left, succeed
metaint([G|Gs]):-   % unify the first goal with the head of a clause
   cls([G|Bs],Gs),  % build a new list of goals from the body of the  
                    % clause extended with the remaining goals as tail
   metaint(Bs).     % interpret the extended body 


% falsifiable rules
cls([ credit, from_banks, from_subsidies, from_founders |Tail],Tail).
cls([ investors, venture, institutional |Tail],Tail).
cls([ cash, sales, investors, credit |Tail],Tail).
cls([ solvent, cash |Tail],Tail).

% false clauses, eg. venture ... means venture ==> false.
cls([ venture |Tail],Tail).
cls([ institutional |Tail],Tail).
cls([ sales |Tail],Tail).
cls([ from_banks |Tail],Tail).
cls([ from_subsidies |Tail],Tail).
cls([ from_founders |Tail],Tail).


metaint_with_trace([],[]).        % no more goals left, succeed
metaint_with_trace([G|Gs],[G|Ts]):-   % unify the first goal with the head of a clause
   cls([G|Bs],Gs),  % build a new list of goals from the body of the
                    % clause extended with the remaining goals as tail
   metaint_with_trace(Bs,Ts).     % interpret the extended body


go1:-
  metaint([solvent]),
  write('successfully falsified'),nl,
  fail.


go:-
  metaint_with_trace([solvent],Ts),
  write('successfully falsified'+Ts),nl,
  fail.
  


/*
?- go.
successfully falsified+[solvent,cash,sales,investors,venture,institutional,credit,from_banks,from_subsidies,from_founders]

?- go1.
successfully falsified
*/
