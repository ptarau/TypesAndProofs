metaint([]).        % no more goals left, succeed
metaint([G|Gs]):-   % unify the first goal with the head of a clause
   cls([G|Bs],Gs),  % build a new list of goals from the body of the  
                    % clause extended with the remaining goals as tail
   metaint(Bs).     % interpret the extended body 


metaint_with_trace([],[]).        % no more goals left, succeed
metaint_with_trace([G|Gs],[G|Ts]):-   % unify the first goal with the head of a clause
   cls([G|Bs],Gs),  % build a new list of goals from the body of the
                    % clause extended with the remaining goals as tail
   metaint_with_trace(Bs,Ts).     % interpret the extended body



cls([  add(0,X,X)                         |Tail],Tail).                   
cls([  add(s(X),Y,s(Z)), add(X,Y,Z)       |Tail],Tail). 
cls([  goal(R), add(s(s(0)),s(s(0)),R)    |Tail],Tail). 


go:-
  metaint_with_trace([goal(R)],Ts),
  write(R+Ts),nl,
  fail.
  
  
% same without metainterpretation

add(0,X,X).
add(s(X),Y,s(Z)):-add(X,Y,Z).

goal(R):-add(s(s(0)),s(s(0)),R).
