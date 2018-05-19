new_ctr(ctr(0)).

ctr_inc(Ctr,Inc):-arg(1,Ctr,K),K1 is K+Inc,nb_setarg(1,Ctr,K1).

ctr_inc(Ctr):-ctr_inc(Ctr,1).

ctr_get(Ctr,Val):-arg(1,Ctr,Val).

ctr_set(Ctr,Val):-nb_setarg(1,Ctr,Val).

%% ctr_add(Ctr,X):  adds value X to Ctr
ctr_add(Ctr,N):-arg(1,Ctr,V1),V2 is V1+N,nb_setarg(1,Ctr,V2).

%% ctr_dec(Ctr,X): decrements Ctr
ctr_dec(Ctr):-X is -1,ctr_add(Ctr,X).


% sorts and trims, but in reverse standard order 
revsort(Xs,Rs):-sort(0,(@>),Xs,Rs).

list(F):-atom(F),!,
  forall(
    between(0,24,N),
    list(F/N)
  ).  
list(F/N):-
 functor(C0,F,N),functor(C,F,N),
 once((
   clause(C0,_,Ref0),
   clause_property(Ref0,file(File)),
   clause_property(Ref0,line_count(Line0))
 )),
 file_name_on_path(File,File0),
 maplist(write,
   ['% ',File0,', line ',Line0,': ',F/N]),nl,nl,
 open(File,read,Stream),
 clause(C,_,Ref),
 clause_property(Ref,line_count(Line)),
 read_source_term_at_location(Stream,
   Term,
   [line(Line),variable_names(Vars)]
 ),
 maplist(call,Vars),
 current_output(Out),
 portray_clause(Out,Term,[quoted(false)]),
 fail
 ;true.
 
ll(FN):-list(FN).

:-op(888,fx,ll).


:-op(800,xfx,(for)).

(I<N) for {Goals}:-
	must_be(integer,N),N1 is N-1,
	must_be(var,I),
	must_be(compound,Goals),
  between(0,N1,I),
    call(Goals), % I should parmeterize goal
	fail
; true.


:-op(1025,fy,do).

do(Goal):-
  Goal,
  fail
; true.

:-op(30,xfx,to).

to(I,SN):-succ(N,SN),between(0,N,I).

