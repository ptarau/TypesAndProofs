fix_iltp:-
  Dirs=[
  'Problems/SYJ/'-'probs/SYJ/',
  'Problems/SYN/'-'probs/SYN/',
  'Problems/LCL/'-'probs/LCL/'
  ],
  atom_codes('.',[DOT]),
  do((
  member(In-Out,Dirs),
  directory_files(In,Fs),  
  member(F0,Fs),
  %ppp(F0),
  atom_codes(F0,[C|_]),C=\=DOT,
  atom_concat(In,F0,InF),
  atomic_list_concat([Out,F0,'l'],OutF),
  %ppp(InF-OutF),
  clean_cp(InF,OutF)
  )).
  
  
clean_cp(InF,OutF):-
  atom_codes('|v=-',[Bar,V,E,L]),
  open(InF,read,S),
  read_stream_to_codes(S,Codes),
  close(S),
  tell(OutF),
  do((
    member(C,Codes),
    ( C =:= Bar-> D=V 
    ; C =:= E -> D=L
    ; D=C
    ),
    put(D)
  )),
  told,
  true
  .

probs2py1:-
  prob:consult('test_data/iltp.pro'),
  tell('test_data/iltp.txt'),
  do((
      prob:iltp(N, TF0, FName,  Form),
      form2tuple(Form,PyForm),
      ( TF0=false->TF='False'
      ; TF0=true->TF='True'
      ; ppp(unextected=TF0),TF=TF0
      ),
      %R=[N,TF,FName,PyForm],
      write('['),
      write(N),write(','),
      write(TF),write(','),
      writeq(FName),write(','),
      writeq(PyForm),write(']'),
      nl   
  )),
  told.

probs2py:-
  prob:consult('test_data/iltp.pro'),
  tell('test_data/iltp.txt'),
  do((
      prob:iltp(N, TF0, FName,  Form),
      form2stack(Form,PyForm),
      ( TF0=false->TF='False'
      ; TF0=true->TF='True'
      ; ppp(unextected=TF0),TF=TF0
      ),
      write('['),
      write(N),write(','),
      write(TF),write(','),
      writeq(FName),write(','),
      writeq(PyForm),write(']'),
      nl
  )),
  told.
 
form2stack(T,Xs):-form2stack(T,Xs,[]).

form2stack(T)-->{compound(T)},!,
  {T=..[F|Xs],atom_string(F,SF)},
  forms2stack(Xs),
  [SF].
form2stack(T)-->{atom_string(T,ST)},
  [ST].

forms2stack([])-->[].
forms2stack([X|Xs])-->
  form2stack(X),
  forms2stack(Xs).
  
save_probs:-
  tell('test_data/iltp.pro'),
  maplist(portray_clause,[
      (:- op(425,  fy,  ~ )),
      (:- op(450, xfy,  & )),   
      (:- op(475, xfy,  v )),  
      (:- op(500, xfx,  <-> ))
  ]),nl,
  listing(prob:iltp),
  told.
  
load_probs:-
  prob:retractall(iltp(_,_,_,_)),
  init(fcount),

  atom_codes('.',[Dot]),
  directory_files(probs,Dirs),
  findall(F,
    (
      member(Dir0,Dirs),atom_codes(Dir0,[D|_]),D=\=Dot,
      atom_concat('probs/',Dir0,Dir),
      directory_files(Dir,Fs),
      member(F0,Fs),
      atom_codes(F0,[C|_]),C=\=Dot,
      atomic_list_concat([Dir,'/',F0],F)
    ),
    Fs0
  ),
  sort(Fs0,Fs),
  
  do((
    member(InF,Fs),   
    atom_codes(InF,[C|_]),C=\=Dot,  
    load_prob(InF,_GVs)
  )).
  
test_probs(Prover):-time(test_probs(nonvar,Prover)).

ensure_loaded_probs:-
  ( prob:clause(iltp(_,_,_,_),true) -> true 
  ; F='test_data/iltp.pro',exists_file(F)->prob:consult(F)
  ; load_probs,save_probs
  ).
  
test_probs(Filter,Prover):-  
  ensure_loaded_probs,
  
  pred_count(prob:iltp(_,_,_,_),Len),
  max_time(M),ppp(problems_time_out_in_secs=M),nl,
  new_ctr(Refuted),new_ctr(Wrong),new_ctr(TOut),new_ctr(Err),
  new_ctr(Skip),
  
  do((    
    prob:iltp(K,Theo,InF,GVs),
    %ppp(filtering=GVs),
    ( call(Filter,GVs)->call_prover(Prover,GVs,Res)
    ; 
      % ppp(Filter=failed),
      Res=not_apply
    ),
    ( member(Res,[true,false])->
       ( Res==Theo->ppp(K:InF=ok(res=Res)),(Res=false->ctr_inc(Refuted);true)
       ; ctr_inc(Wrong),ppp(K:InF=wrong(got=Res,should_be=Theo))
       )
    ; Res=timeout(_)->ctr_inc(TOut),ppp(K:InF=is(Res)+should_be(Theo))
    ; Res=not_apply->ctr_inc(Skip)
    ; ctr_inc(Err),ppp(K:InF=is(Res)+should_be(Theo))
    )
  )),
  ctr_get(Refuted,RK),
  ctr_get(TOut,TK),
  ctr_get(Wrong,WK),
  ctr_get(Err,EK),
  ctr_get(Skip,SK),
  Right is Len-SK-TK-WK-EK,
  Proven is Right-RK,
  Tried is Right + TK,
  nl,
  ppp([
    prover=Prover,total=Len,
    skipped=SK,tried=Tried:[right=Right:[proven=Proven,refuted=RK],wrong=WK,
    timed_out(secs,M)=TK,error=EK]
  ]).

is_par(fpprove).

call_prover(Prover,FullG,R):-is_par(Prover),!,call(Prover,FullG,R).  
call_prover(Prover,FullG,R):- 
  max_time(MaxTime),
  call_prover(MaxTime,Prover,FullG,R).
   
call_prover(MaxTime,Prover,FullG,R):-   
   (
     timed_call(MaxTime,call(Prover,FullG),Exc) ->
     (number(Exc) -> R=true ; R=Exc)
   ; R=false
   ).  
  
test_on(K,Prover):-
  max_time(MaxTime),
  test_on(MaxTime,K,Prover).
  
test_on(MaxTime,K,Prover):-
  ensure_loaded_probs,
  prob:iltp(K,Theo,F,G),
  %ppt(G),
  ppp(file(K)=F),
  ppp(G),nl,
  ppp(expected_to_be=Theo),nl,
  time(call_prover(MaxTime,Prover,G,Res),Time),
  ppp(result=Res),ppp(time=Time).

  
show_test(K):-
  ensure_loaded_probs,
  prob:iltp(K,Theo,F,G),
  ppp(file______________(K)=F),
  ppp(should_be=Theo),
  ppp(G),nl.
  
par_test_on(K,Prover):-
  ensure_loaded_probs,
  prob:iltp(K,Theo,F,G),
  ppp(file______________(K)=F),
  %ppp(G),nl,
  call(Prover,G,Res),
  ppp(expected_to_be=Theo),
  ppp(result=Res),nl.
  
  
load_prob(InF,(G:-Vs)):-
   is_theorem(InF,Theo),
   file2db(InF),
   findall(A,
     (prob:fof(_,Axiom,A),Axiom\==conjecture),
   Vs),
   prob:fof(_,conjecture,G0),
   ( G0=($true)->G=(a->a)
   ; G0=($false)->G=false
   ; G=G0
   ),
   unexpand(Vs,G,Goal),
   total(fcount,K),
   assertz(prob:iltp(K,Theo,InF,Goal)),
   inc(fcount).

   
   
file2terms(F,Ts,[]):-
  read_file_to_terms(F,Ts,[]).

f2c:-is_theorem('probs/SYN391+1.pl',X),ppp(X).

is_theorem(F,true):-
  atom_codes('% Status (intuit.) : Theorem',True),
  file2comment(F,Cs),
  append(True,_,Cs),
  !.
is_theorem(_,false).  


file2comment(F,Cs):-
  atom_codes('%',[Perc]),
  file2lines(F,Ls),
  member(Cs,Ls),
  Cs=[Perc|_].
  

% reads in a file as a list of lines
file2lines(F,Ls):-
  open(F,read,S),
  get_lines(S,Ls),
  close(S).

% line to list reader helper
get_lines(S,[]):-at_end_of_stream(S),!.
get_lines(S,[Codes|Ls]):-
  read_line_to_codes(S,Codes),
  get_lines(S,Ls).
    
file2db(F):-Db=prob,
  Db:retractall(fof(_,_,_)),
  file2terms(F,Ts,_),
  do(( member(T,Ts),
    ( T=':-'(Cmd)->call(Db:Cmd)
    ; assertz(Db:T)
    )
  )).

% adaptors for othe operator sets



%map_operator(f,R)-->{!,R=false}.

map_operators(A,B):-map_operator([
  (#)-(box),
  (*)-(diam),
  (&)-(,),
  (v)-(;),
  (<->)-(<=>),
   (->)-(=>)
],A,B).

map_operator(_,A,R):-atomic(A),!,R=A.
map_operator(Ps,A,B):-
 A=..[F|Xs],
 member(F-G,Ps),
 !,
 maplist(map_operator(Ps),Xs,Ys),
 B=..[G|Ys].
map_operator(Ps,A,B):-
 A=..[F|Xs],
 maplist(map_operator(Ps),Xs,Ys),
 B=..[F|Ys]. 
 
 