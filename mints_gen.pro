:-ensure_loaded('allPartitions.pro').

mints_positive(P)-->[P].                    
mints_positive((P->Q))-->[P,Q].             
mints_positive((P->(Q->R)))-->[P,Q,R].             
mints_positive((P->Q)->R)-->[P,Q,R].

pos_mints_conjuncts([])-->[].
pos_mints_conjuncts([F|Fs])-->mints_positive(F),pos_mints_conjuncts(Fs).

mints_gen(N,(G:-Ms)):-
  length(Xs,N),
  natpartitions(Xs),
  [G|Ys]=Xs, % 
  memberchk(G,Ys), % prevent trivial no
  pos_mints_conjuncts(Ms0,Ys,[]),
  no_dups_in(Ms0,Ms),
  \+ memberchk(G,Ms). % prevent trivial yes
  
mints_show(N):-
  mints_gen(N,T),
  portray_clause(T),
  fail.
  
no_dups_in(Xs,Ys):-
    sort(Xs,Ys),
    length(Xs,L),
    length(Ys,L).    

mints_count(N,Ks):-
  findall(K,
  (
  between(1,N,I),
  findall(X,mints_gen(I,X),Xs),
  length(Xs,K)
  ),
  Ks).