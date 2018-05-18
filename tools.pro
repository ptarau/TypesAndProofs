new_ctr(ctr(0)).

ctr_inc(Ctr,Inc):-arg(1,Ctr,K),K1 is K+Inc,nb_setarg(1,Ctr,K1).

ctr_inc(Ctr):-ctr_inc(Ctr,1).

ctr_get(Ctr,Val):-arg(1,Ctr,Val).

ctr_set(Ctr,Val):-nb_setarg(1,Ctr,Val).

%% ctr_add(Ctr,X):  adds value X to Ctr
ctr_add(Ctr,N):-arg(1,Ctr,V1),V2 is V1+N,nb_setarg(1,Ctr,V2).

%% ctr_dec(Ctr,X): decrements Ctr
ctr_dec(Ctr):-X is -1,ctr_add(Ctr,X).
