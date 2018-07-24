% implicational Harrop formulas (all other connectives removed)

/*
derived from:

g(e)-->[].
g((D->G))-->z,d(D),g(G).

d(e)-->[].
d((G->e))-->z,g(G).

||
\/

dhar((G->e))-->har(G).

har(e)-->[].
har((e->G))-->z,har(G).
har(((H->e)->G))-->z,z,har(H),har(G).

% A086246 - variant of Motzkin
% counts=[1,1,1,2,4,9,21,51,127,323,835,2188,5798]
%ratios=[1,1,2,2,2.25,2.33,2.42,2.49,2.54,2.58,2.62,2.64]
*/


hdef(N,F,Vs):-harrop_definite(N,F,Vs),natpartitions(Vs).

hgoal(N,F,Vs):-harrop_goal(N,F,Vs),natpartitions(Vs).

harrop_definite(N,Form,Vs):-harrop_definite(Form,Vs,[],N,0).
harrop_goal(N,Form,Vs):-harrop_goal(Form,Vs,[],N,0).

harrop_definite((G->V),[V|Vs1],Vs2)-->harrop_goal(G,Vs1,Vs2).

harrop_goal(V,[V|Vs],Vs)-->[].
harrop_goal((V->G),[V|Vs1],Vs2)-->z,harrop_goal(G,Vs1,Vs2).
harrop_goal(((H->V)->G),[V|Vs1],Vs3)-->z,z,
  harrop_goal(H,Vs1,Vs2),
  harrop_goal(G,Vs2,Vs3).

z(SN,N):-succ(N,SN).
