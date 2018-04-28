import timeit
import gs

'''
% derived form this Prolog verion:

add_new(X,Xs,Ys):-memberchk(X,Xs),!,Ys=Xs.
add_new(X,Xs,[X|Xs]).

ljp(A,Vs):-memberchk(A,Vs),!.
ljp((A->B),Vs1):-!,add_new(A,Vs1,Vs2),ljp(B,Vs2). 
ljp(G,Vs1):- % atomic(G),
  select((A->B),Vs1,Vs2),
  ljp_imp(A,B,Vs2),
  !,
  add_new(B,Vs2,Vs3),
  ljp(G,Vs3).

ljp_imp(A,_,Vs):-atomic(A),!,memberchk(A,Vs).   
ljp_imp((C->D),B,Vs1):-
   add_new((D->B),Vs1,Vs2),
   ljp((C->D),Vs2).
'''

def prove(G) : return next(ljp(G,None),False)

def ljp(G,Vs1) :
  if memb(G,Vs1) :
    yield True
  elif not isVar(G) :
    A,B = G
    for R in ljp(B,addNew(A,Vs1))  : yield R 
  else : # isVar(G)
    for V,Vs2 in allFirstsFrom(Vs1) :
      if not isVar(V) :
        A,B=V
        if ljp_imp(A,B,Vs2) :
          Vs3 = addNew(B,Vs2)
          for R in ljp(G,Vs3) : yield R
 
def ljp_imp(A,B,Vs1) :
  if isVar(A) :
    yield memb(A,Vs1)
  else :
    C,D=A
    Vs2 = add_new((D,B),Vs1)
    for R in ljp((C,D),Vs2) : yield R

def allFirstsFrom(Xs) :
  if not Xs is None :
    X,Ys = Xs
    yield (X,Ys)
    for (Z,Zs) in allFirstsFrom(Ys) :
      yield (Z,(X,Zs))
     
def addNew(X,Xs) : 
  if memb(X,Xs) :
    return Xs
  else :
    return (X,Xs)
  
def isVar(i) : return isinstance(i,int)

def memb(X,Xs) :
 if Xs is None :
   return False
 else :
   Y,Ys=Xs
   if X == Y :
     return True
   else :
     return memb(X,Ys)
 
# tests 

def bmf1(f,n) :
 start_time = timeit.default_timer()
 f(n)
 end_time=timeit.default_timer()
 print('time = ',end_time - start_time)  
 
def bmf0(g) :
 start_time = timeit.default_timer()
 res=g
 end_time=timeit.default_timer()
 print('time = ',end_time - start_time)  
 print('res = ',res)

def bmf2(f,x,y) :
 start_time = timeit.default_timer()
 res=f(x,y)
 print(res)
 end_time=timeit.default_timer()
 print('time = ',end_time - start_time)  
 print('res = ',res)
 
 
g=addNew(1,addNew(5,addNew(0,addNew(3,None)))) 

k=(0,(1,0))

s=((0,(1,2)),((0,1),(0,2)))

a=((0,(1,1)),((0,1),(0,2)))

def go() :
  for g in gs.gs: 
    print(g)
    print(prove(g))
 
def proveGs() :
  succ=0
  fail=0
  for g in gs.gs: 
    if(prove(g)) :
      succ+=1; 
    else :
      fail+=1
  return succ,fail      
 

  
    
  