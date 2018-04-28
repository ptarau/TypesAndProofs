import random

# Remy's algorithm - Knuth algorithm R in 7.2.1.6
def ranBin0(N) :
  L = [None] * (2*N+1)
  L[0],n = 0 , 0
  while (n<N) :
    X=random.randrange(0,4*n+2)
    n += 1
    b, k, nn = X % 2, X // 2, 2*n
    L[nn-b] = nn   
    L[nn-1+b] = L[k]
    L[k] = nn-1
  return L  

def links2bin(k,L) :
  if 0 == k % 2 : return ()
  else : return links2bin(L[k],L),links2bin(L[k+1],L)   

# random unlabeled binary tree
def ranBin(N) :
  L = ranBin0(N)
  return links2bin(L[0],L)
  
# >>> ranBin(7)
# (((), (((), ()), ())), (((), ()), ((), ())))

