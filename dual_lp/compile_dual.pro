:- multifile(term_expansion/2).

:-op(1199,xfx,(-:)).
:-op(666,fx,(~)).

compile_dual(C,_):-var(C),!,fail.
compile_dual((H-:B),R):-nonvar(H),nonvar(B),dual2clause((H-:B),R).
compile_dual((~H),R):-nonvar(H),dual2clause((~H),R).


dual2clause((H-:false),false:(H:-true)):-!.
dual2clause((H-:B),false:(H:-CB)):-disj2conj(B,CB).
dual2clause((~H),false:(H)).

disj2conj((A;B),(CA,CB)):-nonvar(A),nonvar(B),!,
    disj2conj(A,CA),
    disj2conj(B,CB).

disj2conj(A,A).

test:-
   Cs=[(
     a-:b;c;d
   ),
   (
     b -: false
   ),
   (
     c -: false
   ),
   (
     ~d
   )
   ],
   maplist(compile_dual,Cs,Rs),
   forall(member(R,Rs),print(R)).

show:-listing(false:_).

% calling in the dual

false(X):-false:X.

% dual Horn clause program

term_expansion(C,R) :- compile_dual(C,R).

a -: b ; c ; d.
a -: e.

d -: c,e.

b -: false.
c -: false.
~ e.


