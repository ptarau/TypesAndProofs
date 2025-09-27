%a => b ; c.

%b => f.
%c => f.

%-f.

% p <= q, false:a.

% q <= false:b.


b ; c :-a.
f :- b.
f :- c.

-f.

a ; -a.


p :- q, -a.
q :- -b.

b ; -b.




