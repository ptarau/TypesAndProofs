% clauses of the form: tp(Theorem,ProofTerm).
% preceeded by LaTeX code for Theorem and ProofTerm, as comments

:- op(900, xfy, '-o').

% \Tree [.-o [.X ] [.-o [.-o [.X ] [.Y ]  ] [.-o [.-o [.Y ] [.Z ]  ] [.Z ]  ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.l [.Z ] [.a [.Z ] [.a [.Y ] [.X ]  ]  ]  ]  ]  ]
tp(A'-o'(A'-o'B)'-o'(B'-o'C)'-o'C, l(F, l(E, l(D, a(D, a(E, F)))))).
% \Tree [.-o [.-o [.X ] [.Y ]  ] [.-o [.X ] [.-o [.-o [.Y ] [.Z ]  ] [.Z ]  ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.l [.Z ] [.a [.Z ] [.a [.X ] [.Y ]  ]  ]  ]  ]  ]
tp((A'-o'B)'-o'A'-o'(B'-o'C)'-o'C, l(E, l(F, l(D, a(D, a(E, F)))))).
% \Tree [.-o [.X ] [.-o [.-o [.Y ] [.Z ]  ] [.-o [.-o [.X ] [.Y ]  ] [.Z ]  ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.l [.Z ] [.a [.Y ] [.a [.Z ] [.X ]  ]  ]  ]  ]  ]
tp(A'-o'(B'-o'C)'-o'(A'-o'B)'-o'C, l(F, l(D, l(E, a(D, a(E, F)))))).
% \Tree [.-o [.-o [.X ] [.Y ]  ] [.-o [.-o [.Y ] [.Z ]  ] [.-o [.X ] [.Z ]  ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.l [.Z ] [.a [.Y ] [.a [.X ] [.Z ]  ]  ]  ]  ]  ]
tp((B'-o'A)'-o'(A'-o'C)'-o'B'-o'C, l(E, l(D, l(F, a(D, a(E, F)))))).
% \Tree [.-o [.-o [.X ] [.Y ]  ] [.-o [.Z ] [.-o [.-o [.Z ] [.X ]  ] [.Y ]  ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.l [.Z ] [.a [.X ] [.a [.Z ] [.Y ]  ]  ]  ]  ]  ]
tp((B'-o'C)'-o'A'-o'(A'-o'B)'-o'C, l(D, l(F, l(E, a(D, a(E, F)))))).
% \Tree [.-o [.-o [.X ] [.Y ]  ] [.-o [.-o [.Z ] [.X ]  ] [.-o [.Z ] [.Y ]  ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.l [.Z ] [.a [.X ] [.a [.Y ] [.Z ]  ]  ]  ]  ]  ]
tp((A'-o'C)'-o'(B'-o'A)'-o'B'-o'C, l(D, l(E, l(F, a(D, a(E, F)))))).
% \Tree [.-o [.X ] [.-o [.Y ] [.-o [.-o [.Y ] [.-o [.X ] [.Z ]  ]  ] [.Z ]  ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.l [.Z ] [.a [.a [.Z ] [.Y ]  ] [.X ]  ]  ]  ]  ]
tp(B'-o'A'-o'(A'-o'B'-o'C)'-o'C, l(F, l(E, l(D, a(a(D, E), F))))).
% \Tree [.-o [.X ] [.-o [.Y ] [.-o [.-o [.X ] [.-o [.Y ] [.Z ]  ]  ] [.Z ]  ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.l [.Z ] [.a [.a [.Z ] [.X ]  ] [.Y ]  ]  ]  ]  ]
tp(A'-o'B'-o'(A'-o'B'-o'C)'-o'C, l(E, l(F, l(D, a(a(D, E), F))))).
% \Tree [.-o [.X ] [.-o [.-o [.Y ] [.-o [.X ] [.Z ]  ]  ] [.-o [.Y ] [.Z ]  ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.l [.Z ] [.a [.a [.Y ] [.Z ]  ] [.X ]  ]  ]  ]  ]
tp(A'-o'(B'-o'A'-o'C)'-o'B'-o'C, l(F, l(D, l(E, a(a(D, E), F))))).
% \Tree [.-o [.X ] [.-o [.-o [.X ] [.-o [.Y ] [.Z ]  ]  ] [.-o [.Y ] [.Z ]  ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.l [.Z ] [.a [.a [.Y ] [.X ]  ] [.Z ]  ]  ]  ]  ]
tp(A'-o'(A'-o'B'-o'C)'-o'B'-o'C, l(E, l(D, l(F, a(a(D, E), F))))).
% \Tree [.-o [.-o [.X ] [.-o [.Y ] [.Z ]  ]  ] [.-o [.Y ] [.-o [.X ] [.Z ]  ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.l [.Z ] [.a [.a [.X ] [.Z ]  ] [.Y ]  ]  ]  ]  ]
tp((B'-o'A'-o'C)'-o'A'-o'B'-o'C, l(D, l(F, l(E, a(a(D, E), F))))).
% \Tree [.-o [.-o [.X ] [.-o [.Y ] [.Z ]  ]  ] [.-o [.X ] [.-o [.Y ] [.Z ]  ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.l [.Z ] [.a [.a [.X ] [.Y ]  ] [.Z ]  ]  ]  ]  ]
tp((A'-o'B'-o'C)'-o'A'-o'B'-o'C, l(D, l(E, l(F, a(a(D, E), F))))).
% \Tree [.-o [.X ] [.-o [.-o [.-o [.-o [.X ] [.Y ]  ] [.Y ]  ] [.Z ]  ] [.Z ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.a [.Y ] [.l [.Z ] [.a [.Z ] [.X ]  ]  ]  ]  ]  ]
tp(A'-o'(((A'-o'B)'-o'B)'-o'C)'-o'C, l(F, l(D, a(D, l(E, a(E, F)))))).
% \Tree [.-o [.-o [.X ] [.Y ]  ] [.-o [.-o [.-o [.X ] [.Y ]  ] [.Z ]  ] [.Z ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.a [.Y ] [.l [.Z ] [.a [.X ] [.Z ]  ]  ]  ]  ]  ]
tp((A'-o'B)'-o'((A'-o'B)'-o'C)'-o'C, l(E, l(D, a(D, l(F, a(E, F)))))).
% \Tree [.-o [.-o [.-o [.X ] [.X ]  ] [.Y ]  ] [.-o [.-o [.Y ] [.Z ]  ] [.Z ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.a [.Y ] [.a [.X ] [.l [.Z ] [.Z ]  ]  ]  ]  ]  ]
tp(((A'-o'A)'-o'B)'-o'(B'-o'C)'-o'C, l(E, l(D, a(D, a(E, l(F, F)))))).
% \Tree [.-o [.-o [.-o [.-o [.X ] [.Y ]  ] [.Y ]  ] [.Z ]  ] [.-o [.X ] [.Z ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.a [.X ] [.l [.Z ] [.a [.Z ] [.Y ]  ]  ]  ]  ]  ]
tp((((B'-o'A)'-o'A)'-o'C)'-o'B'-o'C, l(D, l(F, a(D, l(E, a(E, F)))))).
% \Tree [.-o [.-o [.-o [.X ] [.Y ]  ] [.Z ]  ] [.-o [.-o [.X ] [.Y ]  ] [.Z ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.a [.X ] [.l [.Z ] [.a [.Y ] [.Z ]  ]  ]  ]  ]  ]
tp(((A'-o'B)'-o'C)'-o'(A'-o'B)'-o'C, l(D, l(E, a(D, l(F, a(E, F)))))).
% \Tree [.-o [.-o [.X ] [.Y ]  ] [.-o [.-o [.-o [.Z ] [.Z ]  ] [.X ]  ] [.Y ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.a [.X ] [.a [.Y ] [.l [.Z ] [.Z ]  ]  ]  ]  ]  ]
tp((B'-o'C)'-o'((A'-o'A)'-o'B)'-o'C, l(D, l(E, a(D, a(E, l(F, F)))))).
% \Tree [.-o [.X ] [.-o [.-o [.-o [.Y ] [.Y ]  ] [.-o [.X ] [.Z ]  ]  ] [.Z ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.a [.a [.Y ] [.l [.Z ] [.Z ]  ]  ] [.X ]  ]  ]  ]
tp(B'-o'((A'-o'A)'-o'B'-o'C)'-o'C, l(F, l(D, a(a(D, l(E, E)), F)))).
% \Tree [.-o [.X ] [.-o [.-o [.X ] [.-o [.-o [.Y ] [.Y ]  ] [.Z ]  ]  ] [.Z ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.a [.a [.Y ] [.X ]  ] [.l [.Z ] [.Z ]  ]  ]  ]  ]
tp(A'-o'(A'-o'(B'-o'B)'-o'C)'-o'C, l(E, l(D, a(a(D, E), l(F, F))))).
% \Tree [.-o [.-o [.-o [.X ] [.X ]  ] [.-o [.Y ] [.Z ]  ]  ] [.-o [.Y ] [.Z ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.a [.a [.X ] [.l [.Z ] [.Z ]  ]  ] [.Y ]  ]  ]  ]
tp(((A'-o'A)'-o'B'-o'C)'-o'B'-o'C, l(D, l(F, a(a(D, l(E, E)), F)))).
% \Tree [.-o [.-o [.X ] [.-o [.-o [.Y ] [.Y ]  ] [.Z ]  ]  ] [.-o [.X ] [.Z ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.a [.a [.X ] [.Y ]  ] [.l [.Z ] [.Z ]  ]  ]  ]  ]
tp((B'-o'(A'-o'A)'-o'C)'-o'B'-o'C, l(D, l(E, a(a(D, E), l(F, F))))).
% \Tree [.-o [.-o [.-o [.X ] [.-o [.-o [.X ] [.Y ]  ] [.Y ]  ]  ] [.Z ]  ] [.Z ]  ]
% \Tree [.l [.X ] [.a [.X ] [.l [.Y ] [.l [.Z ] [.a [.Z ] [.Y ]  ]  ]  ]  ]  ]
tp(((A'-o'(A'-o'B)'-o'B)'-o'C)'-o'C, l(D, a(D, l(F, l(E, a(E, F)))))).
% \Tree [.-o [.-o [.-o [.-o [.X ] [.Y ]  ] [.-o [.X ] [.Y ]  ]  ] [.Z ]  ] [.Z ]  ]
% \Tree [.l [.X ] [.a [.X ] [.l [.Y ] [.l [.Z ] [.a [.Y ] [.Z ]  ]  ]  ]  ]  ]
tp((((A'-o'B)'-o'A'-o'B)'-o'C)'-o'C, l(D, a(D, l(E, l(F, a(E, F)))))).
% \Tree [.-o [.-o [.-o [.-o [.-o [.X ] [.X ]  ] [.Y ]  ] [.Y ]  ] [.Z ]  ] [.Z ]  ]
% \Tree [.l [.X ] [.a [.X ] [.l [.Y ] [.a [.Y ] [.l [.Z ] [.Z ]  ]  ]  ]  ]  ]
tp(((((A'-o'A)'-o'B)'-o'B)'-o'C)'-o'C, l(D, a(D, l(E, a(E, l(F, F)))))).
% \Tree [.-o [.-o [.-o [.X ] [.X ]  ] [.-o [.-o [.Y ] [.Y ]  ] [.Z ]  ]  ] [.Z ]  ]
% \Tree [.l [.X ] [.a [.a [.X ] [.l [.Y ] [.Y ]  ]  ] [.l [.Z ] [.Z ]  ]  ]  ]
tp(((A'-o'A)'-o'(B'-o'B)'-o'C)'-o'C, l(D, a(a(D, l(E, E)), l(F, F)))).
