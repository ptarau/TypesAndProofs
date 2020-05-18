% clauses of the form: tp(Theorem,ProofTerm).
% preceeded by LaTeX code for Theorem and ProofTerm, as comments

:- op(900, xfy, '-o').

% \Tree [.-o [.X ] [.-o [.-o [.X ] [.Y ]  ] [.Y ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.a [.Y ] [.X ]  ]  ]  ]
tp(A'-o'(A'-o'B)'-o'B, l(D, l(C, a(C, D)))).
% \Tree [.-o [.-o [.X ] [.Y ]  ] [.-o [.X ] [.Y ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.a [.X ] [.Y ]  ]  ]  ]
tp((A'-o'B)'-o'A'-o'B, l(C, l(D, a(C, D)))).
% \Tree [.-o [.-o [.-o [.X ] [.X ]  ] [.Y ]  ] [.Y ]  ]
% \Tree [.l [.X ] [.a [.X ] [.l [.Y ] [.Y ]  ]  ]  ]
tp(((A'-o'A)'-o'B)'-o'B, l(C, a(C, l(D, D)))).
