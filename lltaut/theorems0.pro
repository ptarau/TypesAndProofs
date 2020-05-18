% clauses of the form: tp(Theorem,ProofTerm).
% preceeded by LaTeX code for Theorem and ProofTerm, as comments

:- op(900, xfy, '-o').

% \Tree [.-o [.X ] [.X ]  ]
% \Tree [.l [.X ] [.X ]  ]
tp(A'-o'A, l(B, B)).
