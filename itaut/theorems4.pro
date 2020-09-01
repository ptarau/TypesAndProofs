% clauses of the form: tp(Theorem,ProofTerm).
% preceeded by LaTeX code for Theorem and ProofTerm, as comments

% \Tree [.$\rightarrow$ [.X ] [.$\rightarrow$ [.Y ] [.$\rightarrow$ [.Z ] [.$\rightarrow$ [.U ] [.U ]  ]  ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.l [.Z ] [.l [.U ] [.U ]  ]  ]  ]  ]
tp((_->_->_->A->A), l(_, l(_, l(_, l(B, B))))).
% \Tree [.$\rightarrow$ [.X ] [.$\rightarrow$ [.Y ] [.$\rightarrow$ [.Z ] [.$\rightarrow$ [.U ] [.Z ]  ]  ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.l [.Z ] [.l [.U ] [.Z ]  ]  ]  ]  ]
tp((_->_->A->_->A), l(_, l(_, l(B, l(_, B))))).
% \Tree [.$\rightarrow$ [.X ] [.$\rightarrow$ [.Y ] [.$\rightarrow$ [.Z ] [.$\rightarrow$ [.U ] [.Y ]  ]  ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.l [.Z ] [.l [.U ] [.Y ]  ]  ]  ]  ]
tp((_->A->_->_->A), l(_, l(B, l(_, l(_, B))))).
% \Tree [.$\rightarrow$ [.X ] [.$\rightarrow$ [.Y ] [.$\rightarrow$ [.Z ] [.$\rightarrow$ [.U ] [.X ]  ]  ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.l [.Z ] [.l [.U ] [.X ]  ]  ]  ]  ]
tp((A->_->_->_->A), l(B, l(_, l(_, l(_, B))))).
% \Tree [.$\rightarrow$ [.X ] [.$\rightarrow$ [.$\rightarrow$ [.X ] [.Y ]  ] [.Y ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.a [.Y ] [.X ]  ]  ]  ]
tp((A->(A->B)->B), l(D, l(C, a(C, D)))).
% \Tree [.$\rightarrow$ [.$\rightarrow$ [.X ] [.Y ]  ] [.$\rightarrow$ [.X ] [.Y ]  ]  ]
% \Tree [.l [.X ] [.l [.Y ] [.a [.X ] [.Y ]  ]  ]  ]
tp(((A->B)->A->B), l(C, l(D, a(C, D)))).
% \Tree [.$\rightarrow$ [.$\rightarrow$ [.$\rightarrow$ [.X ] [.X ]  ] [.Y ]  ] [.Y ]  ]
% \Tree [.l [.X ] [.a [.X ] [.l [.Y ] [.Y ]  ]  ]  ]
tp((((A->A)->B)->B), l(C, a(C, l(D, D)))).
