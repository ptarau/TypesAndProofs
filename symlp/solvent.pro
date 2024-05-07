:-include('compile_clauses.pro').

bankrupt(X)<=false:is_solvent(X).

% example of dual logic program
is_solvent(X) => has_cash(X).

has_cash(X) =>
  has_sales(X) ;
  has_investors(X) ;
  has_credit(X).

has_investors(X) =>
   has_venture_investors(X) ;
   has_institutional_investors(X).

has_credit(X) =>
  has_money_from_banks(X) ;
  has_money_from_subsidies(X) ;
  has_money_from_founders(X).

has_venture_investors(alice) => false.
has_venture_investors(bob) => false.

has_institutional_investors(alice) => false.
has_institutional_investors(bob) => false.

has_sales(alice) => false.

has_money_from_banks(alice) => false.
has_money_from_banks(bob) => false.

has_money_from_subsidies(alice) => false.
has_money_from_subsidies(bob) => false.

has_money_from_founders(alice) => false.
has_money_from_founders(bob) => false.


/*

?- true:bankrupt(X).
X = alice.

*/
