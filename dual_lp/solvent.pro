:-include('compile_dual.pro').

% example of dual logic program
is_solvent -: has_cash.

has_cash -: has_sales ; has_investors ; has_credit.

has_investors -: has_venture_investors ; has_institutional_investors.

has_credit -: from_banks ; from_subsidies ; from_founders.

has_venture_investors -: false.
has_institutional_investors -: false.
has_sales -: false.
from_banks -: false.
from_subsidies -: false.
from_founders -: false.
