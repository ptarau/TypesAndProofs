:-include('compile_clauses.pro').

cautious_buy(X)<=recommended(X),safe(X).

safe(X)<=false:volatile(X).
safe(X)<=false:overvalued(X).
safe(X)<=stable(X).

+recommended(qqq).
+recommended(bitcoin).
+recommended(apple).
+recommended(meta).
+recommended(berkshire).

+stable(att).
+stable(berkshire).

volatile(X) => big_price_changes_last_month(X).

-big_price_changes_last_month(apple).
-big_price_changes_last_month(meta).
-big_price_changes_last_month(comcast).

-overvalued(qqq).
