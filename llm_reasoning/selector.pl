% selector.pl

select_completion(Pairs, BestId) :-
    longest_token(Pairs, _, BestId).

longest_token([Id-Token], Token, Id).
longest_token([Id1-T1, Id2-T2 | Rest], BestToken, BestId) :-
    string_length(T1, L1),
    string_length(T2, L2),
    ( L1 >= L2 ->
        longest_token([Id1-T1 | Rest], BestToken, BestId)
    ;   longest_token([Id2-T2 | Rest], BestToken, BestId)
    ).
