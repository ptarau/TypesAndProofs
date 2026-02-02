
%% converts sequence to left-heavy implication chain
%% e.g. [a,b,c] -> a->(b->c)
s2i([],End,End).
s2i([X|Xs],Curr,End):-
   s2i(Xs,(Curr->X),End).

%% converts list to implication chain
% also handling special cases of empty and single-element lists
list2impl([],[]).
list2impl([X],X).
list2impl([X,Y|Xs],R):-s2i([Y|Xs],X,R).

%% converts all suffixes of a list to implication chains
list2impls(Xs,Is):-
   sufpref(Xs,[S|Suf]),
   list2impl([S|Suf],Is).

%% generates all suffixes of a list
suff(Xs,Xs).
suff([_|Xs],Ys) :- suff(Xs,Ys).

%% generates all prefixes of a list
pref(_,[]).
pref([X|Xs],[X|Ys]):- pref(Xs,Ys).

%% generates all prefixes of each suffix of a list
sufpref(Xs,[X|Ys]):-
   suff(Xs,Suf),
   pref(Suf,[X|Ys]).

%% converts sentence (seen as a long prolog atom) to implication chain
%% made of words (also prolog atoms)
sent2impl(Sent,R):-
   atomic_list_concat(Words,' ',Sent),
   list2impl(Words,R).



%% converts sentence (seen as a long prolog atom) to implication chain
%% made of words (also prolog atoms)
sent2impls(Sent,R):-
   atomic_list_concat(Words,' ',Sent),
   list2impls(Words,R).

%% converts sentence to Gregori Mints canonical format
%% consisting of nestings not deeper than (a->b)->c
sent2mints(Sent,R):-
   sent2impl(Sent,Impl),
   mints(Impl,R).

%% test showing that an unordered list expressed as conjunction
%% implies the corresponding implication chain (but, obviously, not vice versa)
prove_that_conj_implies_chain:-
   list2impl([a,b,c,d,e],R),write(R),nl,
   Conj=a & b & c & d & e,
   write(Conj),nl,
   iprover(Conj->R), %call intiutionistic prover
   write(yes),nl,
   cprover(R->Conj). % call classical prover

%% show that the left-leaning implication chain
%% implies the right-leaning one
%% which, in fact, can be seen as a Horn clause
prove_relating_right_leaning_chain:-
   list2impl([a,b,c,d,e],Left),
   write(Left),nl,
   Right = (a -> b -> c -> d -> e),
   write(Right),nl,
   iprover(Left->Right), %call intutionistic prover
   write(yes),nl,
   cprover(Left->Right). % call classical prover

%% test converting a sentence to implication formula and printing it
convert_sentence_to_formulas:-
   sent2impls('the cat sits on the mat',R),writeq(R),nl,fail.

%% test showing that Mints's normal form conversion
prove_that_mints_normal_form_also_works:-
  Sent='the cat sits on the mat',
  sent2impl(Sent,Impl),
  writeq(Impl),nl,
  sent2mints(Sent,M),
  writeq(M),nl,
  iprover(Impl->M).

%% test showing that from "the cat sits on the" we can infer "mat"
prove_that_next_word_is_inferred:-
  Sent1='the cat sits on the mat',
  sent2impl(Sent1,Impl1),
  sent2impl('the cat sits on the',Impl2),
  writeq(Impl1),nl,
  writeq(Impl2),nl,
  iprover(Impl2 & mat->Impl1), % just modus ponens !
  iprover(Impl2 -> Impl1 ->mat), % andother way to write modus ponens
  write(mat),nl,
  iprover(a->(a->b)->b). % just modus ponens !

%% converts list of sentences to stored suffixes as implication chains
to_frags(Sents,Impl):-
   member(Sent,Sents),
   sent2impls(Sent,Impl).

store_impls(Sents):-
   retractall(stored_impl(_)),
   distinct(Impl,to_frags(Sents,Impl)),
   assertz(stored_impl(Impl)),
    fail;true.

%% example of stored suffixes
example_stored_suffs :-
   Sents=['the cat sits on the mat',
          'the dog sits on the log',
          'the cat chases the mouse',
          'the dog chases the cat'],
   store_impls(Sents),
   listing(stored_impl/1).

%% completes implication chains using stored implications
complete_all(A,C) :- complete_one(A,B), complete_all1((A->B), C).

%% next step of the completion
complete_all1(B, B).
complete_all1(B, C) :- complete_all(B, C).

%% single step of the completion
complete_one(Query,Next):-
   stored_impl(Query),
   stored_impl(Query->Next).



%% query whether a given sentence suffix is implied by stored suffixes
query_with(Sent):-
   sent2impl(Sent,Impl),
   complete_all(Impl,Next), % modus ponens
   writeq(Impl),write(' entails '),writeq(Next),nl.

%% test querying stored suffixes
test_querying_stored_chains :-
   example_stored_suffs,
   query_with('the cat'),
   query_with('sits on the'),
   query_with('the dog'),

   query_with('the elephant'). % should fail
