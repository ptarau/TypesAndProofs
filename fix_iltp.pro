fix_iltp:-
  Dirs=[
  'Problems/SYJ/'-'probs/SYJ/',
  'Problems/SYN/'-'probs/SYN/',
  'Problems/LCL/'-'probs/LCL/'
  ],
  atom_codes('.',[DOT]),
  do((
  member(In-Out,Dirs),
  directory_files(In,Fs),  
  member(F0,Fs),
  %ppp(F0),
  atom_codes(F0,[C|_]),C=\=DOT,
  atom_concat(In,F0,InF),
  atomic_list_concat([Out,F0,'l'],OutF),
  %ppp(InF-OutF),
  clean_cp(InF,OutF)
  )).
  
  
clean_cp(InF,OutF):-
  atom_codes('|v=-',[Bar,V,E,L]),
  open(InF,read,S),
  read_stream_to_codes(S,Codes),
  close(S),
  tell(OutF),
  do((
    member(C,Codes),
    ( C =:= Bar-> D=V 
    ; C =:= E -> D=L
    ; D=C
    ),
    put(D)
  )),
  told,
  true
  .