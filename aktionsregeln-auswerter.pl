%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-*- mode: prolog -*-
%%%  Auswerter fuer Aktionsregeln
%%%  entspricht Programmen P77+P78+P79 in den Notizen.
%%%
%%%    Regel:
%%%      RegelIdentifikator :: Bedingungen *--> Aktionen
%%%
%%%    Aktion:
%%%      + F          Faktum F hinzufuegen.
%%%      - F          Faktum F loeschen.
%%%      F -> G       Faktum F durch Faktum G ersetzen. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auswertung

:-  op(1200, xfx, :: ),  op(1190, xfx, *--> ),  op(990, xfx, -> ).
:-  dynamic(hat_gefeuert/2). 

reset                         :-  retract_each(id_zaehler(_)),
                                  retract_each(faktum(_,_)),
                                  retract_each(hat_gefeuert(_,_)),
                                  ausgabe_schritt_reset,
                                  init.  %%% siehe Objektprogramm

auswerten(Ziel)               :-  ausgabe_separator, ausgabe_DB('DB initial:'),
  auswertung(Ziel).

auswertung(Ziel)              :-  
  erfuellt(Ziel,ZielID),  !,      ausgabe_erfuellt(ZielID).
auswertung(Ziel)              :-  ausgabe_auswertung(Ziel),
  konfliktmenge(Tripels),         ausgabe_konfliktmenge(Tripels),
  auswahl(Tripel, Tripels),       ausgabe_auswahl(Tripel),
  Tripel = (RID,BID,ASeq),
  durchfuehrung(ASeq),            ausgabe_durchfuehrung(ASeq),
  asserta(hat_gefeuert(RID,BID)), ausgabe_DB('DB nach Durchfuehrung: '), 
  auswertung(Ziel). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Konfliktmenge bestimmen

konfliktmenge(Tripels)        :-  setof( (RID, BID, ASeq),
                                          B^( (RID :: B *--> ASeq), 
                                              erfuellt(B,BID),
                                              \+ hat_gefeuert(RID, BID) ),
                                          Tripels ).

erfuellt( B,            IDs)  :-  erfuellt(B,[],IDs).
erfuellt( (B,C),   IDs, IDsBC):-  !,  erfuellt(B,IDs, IDsB), 
                                      erfuellt(C,IDsB,IDsBC).
erfuellt( \+ B,    IDs, IDs)  :-  !,  \+ erfuellt(B, _, _).
erfuellt( call(G), IDs, IDs)  :-  !,  G.
erfuellt( X  = Y,  IDs, IDs)  :-  !,  X  = Y.
erfuellt( X \= Y,  IDs, IDs)  :-  !,  X \= Y.
erfuellt( B,       IDs, IDsB) :-  faktum(ID,B),  einfuegen(ID,IDs,IDsB).

einfuegen(I, [],        [I]). 
einfuegen(I, [K|R], [I,K|R])  :-  I > K,  !.
einfuegen(I, [K|R], [I,K|R])  :-  I = K,  !.
einfuegen(I, [K|R],   [K|S])  :-  einfuegen(I,R,S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Konfliktaufloesung

auswahl(Tripel, [Tripel])     :-  !.
auswahl(MaxTripel, [T|Ts])    :-  listmax(Ts,T,MaxTripel).

listmax([T|Ts],Akk,MaxTripel) :-  kleiner(T,Akk), !, listmax(Ts,Akk,MaxTripel).
listmax([T|Ts], _, MaxTripel) :-  listmax(Ts,T,MaxTripel).
listmax([],    Akk,Akk      ).

kleiner(Tripel1, Tripel2)     :-  bedID(Tripel1,BID1), bedID(Tripel2,BID2),
                                  BID1 @< BID2.

bedID((_RID,BID,_ASeq), BID).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Durchfuehrung der Aktionen in Regelkoepfen

:-  dynamic(faktum/2),  dynamic(id_zaehler/1). 

durchfuehrung( (A1,A2) )      :-  !, durchfuehrung(A1),  durchfuehrung(A2).
durchfuehrung( +F      )      :-  faktum(_,F), !.                %%% optional
durchfuehrung( +F      )      :-  !, neu_id(ID),  asserta(faktum(ID,F)).
durchfuehrung( -F      )      :-  !, retract_each(faktum(_,F)).  %%% optional
durchfuehrung( -F      )      :-  !, retract(faktum(_,F)). 
durchfuehrung( F->G    )      :-  !, durchfuehrung(-F),  durchfuehrung(+G).
durchfuehrung(Programmaufruf) :-  Programmaufruf.
        
id_zaehler(1).
neu_id(I)                     :-  retract(id_zaehler(I)),  Iplus1 is I+1,
                                  assert(id_zaehler(Iplus1)).

retract_each(X)               :-  retract(X),  fail.
retract_each(_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ausgabe, entweder  ausgabe(an).  oder  ausgabe(aus).

ausgabe(an).

falls_ausgabe_an(_)           :-  ausgabe(aus),  !.
falls_ausgabe_an(PrologGoal)  :-  PrologGoal.

ausgabe_erfuellt(ZielID)      :-  falls_ausgabe_an((
                                    comment_schritt_writeln('Ziel erfuellt:'),
                                    indent_writeln(ZielID),  ausgabe_separator
                                  )).

ausgabe_auswertung(Ziel)      :-  falls_ausgabe_an((
                                    ausgabe_fortsetzung_verlangt,  
                                    ausgabe_separator,  ausgabe_schritt_plus1,
                                    comment_schritt_writeln('auswertung(Ziel). Ziel: '), 
                                    indent_writeln(Ziel)
                                  )).

ausgabe_DB                    :-  ausgabe_DB('DB: ').
ausgabe_DB(X)                 :-  falls_ausgabe_an((
                                    comment_schritt_writeln(X),
                                    each_writeln(hat_gefeuert(_,_)),  nl,
                                    each_writeln(id_zaehler(_)),
                                    each_writeln(faktum(_,_))
                                  )).

ausgabe_konfliktmenge         :-  konfliktmenge(Tripels),
                                  ausgabe_konfliktmenge(Tripels).
ausgabe_konfliktmenge(Tripels):-  falls_ausgabe_an((
                                    comment_schritt_writeln('Konfliktmenge:'),
                                    list_writeln(Tripels)
                                  )).

ausgabe_auswahl               :-  konfliktmenge(Tripels), 
                                  auswahl(Tripel,Tripels),
                                  ausgabe_auswahl(Tripel).
ausgabe_auswahl(Tripel)       :-  falls_ausgabe_an((
                                    comment_schritt_writeln('Auswahl:'),
                                    Tripel = (RID,BID,_),
                                    indent_writeln( (RID,BID) )
                                  )).

ausgabe_durchfuehrung         :-  konfliktmenge(Tripels), 
                                  auswahl(Tripel, Tripels),
                                    Tripel = (_,_,ASeq),
                                  ausgabe_durchfuehrung(ASeq). 
ausgabe_durchfuehrung(ASeq)   :-  falls_ausgabe_an((
                                    comment_schritt_writeln('Durchfuehrung:'),
                                    indent_writeln(ASeq)
                                  )).

ausgabe_fortsetzung_verlangt  :-  falls_ausgabe_an((
                                    comment_schritt_write(' beendet. ', 
                                                          'Weiter (j./n.)? '),
                                    read(j),  nl
                                  )).

ausgabe_separator             :-  falls_ausgabe_an((
                                    separator(S),  writeln(S)
                                  )).

:-  dynamic(ausgabe_schritt/1).
ausgabe_schritt_plus1         :-  falls_ausgabe_an((
                                    retract(ausgabe_schritt(N)),  
                                    Nplus1 is N+1,
                                    assert(ausgabe_schritt(Nplus1))
                                  )).

ausgabe_schritt_reset         :-  retract_each(ausgabe_schritt(_)),
                                  assert(ausgabe_schritt(0)). 
:-  ausgabe_schritt_reset.

schritt_writeln               :-  schritt_write,  nl.
schritt_write                 :-  ausgabe_schritt(N),  write(N).

comment_writeln(X)            :-  comment_write(X),  nl.
comment_write(X)              :-  write('%%% '),  write(X).

comment_schritt_writeln(Y)    :-  comment_schritt_write(Y),  nl.
comment_schritt_write(Y)      :-  comment_schritt_write('. ',Y).

comment_schritt_writeln(X,Y)  :-  comment_schritt_write(X,Y),  nl.
comment_schritt_write(X,Y)    :-  comment_write('Schritt '),
                                  schritt_write,  write(X),  write(Y).

list_writeln(Xs)              :-  member(X,Xs),  indent_writeln(X),  fail.
list_writeln(_).

each_writeln(Goal)            :-  Goal,  indent_writeln(Goal),  fail.
each_writeln(_).

indent_writeln(X)             :-  indent_write(X),  nl.
indent_write(X)               :-  write('    '),  write(X).

separator('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%').
