%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-*- mode: prolog -*-
%%%  Aktionsregeln, Waren eintueten, Objektregeln
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Konfigurationswerte

volumen_tuete(5).
volumen_ware(gross,2).
volumen_ware(klein,1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Die Daten repraesentieren Waren, z.B. ware(gross, flasche(wasser))
% Die Waren liegen auf dem Band an der Kasse und sollen in Tueten
% verpackt werden. Die initialen Daten enthalten kontext(start).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Mit komplettieren anfangen

r_kontextwechsel(komplettieren)
::  kontext(start)
*-->  
    kontext(start)->kontext(komplettieren).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% komplettieren: Wer Tee kauft, bekommt Kandis dazu geschenkt

r_komplettieren(klein,kandis)
::  kontext(komplettieren), ware(_,tee), \+ ware(_,kandis)  
*-->  
    +ware(klein,kandis).

r_kontextwechsel(gross)
::  kontext(komplettieren)
*-->  
    kontext(komplettieren)->kontext(gross).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gross: grosse Waren einpacken, Flaschen zuerst

r_gross_flasche_in_tuete(T,gross,flasche(X))
::  kontext(gross), ware(gross,_), tuete(T,hatplatz(V)), ware(gross,flasche(X)),
    call( hatplatz(T,V,gross,Vneu) )
*-->  
    -ware(gross,flasche(X)), +in(T,gross,flasche(X)), 
    tuete(T,hatplatz(V))->tuete(T,hatplatz(Vneu)).

r_gross_in_tuete(T,gross,X)
::  kontext(gross), ware(gross,X), tuete(T,hatplatz(V)),
    call( hatplatz(T,V,gross,Vneu) )
*-->  
    -ware(gross,X), +in(T,gross,X), 
    tuete(T,hatplatz(V))->tuete(T,hatplatz(Vneu)).

r_gross_neuetuete(T)
::  kontext(gross), ware(gross,_),
    call( neuetuete(T,V) )
*--> 
    +tuete(T,hatplatz(V)).

r_kontextwechsel(klein)
::  kontext(gross)
*-->  
    kontext(gross)->kontext(klein).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% klein: kleine Waren dazupacken, bevorzugt in Tueten ohne Flaschen

r_klein_in_tuete_ohne_flasche(T,klein,X)
::  kontext(klein), ware(klein,X), tuete(T,hatplatz(V)), \+ in(T,_,flasche(_)),
    call( hatplatz(T,V,klein,Vneu) )
*-->  
    -ware(klein,X), +in(T,klein,X), 
    tuete(T,hatplatz(V))->tuete(T,hatplatz(Vneu)).

r_klein_in_tuete(T,klein,X)
::  kontext(klein), ware(klein,X), tuete(T,hatplatz(V)),
    call( hatplatz(T,V,klein,Vneu) )
*-->  
    -ware(klein,X), +in(T,klein,X), 
    tuete(T,hatplatz(V))->tuete(T,hatplatz(Vneu)).

r_klein_neuetuete(T)
::  kontext(klein), ware(klein,_),
    call( neuetuete(T,V) )
*--> 
    +tuete(T,hatplatz(V)).

r_kontextwechsel(stop)
::  kontext(klein)
*-->  
    kontext(klein)->kontext(stop).

% r_testregel_tuete1_erneuern
% ::  kontext(gross), \+ ware(gross,_), tuete(1,hatplatz(V)), \+ tuete1_ist_neu
% *-->  
%     tuete(1,hatplatz(V))->tuete(1,hatplatz(V)), +tuete1_ist_neu.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prolog-Prozeduren, die mit call aufgerufen werden.

:-  dynamic(tueten_zaehler/1). 
tueten_zaehler(1).

neuetuete(T,V)         :-  retract(tueten_zaehler(T)),  Tplus1 is T+1,
                           assert(tueten_zaehler(Tplus1)),
                           volumen_tuete(V).

hatplatz(_,V,G, Vneu)  :-  volumen_ware(G,W),  Vneu is V-W,  Vneu >= 0.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   :-  reset, auswerten( kontext(stop) ).
