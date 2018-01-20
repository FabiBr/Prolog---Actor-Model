%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-*- mode: prolog -*-
%%%  Aktionsregeln, Waren eintueten, Daten 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hinter der Kasse auf dem Band liegt, was der Kunde gekauft hat

init  :-  
          assert( faktum(6,  ware(gross, brot)               ) ),
          assert( faktum(5,  ware(gross, milch)              ) ),
          assert( faktum(4,  ware(klein, joghurt)            ) ),
          assert( faktum(3,  ware(klein, flasche(cola))      ) ),
          assert( faktum(2,  ware(gross, flasche(wasser))    ) ),
          assert( faktum(1,  ware(klein, tee)                ) ),
          assert( faktum(0,  kontext(start)                  ) ),

          assert( id_zaehler(11)                               ),
          retract(tueten_zaehler(_)),  assert(tueten_zaehler(1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Nicht init aufrufen, sondern reset. reset verwendet init.
%%%   :-  reset, auswerten( kontext(stop) ).
