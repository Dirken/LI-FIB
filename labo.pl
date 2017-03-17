%%1. Escribe un predicado prolog "flatten" que aplana listas:
%%?- aplana( [a,[b,c,[b,b],e], f], L). 			
%%L = [a,b,c,b,b,e,f]
%%Escribe otro que elimina las repeticiones:
%%?- flattenNoRepetitions( [a,[b,c,[b,b],e], f], L).
%%L = [a,b,c,e,f]

aplana([],[]).

aplana([L|LS], Res):- 	
	aplana(L, R1),
	aplana(LS, R2),
	append(R1, R2, Res).
aplana(L, [L]).

flattenNoRepetitions([],[]).
flattenNoRepetitions(L, Res):-
	aplana(L,L1),
	eliminarRepetits(L1, Res).

eliminarRepetits([],[]).
eliminarRepetits([Ini|Rest], Res):- member(Ini, Rest),!, eliminarRepetits(Rest, Res).
eliminarRepetits([Ini|Rest], [Ini|Res]):- eliminarRepetits(Rest, Res).

