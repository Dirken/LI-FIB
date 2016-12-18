%%Problema A.1
nat(0).
nat(N):- nat(N2), N is N2+1.

camino(E,E, C,C).
camino(EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal):-
    unPaso(EstadoActual, EstSiguiente),
    \+member(EstSiguiente, CaminoHastaAhora),
    camino(EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal).

solucionOptima:-
    nat(N),
    camino([0,0],  [0,4],  [[0,0]],  C),
    length(C,N),
    write('Coste: '), write(N), nl,
    write(C).
    
%Llenar los cubos
unPaso([C1,_],[C1,8]).
unPaso([_,C1],[5,C1]).
%vaciar un cubo del todo
unPaso([C1,_], [C1,0]).
unPaso([_,C2], [0,C2]).
%Se puede verter el contenido de un cubo en otro:
unPaso([C1,C2],[C11,C22]):- C22 is min(C2 + C1, 8), C11 is C1 - (C22 - C2).
unPaso([C1,C2],[C11,C22]):- C11 is min(C1 + C2, 5), C22 is C2 - (C11 - C1). 