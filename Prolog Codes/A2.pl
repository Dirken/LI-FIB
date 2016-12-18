%Problema A.2

pasos(15).

camino(E,E, C,C,_).
camino(EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal,Limite):-
    unPaso(EstadoActual, EstSiguiente, Limite),
    \+member(EstSiguiente, CaminoHastaAhora),
    pasos(P),
    length(CaminoHastaAhora,L),
    L =< P,
    camino(EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal,Limite).

solucionOptima:-
    camino([1,1],  [0,0],  [[1,1]],  C, 6),
    pasos(P),
    length(C,P),
    write('Coste: '), write(P), nl,
    write(C).
   
%Movimientos arriba derecha
unPaso([X,Y],[XF, YF],Limite):- 0 =< (Y-2), (X+1) < Limite, XF is (X+1), YF is (Y-2).
%Movimientos arriba izquierda
unPaso([X,Y],[XF, YF],_):- 0 =< (Y-2), 0 =< (X-1), XF is (X-1), YF is (Y-2).
%Movimientos derecha izquierda
unPaso([X,Y],[XF, YF],Limite):- 0 =< (Y-1), (X+2) < Limite, XF is (X+2), YF is (Y-1).
%Movimientos derecha derecha
unPaso([X,Y],[XF, YF],Limite):- (X+2) < Limite, (Y+1) < Limite, XF is (X+2), YF is (Y+1).
%Movimientos abajo derecha
unPaso([X,Y],[XF, YF],Limite):- (X+1) < Limite, (Y+2) < Limite, YF is (Y+2), XF is (X+1).
%Movimientos abajo izquierda
unPaso([X,Y],[XF, YF],Limite):- 0 =< (X-1), (Y+2) < Limite, YF is (Y+2), XF is (X-1).
%Movimientos izquierda izquierda
unPaso([X,Y],[XF, YF],_):- 0 =< (X-2), 0 =< (Y-1), YF is (Y-1), XF is (X-2).
%Movimientos izquierda derecha
unPaso([X,Y],[XF, YF],Limite):- 0 =< (X-2), (Y+1) < Limite, YF is Y+1, XF is X-2.