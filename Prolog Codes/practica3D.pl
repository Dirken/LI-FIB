%Practica 3 D
cities([1,2,3,4]).
road(1,2, 10).  % road between cities 1 and 2 of 10km
road(1,4, 20).
road(2,3, 25).
road(3,4, 12).

mainRoads(K,M):- cities(C), member(A, C), mainRoadsAux(K,[],[A],M).

mainRoadsAux(K,M,V,R):- 
	sumaK(M, K2), 
	K >= K2, 
	cities(C), 
	subset(C,V), 
	subset(V,C),
	R = M.
mainRoadsAux(K,M,V,R):- 
	sumaK(M, K2), 
	K > K2, 
	road(C1,C2,D), 
	xor(member(C1,V), 
	member(C2, V)),
	\+member([C1,C2,D],M),
	union([C1], V, V1),
	union([C2], V1, CVActualizadas),
	mainRoadsAux(K,[[C1,C2,D]|M], CVActualizadas, R).	

writeAns([]).
writeAns([R|L]):-  writeAns(L), write(R), nl.

and(A,B):- A,B.
or(A,B):- A;B.
xor(A,B):- \+and(A,B), or(A,B). 

sumaK([],0).
sumaK([[_,_,R]|M], K):- sumaK(M, K2), K is K2+R.	


