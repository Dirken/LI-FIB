%Practica 3
%C
numNutrients(8).
product(milk,[2,4,6]).
product(meat,[1,8]).
product(bread,[3,5,7]).

shopping(K,L):- shoppingAux(K,[],[],L).

shoppingAux(K,L,LN, R):- numNutrients(N), length(LN,N), L = R.
shoppingAux(K,L,LN, R):- 
	length(L,D),
	K > D,
	product(P,N), 
	\+member(P,L), 
	union(LN,N,N2),
	shoppingAux(K,[P|L],N2,R).