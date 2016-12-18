%Practica 3
%B
solve(Balls):- solveAux(Balls,[],R), writeAns(R). 

solveAux(Balls,It,It):- length(Balls,1).
solveAux(Balls,It,R):-  move(Balls,Step,It2), solveAux(It2,[Step|It],R).

move(Balls,[B1,B2],R):-
    member(B1,Balls),
    member(B2,Balls),
    valid(B1,B2,Balls,B3),
    delete(Balls, B1,  BallsAux),
    delete(BallsAux, B2, BallsAux2),
    union([B3],BallsAux2,R).

valid([X1,Y1],[X2,Y2],Balls,[X3,Y3]) :- 
    [X1,Y1] \= [X2,Y2],
    DX is (X2-X1), DY is (Y2-Y1),
    X3 is (X2+DX), Y3 is (Y2+DY),
    posInRange([X3,Y3]),
    \+member([X3,Y3],Balls).

posInRange([X,Y]):- between(1,5,X), between(1,5,Y), X >= Y.

writeAns([]).
writeAns([[B1,B2]|R]):- writeAns(R),write(B1), write(' jumps over '), write(B2), nl.