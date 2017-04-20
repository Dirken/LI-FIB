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


%%2. Tenemos una fila de cinco casas, con cinco vecinos con casas de colores diferentes, y cinco 
%%profesiones, animales, bebidas y nacionalidades diferentes, y sabiendo que:

%%     1 - El que vive en la casa roja es de Peru
%%     2 - Al frances le gusta el perro
%%     3 - El pintor es japones
%%     4 - Al chino le gusta el ron
%%     5 - El hungaro vive en la primera casa
%%     6 - Al de la casa verde le gusta el coñac
%%     7 - La casa verde esta a la izquierda de la blanca
%%     8 - El escultor cría caracoles
%%     9 - El de la casa amarilla es actor
%%    10 - El de la tercera casa bebe cava
%%    11 - El que vive al lado del actor tiene un caballo
%%    12 - El hungaro vive al lado de la casa azul
%%    13 - Al notario la gusta el whisky
%%    14 - El que vive al lado del medico tiene un ardilla,

%% Escribe un programa Prolog que averigue para cada persona todas sus 
%% caracteristicas de la forma [num_casa,color,profesion,animal,bebida,pais] 
%% averiguables. Ayuda: sigue el siguiente esquema:

%% casas:-	Sol = [	[1,A1,B1,C1,D1,E1],
%% 		[2,A2,B2,C2,D2,E2],
%% 		[3,A3,B3,C3,D3,E3],
%% 		[4,A4,B4,C4,D4,E4],
%% 		[5,A5,B5,C5,D5,E5] ],
%%         member(  ... , Sol),
%%         ...
%% 	write(Sol), nl.

lado(N, N1) :-  N1 is N - 1.
lado(N, N1) :-  N1 is N + 1.
imprimirSol([]).
imprimirSol([Sol|Sols]):-  write(Sol), nl, imprimirSol(Sols).

casas:-	  Sol = [ [1,A1,B1,C1,D1,E1],
                  [2,A2,B2,C2,D2,E2],
                  [3,A3,B3,C3,D3,E3],
                  [4,A4,B4,C4,D4,E4],
                  [5,A5,B5,C5,D5,E5]
                ],

% member( [ n, c, p, a, b, p ]  , Sol),
member([ _, "roja", _, _, _, "Perú" ],Sol),
member([ _, _, _, "perro", _, "Francia" ],Sol),
member([ _, _, "pintor", _, _, "Japón" ],Sol),
member([ _, _, _, _, "ron", "China" ],Sol),
member([ 1, _, _, _, _, "Hungría" ],Sol),
member([ _, "verde", _, _, "coñac", _ ],Sol),
member([ N1, "verde", _, _, _, _ ],Sol),
N11 is N1 + 1,
member([ N11, "blanca", _, _, _, _ ],Sol),
member([ _, _, "escultor", "caracol", _, _ ],Sol),
member([ _, "amarilla", "actor", _, _, _ ],Sol),
member([ 3, _, _, _, "cava", _ ],Sol),
member([ N2, _, "actor", _, _, _ ],Sol),
lado(N2, N22),
member([ N22, _, _, "caballo", _, _ ],Sol),
member([ N3, "azul", _, _, _, _ ],Sol),
lado(N3, N33),
member([ N33, _, _, _,_, "Hungría" ],Sol),
member([ _, _, "notario", _, "whisky", _ ],Sol),
member([ N4, _, "medico", _, _, _ ],Sol),
lado(N4, N44),
member([ N44, _, _, "ardilla", _, _ ],Sol),

imprimirSol(Sol), 
nl.


/*3. En el lenguaje de programacion "sumbol" un programa tiene la siguiente sintaxis:

<programa>      -->    begin  <instrucciones>  end

<instrucciones>   -->    <instruccion>
<instrucciones>   -->    <instruccion> ; <instrucciones>  

<instruccion>     -->    <variable> = <variable> + <variable>
<instruccion>     -->    if <variable> = <variable> then <instrucciones> 
         else <instrucciones>  endif
<variable>    -->    x
<variable>    -->    y
<variable>    -->    z

Tres ejemplos de programas sumbol:
  begin x=x+z end
  begin x=x+y; z=z+z; x=y+x end
  begin x=y+z; if z=z then x=x+z; y=y+z else z=z+y endif; x=x+z end 

Escribe en Prolog un sencillo analizador sintactico para el lenguaje
sumbol, es decir, que se comporte así:

?- programa( [begin, z, =, x, +, y, end] ).
yes
?- programa( [begin, z, =, x, +, y, ;, x, =, z, z, end] ).
no

(en el segundo ejemplo falta un "+").  
Para ello, haz una clausula Prolog para cada regla de la gramatica, usando appends para
separar las partes de la entrada (obligatorio).
*/



%programa([begin,x,=,y,+,z,;,x,=,z,+,y,end]).
%programa( [begin, if, z, =, x,then, z, =, x, +, y, else, x, =, y, +, z, endif, end]).
programa([]) :- write("yes").
programa([begin|L]):- append(LS,[end],L), instruccion(LS).

variable(x).
variable(y).
variable(z).

instruccion([]) :- write("yes").
instruccion([X,=,Y,+,Z]):- variable(X), variable(Y), variable(Z).
instruccion(L):- append(LS,[;|LS2],L), instruccion(LS), instruccion(LS2).
instruccion([if|L]):- 
  append(LS,[endif], L), 
  append(LS2,[then|LS3], LS), 
  comp(LS2), 
  append(LS4, [else|LS5], LS3), 
  instruccion(LS4), 
  instruccion(LS5), !,
  write("yes"). 
comp([X, =, Y]):- variable(X), variable(Y).



