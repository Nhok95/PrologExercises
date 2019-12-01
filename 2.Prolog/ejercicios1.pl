%pert(X,L)
pert(X,[X|_]).
pert(X,[_|L]):-pert(X,L).

pert_con_resto(X,L,R):- 
    concat(L1,[X|L2],L), concat(L1,L2,R).  

concat([],L,L).
concat([X|L1],L2,[X|L3]):- concat(L1,L2,L3).

permutacion([],[]).
permutacion(L,[X|P]) :- pert_con_resto(X,L,R), permutacion(R,P).


%2 (dif1)
%prod(L,P). P es el producto de los elementos de la lista de enteros dada L
prod([X],X).
prod([X|L],P):-
    prod(L,P1),
    P is X*P1.

prod2([X|L],P):- prod(L,P1), P is P1*X.
prod2([],1).


%3 (dif1)
%pescalar(L1,L2,P) P es el producto escalar de los dos vectores L1 y L2.
pescalar([X|L1],[Y|L2],P):-
    pescalar(L1,L2,P1),
    P is P1+X*Y.   
pescalar([],[],0).


%4 (dif2)
%intersección y union (suponiendo listas ordenadas)
interseccion([],_,[]). 
interseccion([X|L1], L2, [X|LF]):-  
    pert(X, L2),!,  %X se encuentra en L1 y L2, entonces se añade a LF y se eliminan el resto de ramificaciones
    interseccion(L1,L2,LF). %recursividad
interseccion([_|L1],L2,LF):-
    interseccion(L1,L2,LF).

union([],L2,L2).
union([X|L1],L2, LF):-
    pert(X, L2), !, %si X pertenece a L2 significa que esta repetido, por tanto no se añade
    union(L1,L2,LF). %recursividad
union([X|L1],L2, [X|LF]):- %se añade X a LF ya que sabemos que no esta repetido
    union(L1,L2,LF).

%5 (dif2)
%ultimo y inverso usando concat
ultimo(L,U):- concat(_,[U],L).

inverso([],[]).
inverso(L,[X|L1]):- concat(L2,[X],L), inverso(L2,L1).
 

%6 (dif3)
%fib(N, F) F es el N-ésimo número de Fibonacci para la N dada
fib(0, 0):-!.
fib(1, 1):-!.
fib(N, F):- 
    N1 is N-1, N2 is N-2, 
    fib(N2, F2), fib(N1, F1), 
    F is F1+F2. 
    
    
fib2(1,1).
fib2(2,1).
fib2(N,F):-
    N > 2,
    N1 is N-1,
    N2 is N-2,
    fib2(N1,F1), fib2(N2,F2), F is F1+F2.

%7 (dif3)
%dados(P,N,L) la lista L expresa una manera de sumar P puntos lanzando N dados

dados(0,0,[]).
dados(P,N,[X|L]):- N > 0,
    pert(X,[1,2,3,4,5,6]),
    P1 is P-X,
    N1 is N-1,
    dados(P1,N1,L). 

%aux
%suma(L,S) Dada una lista L, devuelve la suma de todos sus elementos en S
suma([],0).
suma([X|L],S):- suma(L, S1), S is S1+X.

%8 (dif2)
%suma_demas(L) Dada una lista de enteros L, se satisface si existe algún elemento 
%              en L que es igual a la suma de los demas elementos de L

suma_demas(L):- 
    concat(L1,[X|L2],L), concat(L1,L2,LS),
    suma(LS,X), !.
    
suma_demas2(L):- 
    pert_con_resto(X,L,R), 
    suma(R,X), !. % si encontramos uno basta

%9 (dif 2)
%suma_ants(L) Dada una lista de enteros L, se satisface, si existe algún elemento
%             en L que es igual a la suma de los elementos anteriores a él en L.

suma_ants(L):-
    concat(L1,[X|_],L), suma(L1,X), !.

%10 (dif 2)
%card(L) Dada una lista de enteros L, escriba la lista que, para cada elemento de L,
%        dice cuántas veces aparece este elemento en L


car([],[]).
car([X|L], [ [X,N1] |Cr] ):- 
    car(L,C),
    pert_con_resto([X,N],C,Cr), !,
    N1 is N+1.
car([X|L], [ [X,1] |C] ):-
    car(L,C).
  
card(L):-car(L,C), write(C).


%11 (dif 2)
%esta_ordenada(L) la lista L de números enteros está ordenada de menor a mayor

esta_ordenada([X,Y]):- X<Y, !.
esta_ordenada([X,Y|L]):- X<Y, esta_ordenada([Y|L]).

%12 (dif 2)
%ordenacion(L1,L2) L2 es la lista de enteros L1 ordenada de menor a mayor  

ordenacion(L1,L2):- permutacion(L1,L2), esta_ordenada(L2), !.


%14 (dif 3)
% insercion(X,L1,L2) L2 es la lista obtenida al insertar X en su sitio en la lista de enteros 
%                    L1 que está ordenada de menor a mayor”.

insercion(X,[],[X]).
insercion(X,[Y|L1],[Y|L2]):- 
    X >= Y,
    insercion(X,L1,L2).
insercion(X,[Y|L1],[X,Y|L1]):-
    X < Y.

% ordenación2(L1,L2) basado en el método de la inserción

ordenacion2([],[]).
ordenacion2([X|L1],L2):- ordenacion2(L1,Laux), insercion(X,Laux,L2).

    
%16 (dif 3)
% ordenacion3(L1,L2) merge sort
ordenacion3([],[]):- !.
ordenacion3([X],[X]):- !.
ordenacion3(L1,L2):- 
    concat2(M1,M2,L1),
    ordenacion3(M1,Laux1),
    ordenacion3(M2,Laux2),
    merge(Laux1,Laux2,L2).

concat2([],[],[]).
concat2([],[X],[X]).
concat2([X|L1],[Y|L2],[X,Y|L3]):- concat2(L1,L2,L3).
    
merge(L1,[],L1):-!.
merge([],L2,L2):-!.
merge([X|L1],[Y|L2],[X|L3]):- 
    X < Y,
    merge(L1,[Y|L2],L3).
merge([X|L1],[Y|L2],[Y|L3]):- merge([X|L1],L2,L3).


%17 (dif 4)
% diccionario(A,N) Dado un alfabeto A de sı́mbolos y un natural N,
%                  escriba todas las palabras de N sı́mbolos, por
%                  orden alfabético (el orden alfabético es según el alfabeto A dado). 
% nmembers(A,N,L) utiliza el pert para obtener una lista L de N

diccionario(A,N):- nmembers(A,N,L), write_dic(L), fail.
diccionario(_,_).

nmembers(_,0,[]):-!.
nmembers(A,N,[X|L]):- 
    pert(X,A),
    N1 is N-1,
    nmembers(A,N1,L).

write_dic([]):- write(' '), nl, !. 
write_dic([X|L]):-
    write(X),
    write_dic(L).

%18 (dif 3)
% palindromos(L) dada una lista de letras L, escriba todas las permutaciones
%               de sus elementos que sean palindromos (capicúas).

%v1
palindromos(L):- 
    permutacion(L,L1),
    es_palindromo(L1),
    write(L1), nl, fail.
palindromos(_). 

es_palindromo(L):-
    reverse(L,L).    
  
%v2
palindromos2(L):- 
  permutacion(L,P), 
  es_palindromo(P), 
  write(P), nl, fail. 
palindromos2(_). 

es_palindromo2([]).
es_palindromo2([_]) :- !. % regla adecuada
es_palindromo2([X|L]) :- concat(L1,[X],L), es_palindromo2(L1). 

% Si no queremos que escriba repetidos se puede usar setof. 
palindroms(L) :- 
    setof(P,(permutation(L,P), 
    es_palindromo(P)),S),
    write(S). 


%19 (dif 4) 
% ¿Qué 8 dígitos diferentes tenemos que asignar a las letras
% S,E,N,D,M,O,R,Y, de manera que se cumpla la suma SEND+MORE=MONEY? Re-
% suelve el problema en Prolog con un predicado suma que sume listas de digitos.
% El programa debe decir “no” si no existe solución.


suma([],[],[],C,C).
suma([X1|L1],[X2|L2],[X3|L3],Cin,Cout) :-
	X3 is (X1 + X2 + Cin) mod 10,
	C  is (X1 + X2 + Cin) //  10,
	suma(L1,L2,L3,C,Cout).


send_more_money1 :-

	L = [S, E, N, D, M, O, R, Y, _, _],
	permutacion(L, [0,1,2,3,4,5,6,7,8,9]),
	suma([D, N, E, S], [E, R, O, M], [Y, E, N, O], 0, M),

	write('S = '), write(S), nl,
	write('E = '), write(E), nl,
	write('N = '), write(N), nl,
	write('D = '), write(D), nl,
	write('M = '), write(M), nl,
	write('O = '), write(O), nl,
	write('R = '), write(R), nl,
	write('Y = '), write(Y), nl,
	write('  '), write([S,E,N,D]), nl,
	write('  '), write([M,O,R,E]), nl,
	write('-------------------'), nl,
	write([M,O,N,E,Y]), nl.


send_more_money2 :-

	L = [0,1,2,3,4,5,6,7,8,9],
	pert_con_resto(M,  [0,1], _),
	pert_con_resto(M,  L,  L0),
	pert_con_resto(O, L0, L1),
	pert_con_resto(R, L1, L2),
	pert_con_resto(Y, L2, L3),
	pert_con_resto(S, L3, L4),
	pert_con_resto(E, L4, L5),
	pert_con_resto(N, L5, L6),
	pert_con_resto(D, L6, _),
	suma([D, N, E, S], [E, R, O, M], [Y, E, N, O], 0, M),

	write('S = '), write(S), nl,
	write('E = '), write(E), nl,
	write('N = '), write(N), nl,
	write('D = '), write(D), nl,
	write('M = '), write(M), nl,
	write('O = '), write(O), nl,
	write('R = '), write(R), nl,
	write('Y = '), write(Y), nl,
	write('  '), write([S,E,N,D]), nl,
	write('  '), write([M,O,R,E]), nl,
	write('-------------------'), nl,
	write([M,O,N,E,Y]), nl.
    

%20 (dif 4)
simplifica(D,D1):- 
    unpaso(D,D2),!,
    simplifica(D2,D1).
simplifica(D,D).

unpaso(A+B,A+C):- unpaso(B,C),!.
unpaso(B+A,C+A):- unpaso(B,C),!.
unpaso(A*B,A*C):- unpaso(B,C),!.
unpaso(B*A,C*A):- write('B = '), unpaso(B,C),!.
unpaso(0*_,0):-!.
unpaso(_*0,0):-!.
unpaso(1*X,X):-!.
unpaso(X*1,X):-!.
unpaso(0+X,X):-!.
unpaso(X+0,X):-!.
unpaso(N1+N2,N3):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(N1*N2,N3):- number(N1), number(N2), N3 is N1*N2,!.
unpaso(N1*X+N2*X,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(N1*X+X*N2,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(X*N1+N2*X,N3*X):- number(N1), number(N2), N3 is N1+N2,!.
unpaso(X*N1+X*N2,N3*X):- number(N1), number(N2), N3 is N1+N2,!.

    