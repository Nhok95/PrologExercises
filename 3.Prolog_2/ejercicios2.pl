pert(X,[X|_]).
pert(X,[_|L]):-pert(X,L).

pert_con_resto(X,L,R):- 
    concat(L1,[X|L2],L), concat(L1,L2,R).  

concat([],L,L).
concat([X|L1],L2,[X|L3]):- concat(L1,L2,L3).

permutacion([],[]).
permutacion(L,[X|P]) :- pert_con_resto(X,L,R), permutacion(R,P).


%1. Escribe un predicado prolog "flatten" que aplana listas:

flatten([],[]).
flatten([X|L],L2):-
    is_list(X),
    flatten(X,LL),
    flatten(L,LM),
    concat(LL,LM,L2),!.
flatten([X|L],L2):-
    flatten(L,LL),
    L2 = [X|LL].    

%Escribe otro que elimina las repeticiones:

eraseRep([],[]).
eraseRep([X|L],L2):-
    pert(X,L),!,
    eraseRep(L,L2).
eraseRep([X|L],[X|L2]):-
    eraseRep(L,L2).
    
%flattenNoRepetitions([],[]).
flattenNoRepetitions(L,L2):-
    flatten(L,LL), eraseRep(LL,L2).
    

% 2. Tenemos una fila de cinco casas, con cinco vecinos con casas de colores 
% diferentes, y cinco profesiones, animales, bebidas y nacionalidades diferentes,  % y sabiendo que:

% casas       --> [1,2,3,4,5]
% colores     --> [rojo,verde,blanca,amarilla]
% profesiones --> [pintor,escultor,actor,notario,medico]
% animales    --> [perro,caracoles,caballo,ardilla]
% bebidas     --> [ron,cava,whisky,]
% pais        --> [peru,francia,japones,chino,hungaro]

% 1 - El que vive en la casa roja es de Peru          --> [_,rojo,_,_,_,peruano],
% 2 - Al frances le gusta el perro                    --> [_,_,_,perro,_,frances]
% 3 - El pintor es japones                            --> [_,_,pintor,_,_,japones]
% 4 - Al chino le gusta el ron                        --> [_,_,_,_,ron,chino]
% 5 - El hungaro vive en la primera casa              --> [1,_,_,_,_,hungaro]
% 6 - Al de la casa verde le gusta el coñac           --> [_,verde,_,_,coñac,_]
% 7 - La casa verde esta a la izquierda de la blanca  --> Vec_Izq [_,verde,_,_,_,_],[_,blanca,_,_,_,_]
% 8 - El escultor cría caracoles                      --> [_,_,escultor,caracoles,_,_]
% 9 - El de la casa amarilla es actor                 --> [_,amarilla,actor,_,_,_]
% 10 - El de la tercera casa bebe cava                --> [3,_,_,_,cava,_]
% 11 - El que vive al lado del actor tiene un caballo --> Vecinos [_,_,actor,_,_,_],[_,_,_,caballo,_,_]  
% 12 - El hungaro vive al lado de la casa azul        --> Vecinos [_,azul,_,_,_,_],[_,_,_,_,_,hungaro] 
% 13 - Al notario la gusta el whisky                  --> [_,_,notario,_,whisky,_]
% 14 - El que vive al lado del medico tiene un ardilla--> Vecino [_,_,medico,_,_,_],[_,_,_,ardilla,_,_] 


% Escribe un programa Prolog que averigue para cada persona todas sus 
% caracteristicas de la forma [num_casa,color,profesion,animal,bebida,pais] 
% averiguables. Ayuda: sigue el siguiente esquema:

v_izq([I|[D|L]],V_Izq,V_Der):- 
    (I = V_Izq,D = V_Der);    %OR
    v_izq([D|L],V_Izq,V_Der). %recursivo
    
vecino([P|L],Pista_1,Pista_2):-
    v_izq([P|L],Pista_1,Pista_2);   %OR
    v_izq([P|L],Pista_2,Pista_1).
    
write_sol([P|L]):-
    write("["),
    write_fila(P),
    write("]"),
    nl,
    write_sol(L),!.
write_sol([]):-!.


write_fila([P,L]):-
    var(P),
    var(L),
    write("_,_"),!.
write_fila([P,L]):-
    var(P),
    nonvar(L),
    write("_,"),
    write(L),!.
write_fila([P,L]):-
    nonvar(P),
    var(L),
    write(P),
    write("_"),!.
write_fila([P,L]):-
    nonvar(P),
    nonvar(L),
    write(P),
    write(","),
    write(L),!.
write_fila([P|L]):-
    nonvar(P),
    write(P), 
    write(","), 
    write_fila(L).
write_fila([P|L]):-
    var(P),
    write("_,"),
    write_fila(L).


  % [1,amarilla,actor,ardilla,_,hungaro]
  % [2,azul,medico,caballo,ron,chino]
  % [3,rojo,escultor,caracoles,cava,peruano]
  % [4,verde,pintor,_,conac,japon]
  % [5,blanca,notario,perro,whisky,frances]

casas:- Sol = [ [1,_,_,_,_,_],
                [2,_,_,_,_,_],
                [3,_,_,_,_,_],
                [4,_,_,_,_,_],
                [5,_,_,_,_,_] ],
        L1 = [_,rojo,_,_,_,peruano],
        L2 = [_,_,_,perro,_,frances],
        L3 = [_,_,pintor,_,_,japones],
        L4 = [_,_,_,_,ron,chino],
        L5 = [1,_,_,_,_,hungaro],
        L6 = [_,verde,_,_,conac,_],
        L7 = [_,_,escultor,caracoles,_,_],
        L8 = [_,amarilla,actor,_,_,_],
        L9 = [3,_,_,_,cava,_],
        L10= [_,_,notario,_,whisky,_],

    member(L1, Sol),member(L2, Sol),
    member(L3, Sol),member(L4, Sol),
    member(L5, Sol),member(L6, Sol),
    member(L7, Sol),member(L8, Sol),
    member(L9, Sol),member(L10, Sol),
    v_izq(Sol,[_,verde,_,_,_,_],[_,blanca,_,_,_,_]),
    vecino(Sol,[_,_,actor,_,_,_],[_,_,_,caballo,_,_]),
    vecino(Sol,[_,azul,_,_,_,_],[_,_,_,_,_,hungaro]),
    vecino(Sol,[_,_,medico,_,_,_],[_,_,_,ardilla,_,_]),

    write_sol(Sol), nl.

% 3. Haz un programa prolog que escriba la manera de colocar sobre un tablero de
%    ajedrez ocho reinas sin que éstas se ataquen entre sí.
%    Por ejemplo, ésta sería una solucion:
%          
%       . . x . . . . .
%       . . . . . x . .
%       . . . x . . . .
%       . x . . . . . .
%       . . . . . . . x
%       . . . . x . . .
%       . . . . . . x .
%       x . . . . . . .


% el tablero se representa mediante un vector de 8 posiciones
% cada posicion i del vector representa una fila, mientras que
% su valor representa la columna en la cual se encuentra la reina
% asi solo necesitamos saber si las reinas no se atacan en las diagonales.
 
ocho_reinas :-
    permutacion([1,2,3,4,5,6,7,8],L),
    tableroCorrecto(L),
    write(L), nl,
    write_tablero(L).
    
write_tablero([X|L]):-
    write_fila(X,1), nl,
    write_tablero(L).

write_fila(_,9).
write_fila(X,N):-
    N < 9,
    (
      (
        X = N,
        write("X ")
      );
      (
        X \= N,
        write(". ")
     )
    ),
    N1 is N+1,
    write_fila(X,N1).
    
tableroCorrecto([]).
tableroCorrecto([C|R]):-
    not(amenaza(C,R)),
    tableroCorrecto(R).
    
amenaza(X,P,[C|_]):-
    X is C+P;
    X is C-P.
amenaza(X,P,[_|R]):-
    P1 is P+1,
    amenaza(X,P1,R).
amenaza(_,[]):- fail.
amenaza(X,Y):-amenaza(X,1,Y).  
    

