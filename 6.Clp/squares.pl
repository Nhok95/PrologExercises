:- use_module(library(clpfd)).

%ejemplo(_, Big, [S1...SN]): how to fit all squares of sizes S1...SN in a square of size Big?
ejemplo(0,  3,[2,1,1,1,1,1]).
    %SOL:
    % RowVars = [1,1,2,3,3,3],
    % ColVars = [1,3,3,3,2,1],
ejemplo(1,  4,[2,2,2,1,1,1,1]).
ejemplo(2,  5,[3,2,2,2,1,1,1,1]).
ejemplo(3, 19,[10,9,7,6,4,4,3,3,3,3,3,2,2,2,1,1,1,1,1,1]).
ejemplo(4,112,[50,42,37,35,33,29,27,25,24,19,18,17,16,15,11,9,8,7,6,4,2]).
ejemplo(5,175,[81,64,56,55,51,43,39,38,35,33,31,30,29,20,18,16,14,9,8,5,4,3,2,1]).


main:- 
    ejemplo(3,Big,Sides),
    nl, write('Fitting all squares of size '), write(Sides), write(' into big square of size '), write(Big), nl,nl,
    length(Sides,N),
    length(RowVars,N), % get list of N prolog vars: Row coordinates of each small square
    length(ColVars,N), % get list of N prolog vars: Cols coordinates of each small square
    insideBigSquare(Big,Sides,RowVars),
    insideBigSquare(Big,Sides,ColVars),
    nonoverlapping(Sides,RowVars,ColVars),
    append(RowVars,ColVars,Vars),
    labeling([ffc],Vars),
    write("Rows: "),write(RowVars), nl, %
    write("Cols: "),write(ColVars), nl, %
    displaySol(N,Sides,RowVars,ColVars), halt.

insideBigSquare(_,[],[]).
insideBigSquare(Big, [S|Sides], [V|Vars]) :-
    S1 is S-1,
    End is Big - S1,
    V in 1..End,
    insideBigSquare(Big, Sides, Vars).

nonoverlapping([],[],[]).
nonoverlapping([S|Sides], [R|RowVars], [C|ColVars]):-
    nonoverlap(S,R,C,Sides,RowVars,ColVars),
    nonoverlapping(Sides, RowVars, ColVars).

nonoverlap(_,_,_,[],[],[]).
nonoverlap(S, R, C, [S2|Sides], [R2|RowVars], [C2|ColVars]):-
    R2 #>= R + S #\/ C2 #>= C + S #\/ 
    R2 #=< R - S2 #\/ C2 #=< C - S2,
    nonoverlap(S, R, C, Sides, RowVars, ColVars).

displaySol(N,Sides,RowVars,ColVars):-
    between(1,N,Row), nl, between(1,N,Col),
    nth1(K,Sides,S),    
    nth1(K,RowVars,RV),    RVS is RV+S-1,     between(RV,RVS,Row),
    nth1(K,ColVars,CV),    CVS is CV+S-1,     between(CV,CVS,Col),
    writeSide(S), fail.
displaySol(_,_,_,_):- nl,nl,!.

writeSide(S):- S<10, write('  '),write(S),!.
writeSide(S):-       write(' ' ),write(S),!.
