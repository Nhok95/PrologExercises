:- module(pack_squares, [pack_squares/0]).
:- [library(clpfd)].

pack_squares :-
    maplist(square, [5,4,3,2], Squares),
    flatten(Squares, Coords),
    not_overlap(Squares),
    Coords ins 1..10,
    label(Coords),
    maplist(writeln, Squares),
    draw_squares(Squares).

draw_squares(Squares) :-
    forall(between(1, 10, Y),
           (   forall(between(1, 10, X),
              sumpts(X, Y, Squares, 0)),
           nl
           )).

sumpts(_, _, [], S) :- write(S).
sumpts(X, Y, [[X1,Y1, X2,Y2]|Qs], A) :-
    ( ( X >= X1, X =< X2, Y >= Y1, Y =< Y2 )
    ->  B is A+X2-X1+1
    ;   B is A
    ),
    sumpts(X, Y, Qs, B).

square(D, [X1,Y1, X2,Y2]) :-
    X1 + D - 1 #= X2,
    Y1 + D - 1 #= Y2.

not_overlap([_]).
not_overlap([A,B|L]) :-
    not_overlap(A, [B|L]),
    !, not_overlap([B|L]).

not_overlap(_, []).
not_overlap(Q, [R|Rs]) :-
    not_overlap_c(Q, R),
    not_overlap_c(R, Q),
    not_overlap(Q, Rs).

not_overlap_c([X1,Y1, X2,Y2], Q) :-
    not_inside(X1,Y1, Q),
    not_inside(X1,Y2, Q),
    not_inside(X2,Y1, Q),
    not_inside(X2,Y2, Q).

not_inside(X,Y, [X1,Y1, X2,Y2]) :-
    X #< X1 #\/ X #> X2 #\/ Y #< Y1 #\/ Y #> Y2.