#!/usr/bin/env swipl

:- use_module(library(dcg/basics), except([eos/2])).

parse(Input_path, Time, Buses) :-
    DCG = read_input([Time, Buses]),
    phrase_from_file(DCG,Input_path), !.

read_input([Time,Buses]) -->
    number(Time),
    "\n",
    read_buses(Buses),
    "\n".

read_buses(Buses) -->
    ("x,", read_buses(Buses)).
read_buses([Bus|Buses]) -->
    number(Bus),
    ("," -> read_buses(Buses) ;
     {Buses = []}).

waiting_time(D, Bus, (X, Bus)) :- X is Bus - D mod Bus, !.
times(D, Buses, Times) :- maplist(waiting_time(D), Buses, Times).

mintuple((X,X1), (Y,_), (R,R1)) :- X <  Y, R = X, R1 = X1, !.
mintuple((X,_), (Y,Y1), (R,R1)) :- Y =< X, R = Y, R1 = Y1, !.

part1(Answer) :-
    parse("input", Time, Buses),
    times(Time, Buses, Times),
    foldl(mintuple, Times, (1.0Inf, 0), (X, Y)),
    Answer is X * Y.
