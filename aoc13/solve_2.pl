#!/usr/bin/env swipl

:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

parse(Input_path, Buses) :-
    DCG = read_input(Buses),
    phrase_from_file(DCG,Input_path), !.

read_input(Buses) -->
    number(_),
    "\n",
    read_buses(0, Buses),
    "\n".

read_buses(I, Buses) -->
    "x,",
    {J #= I - 1},
    read_buses(J, Buses).
read_buses(I, [(I, Bus)|Buses]) -->
    number(Bus),
    {J #= I - 1}, % X = -i mod Bus. X is I before 0 mod Bus
    ("," -> read_buses(J, Buses) ;
     {Buses = []}).

product(A, B, C) :- C is A*B.
prod_3(A,B,C,D) :- D is A*B*C.
sum_2(A, B, C) :- C is A+B.

pair(X, Y, (X,Y)).

find_u(X, M, U) :-
    length(_,U),
    (X*U) mod M #= 1.

% To solve this:
% x = a mod i
% x = b mod j
% solution((a, i), (b, j), Answer).
solution(Pairs, X) :-
    maplist(pair, As, Ms, Pairs), % As is result, Ms is modulos
    foldl(product, Ms, 1, N), % Least Common Multiple is product of all
    maplist(divmod(N), Ms, Ps, _), % Ps(n) = N / M(n)
    maplist(find_u, Ps, Ms, Us), % Us(n): Ps(n) * Us(n) = 1 mod Ms(n)
    maplist(prod_3, As, Ps, Us, Rs), % Rs(n) = As(n) * Ps(n) * Us(n)
    foldl(sum_2, Rs, 0, Y), % Sum Rs
    X #= Y mod N, !.

% 626670513163231.
solve(X) :-
    parse("input", Buses),
    solution(Buses, X).
