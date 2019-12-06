#!/usr/bin/env swipl
%% -*- prolog -*-

:- use_module(library(clpfd)).
:- initialization(main, main).

common([P, X5, X4, X3, X2, X1, X0]) :-
        P in 402328..864247,
        X0 in 0..9,
        X1 in 0..9,
        X2 in 0..9,
        X3 in 0..9,
        X4 in 0..9,
        X5 in 0..9,
        P #= X5 + X4 * 10 + X3 * 100 + X2 * 1000 + X1 * 10000 + X0 * 100000,

        %% No decreasing digits from left to right
        X5 #>= X4,
        X4 #>= X3,
        X3 #>= X2,
        X2 #>= X1,
        X1 #>= X0.

password1(P) :-
        common([P, X5, X4, X3, X2, X1, X0]),
        (X5 #= X4) #\/ (X4 #= X3) #\/ (X3 #= X2) #\/ (X2 #= X1) #\/ (X1 #= X0),
        labeling([], [X0, X1, X2, X3, X4, X5, P]).


password2(P) :-
        common([P, X5, X4, X3, X2, X1, X0]),

        (                (X5 #= X4) #/\ (X4 #\= X3)) #\/ %% aab...
        ((X5 #\= X4) #/\ (X4 #= X3) #/\ (X3 #\= X2)) #\/ %% baac..
        ((X4 #\= X3) #/\ (X3 #= X2) #/\ (X2 #\= X1)) #\/ %% .baac.
        ((X3 #\= X2) #/\ (X2 #= X1) #/\ (X1 #\= X0)) #\/ %% ..baac
        ((X2 #\= X1) #/\ (X1 #= X0)                ),    %% ...baa
        labeling([], [X0, X1, X2, X3, X4, X5, P]).


main(_Argv) :-
        findall(P1, password1(P1), P1s),
        length(P1s, 454),
        findall(P2, password2(P2), P2s),
        length(P2s, 288).
