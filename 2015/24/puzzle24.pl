%% -*- prolog -*-

:- use_module(library(clpfd)).

packages_test([1, 2, 3, 4, 5, 7, 8, 9, 10, 11]).

packages([1, 2, 3, 7, 11, 13, 17, 19, 23, 31, 37, 41, 43, 47,
           53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107,
          109, 113]).

start1(As, Bs, Cs) :-
        packages_test(List),
        part3(List, As, Bs, Cs).

start2(As, Bs, Cs) :-
        packages(List),
        part3(List, As, Bs, Cs).
        
%% Partition the integers in As into 3 subsets of equal sum.
part3(Xs, As, Bs, Cs) :-
        sum(Xs, #=, Sum),
        N * 3 #= Sum,
        tpartition(*, Xs, As, BCs),
        sum(As, #=, N),
        sum(BCs, #=, N * 2),
        tpartition(*, BCs, Bs, Cs),
        sum(Bs, #=, N),
        sum(Cs, #=, N).

%% Prolog magic below, stolen from Stackoverflow.
%% https://stackoverflow.com/questions/26990728/most-general-higher-order-constraint-describing-a-sequence-of-integers-ordered-w/29867514#29867514
%% https://stackoverflow.com/questions/39833370/what-use-does-if-3-have

_ * true.
_ * false.
if_(If_1, Then_0, Else_0) :-
        call(If_1, T),
        (  T == true -> call(Then_0)
        ;  T == false -> call(Else_0)
        ;  nonvar(T) -> throw(error(type_error(boolean,T),_))
        ;  /* var(T) */ throw(error(instantiation_error,_))
        ).

tpartition(P_2,List,Ts,Fs) :- tpartition_ts_fs_(List,Ts,Fs,P_2).
tpartition_ts_fs_([],[],[],_).
tpartition_ts_fs_([X|Xs0],Ts,Fs,P_2) :-
        if_(call(P_2,X),
            (Ts = [X|Ts0], Fs = Fs0),
            (Ts = Ts0,     Fs = [X|Fs0])),
        tpartition_ts_fs_(Xs0,Ts0,Fs0,P_2).
