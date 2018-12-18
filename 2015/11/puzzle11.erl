%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 18 Dec 2018 by Jesper Eskilson <>

-module(puzzle11).

-compile([export_all]).

input() ->
    "cqjxjnds".

rev(S) ->
     lists:reverse(S).

increment(Pwd) ->
    Incr = rev(increment0(rev(Pwd))).

increment0([]) ->
    [];
increment0([$z|Xs]) ->
    [$a|increment0(Xs)];
increment0([C|Xs]) ->
    [C + 1|Xs].

valid(Pwd) ->
    
    %% * Passwords must include one increasing straight of at least three
    %% letters, like abc, bcd, cde, and so on, up to xyz. They cannot skip
    %% letters; abd doesn't count.
    %% 
    %% * Passwords may not contain the letters i, o, or l, as these
    %% letters can be mistaken for other characters and are therefore
    %% confusing.
    %% 
    %% * Passwords must contain at least two different, non-overlapping
    %% pairs of letters, like aa, bb, or zz.
    
    true.

next_valid_password(Pwd) ->
    Next = increment(Pwd),
    case valid(Next) of
        true ->
            Next;
        false ->
            %% invalid password, take next
            next_valid_password(increment(Next))
    end.

start() ->
    increment(input()).

