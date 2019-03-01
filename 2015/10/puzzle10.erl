%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 18 Dec 2018 by Jesper Eskilson <>

-module(puzzle10).

% -export([start/0]).
-compile([export_all]).

input() -> 
    "1321131112".

start() ->
    test(),
    {part1(), part2()}.

part1() -> iterate(40).
part2() -> iterate(50).

iterate(N) ->
    Fun = fun(_, S) -> look_and_say(S) end,
    length(lists:foldl(Fun, input(), lists:seq(1, N))).

test() ->
    "11" = look_and_say("1"),
    "21" = look_and_say("11"),
    "1211" = look_and_say("21").

look_and_say(List) ->
    lists:flatten(look_and_say0(List)).
    
look_and_say0([]) -> [];
look_and_say0([X|_] = L) ->
    {Len, Rest} = split_prefix(X, L, 0),
    [integer_to_list(Len), [X], look_and_say0(Rest)].

split_prefix(X, [], N) -> {N, []};
split_prefix(X, [Y|_] = L, N) when X =/= Y -> {N, L};
split_prefix(X, [X|Rest], N) ->
    split_prefix(X, Rest, N + 1).
                             


