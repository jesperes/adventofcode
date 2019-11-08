-module(aoc2015_day10).

-include_lib("eunit/include/eunit.hrl").

input() ->
  "1321131112".

main_test_() ->
  {timeout, 60,
   [ fun simple/0
   , fun part1/0
   , fun part2/0
   ]}.

part1() -> ?assertEqual(492982, iterate(40)).

part2() -> ?assertEqual(6989950, iterate(50)).

simple() ->
  "11" = look_and_say("1"),
  "21" = look_and_say("11"),
  "1211" = look_and_say("21").

iterate(N) ->
    Fun = fun(_, S) -> look_and_say(S) end,
    length(lists:foldl(Fun, input(), lists:seq(1, N))).

look_and_say(List) ->
    lists:flatten(look_and_say0(List)).

look_and_say0([]) -> [];
look_and_say0([X|_] = L) ->
    {Len, Rest} = split_prefix(X, L, 0),
    [integer_to_list(Len), [X], look_and_say0(Rest)].

split_prefix(_, [], N) -> {N, []};
split_prefix(X, [Y|_] = L, N) when X =/= Y -> {N, L};
split_prefix(X, [X|Rest], N) ->
    split_prefix(X, Rest, N + 1).
