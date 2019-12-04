%%% Advent of Code solution for 2019 day 04.
%%% Created: 2019-12-04T07:51:59+00:00

-module(aoc2019_day04).
-include_lib("eunit/include/eunit.hrl").

solve() ->
  Passwords = lists:seq(402328, 864247),
  Part1 = length(lists:filter(fun is_part1/1, Passwords)),
  Part2 = length(lists:filter(fun is_part2/1, Passwords)),
  {Part1, Part2}.

is_part1(P) when is_integer(P) ->
  is_part1(integer_to_list(P));
is_part1([A, A, B, C, D, E]) when (B >= A) and (C >= B) and (D >= C) and (E >= D) -> true;
is_part1([A, B, B, C, D, E]) when (B >= A) and (C >= B) and (D >= C) and (E >= D) -> true;
is_part1([A, B, C, C, D, E]) when (B >= A) and (C >= B) and (D >= C) and (E >= D) -> true;
is_part1([A, B, C, D, D, E]) when (B >= A) and (C >= B) and (D >= C) and (E >= D) -> true;
is_part1([A, B, C, D, E, E]) when (B >= A) and (C >= B) and (D >= C) and (E >= D) -> true;
is_part1(_) -> false.

is_part2(P) when is_integer(P) ->
  is_part2(integer_to_list(P));
is_part2([A, A, B, C, D, E]) when (B >= A) and (C >= B) and (D >= C) and (E >= D) and (A =/= B) -> true;
is_part2([A, B, B, C, D, E]) when (B >= A) and (C >= B) and (D >= C) and (E >= D) and (A =/= B) and (B =/= C) -> true;
is_part2([A, B, C, C, D, E]) when (B >= A) and (C >= B) and (D >= C) and (E >= D) and (B =/= C) and (C =/= D) -> true;
is_part2([A, B, C, D, D, E]) when (B >= A) and (C >= B) and (D >= C) and (E >= D) and (C =/= D) and (D =/= E) -> true;
is_part2([A, B, C, D, E, E]) when (B >= A) and (C >= B) and (D >= C) and (E >= D) and (D =/= E) -> true;
is_part2(_) -> false.

%% Tests
main_test_() ->
  {"Part 1 & 2", ?_assertEqual({454, 288}, solve())}.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
