%%% Advent of Code solution for 2020 day 05.
%%% Created: 2020-12-05T06:22:29+00:00

-module(aoc2020_day05).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
solve(Input) ->
  %% Sort the list of seat ids in reverse order. This avoids the extra
  %% traversal to find the max seat id.
  [Max|_] = SeatIds =
    lists:sort(fun desc/2, lists:map(fun seat_id/1, Input)),
  {Max, find_seat(SeatIds)}.

%% Sorting function for descending order.
desc(A, B) -> B =< A.

%% Seat ids are really just the boarding card ids thinly disguised as
%% binary numbers
seat_id(S) ->
  lists:foldl(fun(C, Acc) when C =:= $B ; C =:= $R ->
                  (Acc bsl 1) bor 1;
                 (_, Acc) ->
                  Acc bsl 1
              end, 0, S).

%% The input to this function is a list of all occupied seats.  "Our"
%% seat is the only one missing, but it is specified to have a seat id
%% at -1 and +1, so we want to find the first "hole" in the sequence
%% of occupied seats.
find_seat([A, B|_]) when A == B + 2 -> B + 1;
find_seat([_|Rest]) -> find_seat(Rest).

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input05.txt).
get_input() ->
  inputs:get_as_lines(2020, 05).

%% Tests
main_test_() ->
  Input = get_input(),
  {"Part 1 & 2", ?_assertEqual({928, 610}, solve(Input))}.

seat_id_test_() ->
  [?_assertEqual(357, seat_id("FBFBBFFRLR")),
   ?_assertEqual(567, seat_id("BFFFBBFRRR")),
   ?_assertEqual(119, seat_id("FFFBBBFRRR")),
   ?_assertEqual(820, seat_id("BBFFBBFRLL"))].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
