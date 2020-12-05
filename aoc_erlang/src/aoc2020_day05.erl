%%% Advent of Code solution for 2020 day 05.
%%% Created: 2020-12-05T06:22:29+00:00

-module(aoc2020_day05).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Input) ->
  lists:max(lists:map(fun seat_id/1, Input)).

part2(Input) ->
  Set = lists:foldl(
          fun(S, Acc) ->
              sets:add_element(seat_id(S), Acc)
          end, sets:new(), Input),
  FilledSeats = lists:sort(sets:to_list(Set)),
  find_seat(FilledSeats).

%% ============================================================
%% Part 1
%% ============================================================

seat_id(S) ->
  ?assertEqual(7 + 3, length(S)),
  {Rows, Seats} = lists:split(7, S),
  ?assertEqual(7, length(Rows)),
  Row = binary_search(Rows, 128, 0, 127, $F, $B),
  Seat = binary_search(Seats, 8, 0, 7, $L, $R),
  Row * 8 + Seat.

binary_search([], Size, Lower, Upper, _LowerC, _UpperC) when
    Size == 1, Lower == Upper ->
  %% End case
  Lower;
binary_search([C|Rows], Size, Lower, _Upper, LowerC, UpperC) when
    C == LowerC ->
  Half = (Size bsr 1) - 1,
  binary_search(Rows, Size bsr 1, Lower, Lower + Half, LowerC, UpperC);
binary_search([C|Rows], Size, Lower, Upper, LowerC, UpperC) when
    C == UpperC ->
  %% Keep upper half
  Half = Size bsr 1,
  binary_search(Rows, Size bsr 1, Lower + Half, Upper, LowerC, UpperC).

%% ============================================================
%% Part 2
%% ============================================================

%% The input to this function is a list of all occupied seats.  "Our"
%% seat is the only one missing, but it is specified to have a seat id
%% at -1 and +1, so we want to find the first "hole" in the sequence
%% of occupied seats.
find_seat([A, B|_]) when B == A + 2 ->
  A + 1;
find_seat([_|Rest]) ->
  find_seat(Rest).

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input05.txt).
get_input() ->
  inputs:get_as_lines(2020, 05).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(928, part1(Input))}
  , {"Part 2", ?_assertEqual(610, part2(Input))}
  ].

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
