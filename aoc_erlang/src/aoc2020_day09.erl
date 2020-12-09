%%% Advent of Code solution for 2020 day 09.
%%% Created: 2020-12-09T05:11:18+00:00

-module(aoc2020_day09).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Input) ->
  find_first_invalid(Input, 25).

part2(Input) ->
  find_range(Input, 138879426).

find_first_invalid([_|Rest] = List, N) ->
  {Preamble, [Next|_]} = lists:split(N, List),
  case [Next || X <- Preamble,
                Y <- Preamble,
                X /= Y, X + Y == Next] of
    [_|_] ->
      find_first_invalid(Rest, N);
    _ ->
      Next
  end.

find_range(List, Num) ->
  find_range(List, 2, Num).

find_range(List, Len, Num) when Len < length(List) ->
  case find_range0(List, Len, Num) of
    N when is_integer(N) ->
      N;
    false ->
      %% No range of len Num was found, try a longer one
      find_range(List, Len + 1, Num)
  end.

%% Find a range in `List' of length `Len' which sums up to `Num`.
%% Returns the sum of the first and last numbers if found, or false if
%% no such range was found.
find_range0(List, Len, _Num) when length(List) < Len ->
  false;
find_range0([_|Rest] = List, Len, Num) ->
  {Range, _} = lists:split(Len, List),
  Sum = lists:sum(Range),
  if Sum == Num ->
      lists:min(Range) + lists:max(Range);
     true ->
      find_range0(Rest, Len, Num)
  end.

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input09.txt).
get_input() ->
  inputs:get_as_ints(2020, 09).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(138879426, part1(Input))}
  , {"Part 2", ?_assertEqual(23761694, part2(Input))}
  ].

test_input() ->
  [35,
   20,
   15,
   25,
   47,
   40,
   62,
   55,
   65,
   95,
   102,
   117,
   150,
   182,
   127,
   219,
   299,
   277,
   309,
   576].

ex1_test_() ->
  L = test_input(),
  ?_assertEqual(127, find_first_invalid(L, 5)).

ex2_test_() ->
  L = test_input(),
  ?_assertEqual(62, find_range(L, 127)).


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
