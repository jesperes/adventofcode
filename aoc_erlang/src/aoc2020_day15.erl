%%% Advent of Code solution for 2020 day 15.
%%% Created: 2020-12-15T06:19:22+00:00

-module(aoc2020_day15).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
solve(Input, Limit) ->
  [Last|Rest] = lists:reverse(Input),
  Input0 = lists:reverse(Rest),

  Array = array_new(Limit),
  lists:foreach(
    fun({Turn, Num}) ->
        array_put(Array, Num, Turn)
    end, lists:zip(lists:seq(1, length(Input0)),
                   Input0)),

  NextTurn = length(Input) + 1,
  solve0(NextTurn, Last, Array, Limit).

solve0(Turn, Last, _, Limit) when Turn > Limit ->
  Last;
solve0(Turn, Last, Array, Limit) ->
  Next =
    case array_get(Array, Last) of
      0 -> 0;
      IndexOfLast -> Turn - IndexOfLast - 1
    end,
  array_put(Array, Last, Turn - 1),
  solve0(Turn + 1, Next, Array, Limit).

array_new(Size) ->
  counters:new(Size + 1, []).

array_put(Array, I, Val) ->
  ok = counters:put(Array, I + 1, Val).

array_get(Array, I) ->
  counters:get(Array, I + 1).

input() ->
  [6,4,12,1,20,0,16].

%% Tests
main_test_() ->
  Input = input(),

  [ {"Part 1", ?_assertEqual(475, solve(Input, 2020))}
  , {timeout, 60, {"Part 2", ?_assertEqual(11261, solve(Input, 30000000))}}
  ].

ex1_test_() ->
  ?_assertEqual(436, solve([0, 3, 6], 2020)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
