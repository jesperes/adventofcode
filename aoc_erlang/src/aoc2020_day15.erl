%%% Advent of Code solution for 2020 day 15.
%%% Created: 2020-12-15T06:19:22+00:00

-module(aoc2020_day15).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Input) ->
  speak(lists:reverse(Input), 2020).

speak([F|_] = L, Limit) when length(L) == Limit ->
  F;
speak([MostRecentlySpokenNumber|Rest] = L, Limit) ->
  case indexof(MostRecentlySpokenNumber, Rest) of
    false -> speak([0|L], Limit);
    I -> speak([I + 1|L], Limit)
  end.

indexof(E, L) -> indexof(E, 0, L).

indexof(_E, _I, []) -> false;
indexof(E, I, [E|_]) -> I;
indexof(E, I, [_|Rest]) -> indexof(E, I + 1, Rest).


part2(List, Limit) ->
  %% Map from turn# to the number spoken during that turn
  TurnToNumberMap = maps:from_list(
                      lists:zip(lists:seq(1, length(List)),
                                List)),

  %% Map from numbers to a list of the turns that number was spoken in
  NumberToTurnsMap =
    lists:foldl(fun({Turn, Num}, Acc) ->
                    maps:update_with(Num,
                                     fun(Old) -> [Turn|Old] end,
                                     [Turn], Acc)
                end, #{},
                lists:zip(lists:seq(1, length(List)),
                          List)),

  LastSpoken = lists:last(List),
  speak2(LastSpoken, TurnToNumberMap, NumberToTurnsMap, Limit).

speak2(LastSpoken, TurnToNumberMap, NumberToTurnsMap, Limit) ->

  Turns = maps:size(TurnToNumberMap),

  case Turns of
    T when T == Limit ->
      LastSpoken;
    _ ->
      NextSpoken =
        case maps:get(LastSpoken, NumberToTurnsMap, undefined) of
          [_] -> 0;
          [T1,T2|_] ->
            T1 - T2
        end,

      Turn = Turns + 1,

      speak2(NextSpoken,
             maps:put(Turn, NextSpoken, TurnToNumberMap),
             maps:update_with(NextSpoken,
                              fun([Old|_]) -> [Turn, Old] end, % only keep last two
                              [Turn], NumberToTurnsMap),
             Limit)
  end.

input() ->
  [6,4,12,1,20,0,16].

%% Tests
main_test_() ->
  Input = input(),

  [ {"Part 1", ?_assertEqual(475, part1(Input))}
  , {timeout, 10000, {"Part 2", ?_assertEqual(11261, part2(Input, 30000000))}}
  ].

indexof_test_() ->
  [ ?_assertEqual(2, indexof(0, [6, 3, 0]))
  , ?_assertEqual(0, indexof(6, [6, 3, 0]))
  ].

ex1_test_() ->
  ?_assertEqual(436, part1([0, 3, 6])).

ex2_test_() ->
  ?_assertEqual(436, part2([0, 3, 6], 2020)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
