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
    false ->
      %% Number not spoken before
      speak([0|L], Limit);
    I ->
      %% Number was spoken before, compute number of turns
      %% apart of the last two times it was spoken.
      speak([I + 1|L], Limit)
  end.

indexof(E, L) -> indexof(E, 0, L).

indexof(_E, _I, []) -> false;
indexof(E, I, [E|_]) -> I;
indexof(E, I, [_|Rest]) -> indexof(E, I + 1, Rest).


part2(_Input) ->
  ?debugMsg("Not implemented."),
  0.

input() ->
  [6,4,12,1,20,0,16].

%% Tests
main_test_() ->
  Input = input(),

  [ {"Part 1", ?_assertEqual(475, part1(Input))}
  , {"Part 2", ?_assertEqual(0, part2(Input))}
  ].

indexof_test_() ->
  [ ?_assertEqual(2, indexof(0, [6, 3, 0]))
  , ?_assertEqual(0, indexof(6, [6, 3, 0]))
  ].

ex1_test_() ->
  [ ?_assertEqual(436, part1([0, 3, 6]))
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
