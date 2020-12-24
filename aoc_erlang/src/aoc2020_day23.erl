%%% Advent of Code solution for 2020 day 23.
%%% Created: 2020-12-23T05:43:00+00:00

-module(aoc2020_day23).
-include_lib("eunit/include/eunit.hrl").

-export([ split_at/2
        , solve/2
        , solve2/2
        ]).

-compile([nowarn_unused_function]).

%% Puzzle solution
part1(Input) ->
  solve(Input, 100).

solve(L, 0) ->
  {L1, L2} = split_at(1, L),
  lists:map(fun(N) -> N + $0 end, L2 ++ L1);
solve([CurrentCup, A, B, C|Input], Moves) ->
  Dest = find_destination_cup(CurrentCup, Input),
  {L1, L2} = split_at(Dest, Input),
  %% io:format("~nMove~n", []),
  %% io:format("===========~n", []),
  %% io:format("Current = ~p~n", [CurrentCup]),
  %% io:format("L = ~p~n", [L]),
  %% io:format("Pickup = ~p~n", [[A, B, C]]),
  %% io:format("Dest = ~p~n", [Dest]),
  %% io:format("Split: ~p + ~p~n", [L1, L2]),
  LNext = L1 ++ [Dest, A, B, C] ++ L2 ++ [CurrentCup],
  %% io:format("Change: ~p~n", [LNext]),
  solve(LNext, Moves - 1).

find_destination_cup(C, Input) ->
  C0 = (C + 10 - 1) rem 10,
  case lists:member(C0, Input) of
    true -> C0;
    false -> find_destination_cup(C0, Input)
  end.

part2(_Input) ->
  ok.

solve2(Input, _Moves) ->
  {First, Last, Ring} =
    lists:foldl(
      fun(N, {First, Prev, G}) ->
          V = digraph:add_vertex(G, N),
          case Prev of
            nil ->
              {V, V, G};
            _ ->
              digraph:add_edge(G, Prev, V),
              {First, V, G}
          end
      end, {nil, nil, digraph:new()}, Input),
  digraph:add_edge(Ring, Last, First),
  io:format("Cycle: ~p~n", [digraph:get_cycle(Ring, First)]).

%% ======================================================================
%% Helpers
%% ======================================================================

split_at(C, L) ->
  split_at(C, L, []).

split_at(_C, [], L) ->
  {lists:reverse(L), []};
split_at(C, [C|Rest], L) ->
  {lists:reverse(L), Rest};
split_at(C, [X|Rest], L) ->
  split_at(C, Rest, [X|L]).

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input23.txt).
get_input() ->
  [5, 8, 6, 4, 3, 9, 1, 7, 2].

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual("28946753", part1(Input))}
  , {timeout, 600, {"Part 2", ?_assertEqual(0, part2(Input))}}
  ].

ex1_test_() ->
  [ ?_assertEqual("92658374", solve([3, 8, 9, 1, 2, 5, 4, 6, 7], 10))
  , ?_assertEqual("67384529", solve([3, 8, 9, 1, 2, 5, 4, 6, 7], 100))
  ].

%% too high 54237869

split_at_test_() ->
  [ ?_assertEqual({[1, 2, 3], [5, 6, 7]}, split_at(4, [1, 2, 3, 4, 5, 6, 7]))
  , ?_assertEqual({[1, 2, 3, 4, 5, 6, 7], []}, split_at(8, [1, 2, 3, 4, 5, 6, 7]))
  , ?_assertEqual({[], [2, 3, 4, 5, 6, 7]}, split_at(1, [1, 2, 3, 4, 5, 6, 7]))
  , ?_assertEqual({[1, 2, 3, 4, 5, 6], []}, split_at(7, [1, 2, 3, 4, 5, 6, 7]))
  , ?_assertEqual({[1, 2, 3, 4, 5, 6], []}, split_at(7, [1, 2, 3, 4, 5, 6, 7]))
  ].


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
