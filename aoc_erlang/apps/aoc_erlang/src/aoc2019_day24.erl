%%% Advent of Code solution for 2019 day 24.
%%% Created: 2019-12-24T11:21:51+00:00

-module(aoc2019_day24).
-include_lib("eunit/include/eunit.hrl").

part1(Bin) ->
  State = find_first_repeated_state(parse(Bin), sets:new()),
  biodiv_rating(State).

find_first_repeated_state(State, SeenStates) ->
  NewState = game_of_life(State),
  case sets:is_element(NewState, SeenStates) of
    true -> NewState;
    false ->
      find_first_repeated_state(
        NewState,
        sets:add_element(NewState,
                         SeenStates))
  end.

biodiv_rating(State) ->
  lists:foldl(fun(Off, Acc) ->
                  X = Off rem 5,
                  Y = Off div 5,
                  case maps:get({X, Y}, State) of
                    $# -> Acc + (1 bsl Off);
                    _ -> Acc
                  end
              end, 0, lists:seq(0, 24)).

game_of_life(Map) ->
  maps:from_list(
    [{{X, Y}, next({X, Y}, Map)} ||
      X <- lists:seq(0, 4),
      Y <- lists:seq(0, 4)]).

%% A bug dies (becoming an empty space) unless there is exactly one
%% bug adjacent to it.  An empty space becomes infested with a bug if
%% exactly one or two bugs are adjacent to it.

next(Key, Map) ->
  case {maps:get(Key, Map), adjacent_bugs(Key, Map)} of
    {$#, N} when N =/= 1 -> $.;
    {$., 1} -> $#;
    {$., 2} -> $#;
    {C, _} -> C
  end.

adjacent_bugs({X, Y}, Map) ->
  N = maps:get({X, Y - 1}, Map, $.),
  E = maps:get({X + 1, Y}, Map, $.),
  S = maps:get({X, Y + 1}, Map, $.),
  W = maps:get({X - 1, Y}, Map, $.),
  lists:foldl(fun($#, Acc) -> Acc + 1;
                 (_, Acc) -> Acc
              end, 0, [N, E, S, W]).

get_input() ->
  <<"...#.",
    "#.##.",
    "#..##",
    "#.###",
    "##...">>.

grid_to_str(Map) ->
  [[ maps:get({X, Y}, Map) ||
     X <- lists:seq(0, 4) ] ++ "\n"
   || Y <- lists:seq(0, 4) ].

parse(Bin) ->
  lists:foldl(fun({X, Y} = Key, Acc) ->
                  Off = Y * 5 + X,
                  maps:put(Key, binary:at(Bin, Off), Acc)
              end, #{},
              [{X, Y} ||
                X <- lists:seq(0, 4),
                Y <- lists:seq(0, 4)]).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(0, part1(Input))}
  %% , {"Part 2", ?_assertEqual(0, part2(Input))}
  ].

ex_test_() ->
  Bin = <<"....#",
          "#..#.",
          "#..##",
          "..#..",
          "#....">>,
  ?_assertEqual(2129920, part1(Bin)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
