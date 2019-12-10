%%% Advent of Code solution for 2019 day 10.
%%% Created: 2019-12-10T05:33:20+00:00

-module(aoc2019_day10).
-include_lib("eunit/include/eunit.hrl").

-type asteroid() :: {X :: integer(), Y :: integer()}.

%% Puzzle solution
part1(Asteroids) ->
  %% Compute a list of {A, B, Dir} where A and B are asteroid coords
  %% and Dir is the direction from A to B expressed as a fraction.
  AsteroidDirs =
    [ {A, B, direction(A, B)}
      || A <- Asteroids,
         B <- Asteroids,
         A =/= B],

  %% Compute a map where the keys are asteroids (A). Each key maps to
  %% a map of (integer() -> list(Asteroids)), where the integer is the
  %% slope (direction), and the list contains all the asteroids which
  %% lie along that line.
  AstroidsGroupedByVisibility =
    lists:foldl(
      fun({A, B, Dir}, Map) ->
          maps:update_with(
            A, fun(OldMap) ->
                   maps:update_with(
                     Dir, fun(OldList) ->
                              [B|OldList]
                          end, [B], OldMap)
               end, #{Dir => [B]}, Map)
      end, #{}, AsteroidDirs),

  maps:fold(fun(K, V, {_, Max, _} = Acc) ->
                case maps:size(V) of
                  Len when Len > Max -> {K, Len, V};
                  _ -> Acc
                end
            end, {undef, 0, []}, AstroidsGroupedByVisibility).

part2(_Input) ->
  ?debugMsg("Not implemented."),
  0.

direction({X0, Y0}, {X1, Y1}) ->
  floor(math:atan2(Y1 - Y0, X1 - X0) * 1000000).

%% Input reader (place downloaded input file in
%% priv/inputs/2019/input10.txt).
-spec get_input() -> list(asteroid()).
get_input() ->
  Binary = inputs:get_as_binary(2019, 10),
  %% Grid is 33 chars wide (+ newline)
  parse_grid(Binary, 33).

-spec parse_grid(binary(), Width :: integer()) -> list(asteroid()).
parse_grid(Binary, Width) ->
  lists:map(fun({Start, _} = _Match) ->
                %% +1 is for the newline.
                {_X = Start rem (Width + 1), _Y = Start div (Width + 1)}
            end, binary:matches(Binary, <<"#">>)).

%% Tests

%% Too high: (You guessed 337.)
%% Too high: (You guessed 336.)
%% Wrong:    (You guessed 335.)
%% Wrong:    (You guessed 333.)
%% Too low:  (You guessed 330.)
%% Guessed correct answer: 334, but code still gives 337...

main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertMatch({_, 334, _}, part1(Input))}
    %%   , {"Part 2", ?_assertEqual(0, part2(Input))}
  ].

ex1_test_() ->
  Binary = <<".#..#\n",
             ".....\n",
             "#####\n",
             "....#\n",
             "...##\n">>,
  Asteroids = parse_grid(Binary, 5),
  ?_assertMatch({{3,4}, 8, _}, part1(Asteroids)).

ex2_test_() ->
  Binary = <<"......#.#.\n",
             "#..#.#....\n",
             "..#######.\n",
             ".#.#.###..\n",
             ".#..#.....\n",
             "..#....#.#\n",
             "#..#....#.\n",
             ".##.#..###\n",
             "##...#..#.\n",
             ".#....####\n">>,
  Asteroids = parse_grid(Binary, 10),
  ?_assertMatch({{5, 8}, 33, _}, part1(Asteroids)).

ex3_test_() ->
  Binary = <<".#..#..###\n",
             "####.###.#\n",
             "....###.#.\n",
             "..###.##.#\n",
             "##.##.#.#.\n",
             "....###..#\n",
             "..#.#..#.#\n",
             "#..#.#.###\n",
             ".##...##.#\n",
             ".....#.#..\n">>,
  Asteroids = parse_grid(Binary, 10),
  ?_assertMatch({{6, 3}, 41, _}, part1(Asteroids)).

ex4_test_() ->
  Binary = <<"#.#...#.#.\n",
             ".###....#.\n",
             ".#....#...\n",
             "##.#.#.#.#\n",
             "....#.#.#.\n",
             ".##..###.#\n",
             "..#...##..\n",
             "..##....##\n",
             "......#...\n",
             ".####.###.\n">>,
  Asteroids = parse_grid(Binary, 10),
  ?_assertMatch({{1, 2}, 35, _}, part1(Asteroids)).

ex5_test_() ->
  Binary = <<".#..##.###...#######\n",
             "##.############..##.\n",
             ".#.######.########.#\n",
             ".###.#######.####.#.\n",
             "#####.##.#.##.###.##\n",
             "..#####..#.#########\n",
             "####################\n",
             "#.####....###.#.#.##\n",
             "##.#################\n",
             "#####.##.###..####..\n",
             "..######..##.#######\n",
             "####.##.####...##..#\n",
             ".#####..#.######.###\n",
             "##...#.##########...\n",
             "#.##########.#######\n",
             ".####.#.###.###.#.##\n",
             "....##.##.###..#####\n",
             ".#.#.###########.###\n",
             "#.#.#.#####.####.###\n",
             "###.##.####.##.#..##\n">>,
  Asteroids = parse_grid(Binary, 20),
  ?_assertMatch({{11, 13}, 210, _}, part1(Asteroids)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
