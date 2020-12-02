%%% Advent of Code solution for 2019 day 19.
%%% Created: 2019-12-19T06:35:40+00:00

-module(aoc2019_day19).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Prog) ->
  lists:sum([read_point(X, Y, Prog) ||
              X <- lists:seq(0, 49),
              Y <- lists:seq(0, 49)]).

%% Returns 1 if {X,Y} is covered by the tractor beam, 0 otherwise.
read_point(X, Y, Prog) ->
  {_, [R]} = intcode:execute(Prog, [X, Y]),
  R.

%% For part two we need to find the closest point where the 100x100
%% spaceship fits entirely within the tractor beam. This turned out to
%% be most easily done by hand.
part2(Prog) ->
  Size = 100,
  X = 1308,
  Y = 1049,
  ?assert(fits(Prog, X, Y, Size)),
  X * 10000 + Y.

fits(Prog, X, Y, Size) ->
  S1 = lists:sum([read_point(X, Y + Y0, Prog) || Y0 <- lists:seq(0, 99)]),
  S2 = lists:sum([read_point(X + X0, Y, Prog) || X0 <- lists:seq(0, 99)]),
  S1 >= Size andalso S2 >= Size.

%% Input reader (place downloaded input file in
%% priv/inputs/2019/input19.txt).
get_input() ->
  intcode:parse(inputs:get_as_binary(2019, 19)).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(164, part1(Input))}
  , {"Part 2", ?_assertEqual(13081049, part2(Input))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
