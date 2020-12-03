%%% Advent of Code solution for 2020 day 01.
%%% Created: 2020-12-01T07:09:55+00:00

-module(aoc2020_day01).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Input) ->
  [X * Y || X <- Input,
            Y <- Input,
            X < Y,
            X + Y == 2020].

part2(Input) ->
  Min = lists:min(Input),
  [X * Y * Z || X <- Input,
                Y <- Input,
                X < Y,
                X + Y < (2020 - Min),
                Z <- Input,
                Y < Z,
                X + Y + Z == 2020].

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input01.txt).
get_input() ->
  lists:map(fun erlang:list_to_integer/1, inputs:get_as_lines(2020, 01)).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual([987339], part1(Input))}
  , {"Part 2", ?_assertEqual([259521570], part2(Input))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
