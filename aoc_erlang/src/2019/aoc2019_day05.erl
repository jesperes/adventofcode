%%% Advent of Code solution for 2019 day 05.
%%% Created: 2019-12-05T16:25:02+00:00

-module(aoc2019_day05).
-include_lib("eunit/include/eunit.hrl").

get_input() ->
  intcode:parse(inputs:get_as_string(2019, 5)).

execute(Prog, Input) ->
  {_, [Output|Rest]} = intcode:execute(Prog, [Input]),
  %% Rest should only consist of zeroes (or be empty)
  ?assertEqual([], lists:filter(fun(X) -> X =/= 0 end, Rest)),
  Output.

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(16348437, execute(Input, 1))}
  , {"Part 2", ?_assertEqual(6959377, execute(Input, 5))}
  ].

test_helper(InputStr, Input) ->
  Prog = intcode:parse(InputStr),
  {_, [Output]} = intcode:execute(Prog, [Input]),
  Output.

ex1_test_() ->
  %% Returns its input
  ExProg1 = "3,0,4,0,99",
  ?_assertEqual(42, test_helper(ExProg1, 42)).

ex3_test_() ->
  ExProg3 = "3,9,8,9,10,9,4,9,99,-1,8",
  %% Equal to 8
  [ ?_assertEqual(0,  test_helper(ExProg3, 1))
  , ?_assertEqual(1,  test_helper(ExProg3, 8))
  ].

ex4_test_() ->
  ExProg4 = "3,9,7,9,10,9,4,9,99,-1,8",
  %% Less than 8
  [ ?_assertEqual(1,  test_helper(ExProg4, 1))
  , ?_assertEqual(0,  test_helper(ExProg4, 8))
  , ?_assertEqual(0,  test_helper(ExProg4, 9))
  ].

ex5_test_() ->
  ExProg5 = "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9",
  %% Equal to 0
  [ ?_assertEqual(0,  test_helper(ExProg5, 0))
  , ?_assertEqual(1,  test_helper(ExProg5, 42))
  ].

ex6_test_() ->
  %% Outputs 999 if the input value is below 8, outputs 1000 if the
  %% input value is equal to 8, or outputs 1001 if the input value is
  %% greater than 8.
  ExProg6 =
    "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,"
    "1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,"
    "999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99",

  [ ?_assertEqual(999, test_helper(ExProg6, 7))
  , ?_assertEqual(1000, test_helper(ExProg6, 8))
  , ?_assertEqual(1001, test_helper(ExProg6, 9))
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
