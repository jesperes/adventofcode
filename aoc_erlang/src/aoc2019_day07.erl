%%% Advent of Code solution for 2019 day 07.
%%% Created: 2019-12-05T16:25:02+00:00

%% Solution based on the day 5 solution.

-module(aoc2019_day07).
-include_lib("eunit/include/eunit.hrl").

%%% --- PART 1 ---

chained_execute(_Prog, Input1, []) ->
  Input1;
chained_execute(Prog, Input1, [Input2|Rest]) ->
  {_, [Output]} = intcode:execute(Prog, [Input2, Input1]),
  chained_execute(Prog, Output, Rest).

find_best_phase_setting(Prog) ->
  ThrustLevels =
    [begin
       PS = [X1, X2, X3, X4, X5],
       chained_execute(Prog, 0, PS)
     end ||
      X1 <- lists:seq(0,4),
      X2 <- lists:seq(0,4),
      X3 <- lists:seq(0,4),
      X4 <- lists:seq(0,4),
      X5 <- lists:seq(0,4),
      X1 =/= X2, X1 =/= X3, X1 =/= X4, X1 =/= X5,
      X2 =/= X3, X2 =/= X4, X2 =/= X5,
      X3 =/= X4, X3 =/= X5,
      X4 =/= X5],

  lists:max(ThrustLevels).

%% --- Tests ---

main_test_() ->
  Prog = intcode:parse(inputs:get_as_string(2019, 7)),
  {"Part 1", ?_assertEqual(70597, find_best_phase_setting(Prog))}.

%% --- Part 1 examples ---

test_helper(InputStr) ->
  Prog = intcode:parse(InputStr),
  find_best_phase_setting(Prog).

ex1_test_() ->
  ProgStr = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0",
  ?_assertEqual(43210, test_helper(ProgStr)).

ex2_test_() ->
  ProgStr =
    "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,"
    "23,1,24,23,23,4,23,99,0,0",
  ?_assertEqual(54321, test_helper(ProgStr)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
