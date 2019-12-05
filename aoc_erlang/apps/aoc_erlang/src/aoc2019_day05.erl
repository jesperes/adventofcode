%%% Advent of Code solution for 2019 day 05.
%%% Created: 2019-12-05T16:25:02+00:00

-module(aoc2019_day05).
-include_lib("eunit/include/eunit.hrl").

-define(OP_ADD, 1).
-define(OP_MUL, 2).
-define(OP_INPUT, 3).
-define(OP_OUTPUT, 4).
-define(OP_JUMP_IF_TRUE, 5).
-define(OP_JUMP_IF_FALSE, 6).
-define(OP_LESS_THAN, 7).
-define(OP_EQUALS, 8).
-define(OP_END, 99).

-define(MODE_POS, 0).
-define(MODE_IMM, 1).

execute(Prog, Input) ->
  execute(Prog, 0, Input, 0).

execute(Prog, PC, In, Out) ->

  R = fun(K) -> maps:get(K, Prog, undefined) end,
  W = fun(K, V) -> maps:put(K, V, Prog) end,

  %% Read with addressing mode
  RM = fun(K, ?MODE_POS) -> R(K);
          (K, ?MODE_IMM) -> K
       end,

  %% Decode the opcode into instruction and addressing mode
  Op0 = R(PC) rem 100,
  M1  = (R(PC) div 100)  rem 10,
  M2  = (R(PC) div 1000) rem 10,

  %% Operands.
  Op1 = R(PC + 1),
  Op2 = R(PC + 2),
  Op3 = R(PC + 3),

  case Op0 of
    ?OP_ADD ->
      execute(W(Op3, RM(Op1, M1) + RM(Op2, M2)), PC + 4, In, Out);
    ?OP_MUL ->
      execute(W(Op3, RM(Op1, M1) * RM(Op2, M2)), PC + 4, In, Out);
    ?OP_JUMP_IF_TRUE ->
      case RM(Op1, M1) of
        0 -> execute(Prog, PC + 3, In, Out);
        _ -> execute(Prog, RM(Op2, M2), In, Out)
      end;
    ?OP_JUMP_IF_FALSE ->
      case RM(Op1, M1) of
        0 -> execute(Prog, RM(Op2, M2), In, Out);
        _ -> execute(Prog, PC + 3, In, Out)
      end;
    ?OP_LESS_THAN ->
      case RM(Op1, M1) < RM(Op2, M2) of
        true  -> execute(W(Op3, 1), PC + 4, In, Out);
        false -> execute(W(Op3, 0), PC + 4, In, Out)
      end;
    ?OP_EQUALS ->
      case RM(Op1, M1) == RM(Op2, M2) of
        true  -> execute(W(Op3, 1), PC + 4, In, Out);
        false -> execute(W(Op3, 0), PC + 4, In, Out)
      end;
    ?OP_INPUT ->
      execute(W(Op1, In), PC + 2, In, Out);
    ?OP_OUTPUT ->
      %% Out should always be 0, except for the very last, which is
      %% what we return.
      ?assertEqual(0, Out),
      execute(Prog, PC + 2, In, RM(Op1, M1));
    ?OP_END ->
      Out
  end.

get_input() ->
  get_input(inputs:get_as_string(2019, 05)).

%% Read input and convert to map from PC (index) -> Value (int).
get_input(InputStr) ->
  Ints = string:tokens(InputStr, ","),
  maps:from_list(
    lists:zip(lists:seq(0, length(Ints) - 1),
              lists:map(fun list_to_integer/1, Ints))).

%% Tests
main_test_() ->
  Input = get_input(),
  [ {"Part 1", ?_assertEqual(16348437, execute(Input, 1))}
  , {"Part 2", ?_assertEqual(6959377, execute(Input, 5))}
  ].

test_helper(InputStr, Input) ->
  Prog = get_input(InputStr),
  execute(Prog, Input).

ex1_test_() ->
  %% Returns its input
  ExProg1 = "3,0,4,0,99",
  ?_assertEqual(42, test_helper(ExProg1, 42)).

ex2_test_() ->
  %% Should return 0
  ExProg2 = "1002,4,3,4,33",
  ?_assertEqual(0,  test_helper(ExProg2, 0)).

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
