%%% Advent of Code solution for 2019 day 07.
%%% Created: 2019-12-05T16:25:02+00:00

%% Solution based on the day 5 solution.

-module(aoc2019_day07).
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

%% This is identical to the day 5 solution, except it can take a list
%% of inputs with one element for each executed input instruction.
-spec execute(Prog :: map(),
              In :: list()) ->
                 Out :: integer().
execute(Prog, In) ->
  execute(Prog, 0, In, 0).

execute(Prog, PC, In, Out) ->
  R = fun(K) -> maps:get(K, Prog, undefined) end,
  W = fun(K, V) -> maps:put(K, V, Prog) end,

  %% Read with addressing mode
  RM = fun(K, ?MODE_POS) -> R(K);
          (K, ?MODE_IMM) -> K
       end,

  %% Decode the opcode into instruction and addressing mode
  RPC = R(PC),
  Op0 = RPC rem 100,
  M1  = (RPC div 100)  rem 10,
  M2  = (RPC div 1000) rem 10,

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
      [Input|Rest] = In,
      execute(W(Op1, Input), PC + 2, Rest, Out);
    ?OP_OUTPUT ->
      %% Out should always be 0, except for the very last, which is
      %% what we return.
      ?assertEqual(0, Out),
      execute(Prog, PC + 2, In, RM(Op1, M1));
    ?OP_END ->
      Out
  end.

%%% --- PART 1 ---

chained_execute(_Prog, Input1, []) ->
  Input1;
chained_execute(Prog, Input1, [Input2|Rest]) ->
  Output = execute(Prog, [Input2, Input1]),
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

%%% --- Helpers ---

get_input() ->
  get_input(inputs:get_as_string(2019, 07)).

%% Read input and convert to map from PC (index) -> Value (int).
get_input(InputStr) ->
  Ints = string:tokens(InputStr, ","),
  maps:from_list(
    lists:zip(lists:seq(0, length(Ints) - 1),
              lists:map(fun list_to_integer/1, Ints))).

%% --- Tests ---
main_test_() ->
  Prog = get_input(),
  {"Part 1", ?_assertEqual(70597, find_best_phase_setting(Prog))}.

%% --- Part 1 examples ---

test_helper(InputStr) ->
  Prog = get_input(InputStr),
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
