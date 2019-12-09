%%% Advent of Code solution for 2019 day 09.
%%% Created: 2019-12-09T05:18:15+00:00

-module(aoc2019_day09).
-include_lib("eunit/include/eunit.hrl").

%% --- [ Implementation of IntCode computer ] ---

-define(OP_ADD, 1).
-define(OP_MUL, 2).
-define(OP_INPUT, 3).
-define(OP_OUTPUT, 4).
-define(OP_JUMP_IF_TRUE, 5).
-define(OP_JUMP_IF_FALSE, 6).
-define(OP_LESS_THAN, 7).
-define(OP_EQUALS, 8).
-define(OP_ADJ_RELBASE, 9).
-define(OP_END, 99).

-define(MODE_POS, 0).
-define(MODE_IMM, 1).
-define(MODE_REL, 2).

%% Execute an IntCode program.
%%
%% @param   Prog    The IntCode program to execute.
%% @param   Input   A single integer to provide to the INPUT instruction.
%% @returns Output  A list of integers outputed by the OUTPUT instruction.
-spec execute(Prog :: map() ,
              Input :: integer()) ->
                 Output :: list(integer()).
execute(Prog, Input) ->
  execute(Prog, 0, 0, Input, []).

execute(Prog, PC, RelBase, In, Out) ->

  %% Read
  R = fun(K) -> maps:get(K, Prog, 0) end,

  %% Write (POS and REL)
  W = fun(K, ?MODE_POS, V) -> maps:put(K, V, Prog);
         (K, ?MODE_REL, V) -> maps:put(K + RelBase, V, Prog)
      end,

  %% Read (POS, REL, and IMM)
  RM = fun(K, ?MODE_POS) -> R(K);
          (K, ?MODE_REL) -> R(K + RelBase);
          (K, ?MODE_IMM) -> K
       end,

  %% Decode the opcode into instruction and addressing mode
  Op0 = R(PC) rem 100,
  M1  = (R(PC) div 100)   rem 10,
  M2  = (R(PC) div 1000)  rem 10,
  M3  = (R(PC) div 10000) rem 10,

  %% Operands
  Op1 = R(PC + 1),
  Op2 = R(PC + 2),
  Op3 = R(PC + 3),

  case Op0 of
    ?OP_ADD ->
      execute(W(Op3, M3, RM(Op1, M1) + RM(Op2, M2)), PC + 4, RelBase, In, Out);
    ?OP_MUL ->
      execute(W(Op3, M3, RM(Op1, M1) * RM(Op2, M2)), PC + 4, RelBase, In, Out);
    ?OP_JUMP_IF_TRUE ->
      case RM(Op1, M1) of
        0 -> execute(Prog, PC + 3, RelBase, In, Out);
        _ -> execute(Prog, RM(Op2, M2), RelBase, In, Out)
      end;
    ?OP_JUMP_IF_FALSE ->
      case RM(Op1, M1) of
        0 -> execute(Prog, RM(Op2, M2), RelBase, In, Out);
        _ -> execute(Prog, PC + 3, RelBase, In, Out)
      end;
    ?OP_LESS_THAN ->
      case RM(Op1, M1) < RM(Op2, M2) of
        true  -> execute(W(Op3, M3, 1), PC + 4, RelBase, In, Out);
        false -> execute(W(Op3, M3, 0), PC + 4, RelBase, In, Out)
      end;
    ?OP_EQUALS ->
      case RM(Op1, M1) == RM(Op2, M2) of
        true  -> execute(W(Op3, M3, 1), PC + 4, RelBase, In, Out);
        false -> execute(W(Op3, M3, 0), PC + 4, RelBase, In, Out)
      end;
    ?OP_ADJ_RELBASE ->
      execute(Prog, PC + 2, RelBase + RM(Op1, M1), In, Out);
    ?OP_INPUT ->
      execute(W(Op1, M1, In), PC + 2, RelBase, In, Out);
    ?OP_OUTPUT ->
      execute(Prog, PC + 2, RelBase, In, [RM(Op1, M1)|Out]);
    ?OP_END ->
      lists:reverse(Out)
  end.

%% Puzzle solution
part1(Input) ->
  [Out] = execute(Input, 1),
  Out.

part2(Input) ->
  [Out] = execute(Input, 2),
  Out.

%% --- [ Input/parsing ] ---

get_input() ->
  get_input(inputs:get_as_string(2019, 09)).

%% Read input and convert to map from PC (index) -> Value (int).
get_input(InputStr) ->
  Ints = string:tokens(InputStr, ","),
  maps:from_list(
    lists:zip(lists:seq(0, length(Ints) - 1),
              lists:map(fun list_to_integer/1, Ints))).

%% --- [ Tests ] ---

main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(2594708277, part1(Input))}
  , {"Part 2", ?_assertEqual(87721, part2(Input))}
  ].

ex1_test_() ->
  Prog = get_input("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"),
  Output = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99],
  ?_assertEqual(Output, execute(Prog, 0)).

ex2_test_() ->
  Prog = get_input("104,1125899906842624,99"),
  ?_assertEqual([1125899906842624], execute(Prog, 0)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
