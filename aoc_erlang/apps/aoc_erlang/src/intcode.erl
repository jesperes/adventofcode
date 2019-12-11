%%% Intcode computer implementation for the 2019 puzzles.
-module(intcode).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([
          execute/1
        , execute/2
        , execute/4
        , parse/1
        ]).

%% Opcode definitions
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

%% Addressing modes
-define(MODE_POS, 0).
-define(MODE_IMM, 1).
-define(MODE_REL, 2).

-type intcode_state() :: term().
-type intcode_program() :: map().

%% Input fun receives the state, and returns a new state + an input
%% value.
-type intcode_input() :: fun((State :: intcode_state())
                             -> {NewState :: intcode_state(),
                                 Input :: integer()}).

%% Output fun receives the output value and the state, and returns a
%% new state.
-type intcode_output() :: fun((Output :: integer(),
                               State :: integer())
                              -> NewState :: intcode_state()).


%% Execute an IntCode program. No inputs can be provided, and all the
%% outputs are returned in a list.
-spec execute(Prog :: intcode_program()) -> Outputs :: list(integer()).
execute(Prog) ->
  execute(Prog, []).

%% Execute an IntCode program. Inputs are passed as list, and output
%% values are returned when the program exits (most recent output
%% first).
%%
%% @param Prog     The IntCode program to execute.
%% @param Input    List of integers to supply as input.
%% @returns Output List of integers the program sent to the OUTPUT
%%                 instruction.
-spec execute(Prog :: intcode_program(),
              Input :: list(integer())) ->
                 {ProgOut :: intcode_program(),
                  Output :: list(integer())}.
execute(Prog, Input) ->
  {ProgOut, {_, Outputs0}} =
    execute(Prog,
            fun({[N|Inputs], Outputs} = _State) ->
                {{Inputs, Outputs}, N}
            end,
            fun(Output, {Inputs, Outputs} = _State) ->
                {Inputs, [Output|Outputs]}
            end,
            {Input, []}),
  {ProgOut, Outputs0}.

%% Execute an IntCode program. Input and output are communicated
%% through funs which is passed a state which can be modified and
%% returned by the input/output funs.
%%
%% @param   Prog      The IntCode program to execute.
%% @param   Input     Fun to provide input values
%% @param   Output    Fun to receive output values
%% @param   State     Initial state
%% @returns {P,S}     The program and the end state. The program may
%%                    modify itself, and this is used by some puzzles.
-spec execute(Prog :: intcode_program(),
              Input :: intcode_input(),
              Output :: intcode_output(),
              InitState :: term()) ->
                 {intcode_program(), intcode_state()}.
execute(Prog, Input, Output, State) ->
  execute(Prog, 0, 0, Input, Output, State).

execute(Prog, PC, RelBase, In, Out, State) ->

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
      execute(W(Op3, M3, RM(Op1, M1) + RM(Op2, M2)), PC + 4, RelBase, In, Out, State);
    ?OP_MUL ->
      execute(W(Op3, M3, RM(Op1, M1) * RM(Op2, M2)), PC + 4, RelBase, In, Out, State);
    ?OP_JUMP_IF_TRUE ->
      case RM(Op1, M1) of
        0 -> execute(Prog, PC + 3, RelBase, In, Out, State);
        _ -> execute(Prog, RM(Op2, M2), RelBase, In, Out, State)
      end;
    ?OP_JUMP_IF_FALSE ->
      case RM(Op1, M1) of
        0 -> execute(Prog, RM(Op2, M2), RelBase, In, Out, State);
        _ -> execute(Prog, PC + 3, RelBase, In, Out, State)
      end;
    ?OP_LESS_THAN ->
      case RM(Op1, M1) < RM(Op2, M2) of
        true  -> execute(W(Op3, M3, 1), PC + 4, RelBase, In, Out, State);
        false -> execute(W(Op3, M3, 0), PC + 4, RelBase, In, Out, State)
      end;
    ?OP_EQUALS ->
      case RM(Op1, M1) == RM(Op2, M2) of
        true  -> execute(W(Op3, M3, 1), PC + 4, RelBase, In, Out, State);
        false -> execute(W(Op3, M3, 0), PC + 4, RelBase, In, Out, State)
      end;
    ?OP_ADJ_RELBASE ->
      execute(Prog, PC + 2, RelBase + RM(Op1, M1), In, Out, State);
    ?OP_INPUT ->
      {State0, Input} = In(State),
      ?assert(is_map(State0)),
      execute(W(Op1, M1, Input), PC + 2, RelBase, In, Out, State0);
    ?OP_OUTPUT ->
      State0 = Out(RM(Op1, M1), State),
      ?assert(is_map(State0)),
      execute(Prog, PC + 2, RelBase, In, Out, State0);
    ?OP_END ->
      {Prog, State}
  end.

%% Parse an intcode program and return it as a map from memory
%% locations (integers) to values (integers).
-spec parse(Prog :: binary()
                  | string()) ->
               map().
parse(Binary) when is_binary(Binary) ->
  parse(binary_to_list(Binary));
parse(String) when is_list(String) ->
  Ints = string:tokens(String, ","),
  maps:from_list(
    lists:zip(lists:seq(0, length(Ints) - 1),
              lists:map(fun list_to_integer/1,
                        lists:map(fun string:trim/1, Ints)))).
