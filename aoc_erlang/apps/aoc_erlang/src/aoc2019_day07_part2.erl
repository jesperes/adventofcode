%%% Advent of Code solution for 2019 day 07.
%%% Created: 2019-12-05T16:25:02+00:00

%% Part 2 of day 7 solution.

-module(aoc2019_day07_part2).
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

%% Internal exports
-export([amp/4]).

%% Inputs and outputs are here provided through funs
-spec execute(Prog :: map(),
              In :: fun(() -> integer()),
              Out :: fun((integer()) -> ok)) -> ok.
execute(Prog, In, Out) ->
  execute(Prog, 0, In, Out, 0).

execute(Prog, PC, In, Out, LastOut) ->
  R = fun(K) -> maps:get(K, Prog, undefined) end,
  W = fun(K, V) -> maps:put(K, V, Prog) end,

  %% Read with addressing mode
  RM = fun(K, ?MODE_POS) -> R(K);
          (K, ?MODE_IMM) -> K
       end,

  %% ?trace("prog ~s",
  %%       [lists:map(fun({K, V}) when K =:= PC ->
  %%                      io_lib:format("~p:(~p=~p) ", [K, V, describe(R(K))]);
  %%                     ({K, V}) ->
  %%                      io_lib:format("~p:~p ", [K, V])
  %%                  end, lists:sort(maps:to_list(Prog)))]),

  %% Decode the opcode into instruction and addressing mode
  RPC = R(PC),
  Op0 = RPC rem 100,
  M1  = (RPC div 100)  rem 10,
  M2  = (RPC div 1000) rem 10,

  %% Operands.
  Op1 = R(PC + 1),
  Op2 = R(PC + 2),
  Op3 = R(PC + 3),

  %% ?trace("pc=~p instr=~p op1=~p (~p) op2=~p (~p) op3=~p",
  %%       [ PC
  %%       , describe(Op0)
  %%       , Op1, describe_mode(M1)
  %%       , Op2, describe_mode(M2)
  %%       , Op3
  %%       ]),

  case Op0 of
    ?OP_ADD ->
      execute(W(Op3, RM(Op1, M1) + RM(Op2, M2)), PC + 4, In, Out, LastOut);
    ?OP_MUL ->
      execute(W(Op3, RM(Op1, M1) * RM(Op2, M2)), PC + 4, In, Out, LastOut);
    ?OP_JUMP_IF_TRUE ->
      case RM(Op1, M1) of
        0 -> execute(Prog, PC + 3, In, Out, LastOut);
        _ -> execute(Prog, RM(Op2, M2), In, Out, LastOut)
      end;
    ?OP_JUMP_IF_FALSE ->
      case RM(Op1, M1) of
        0 -> execute(Prog, RM(Op2, M2), In, Out, LastOut);
        _ -> execute(Prog, PC + 3, In, Out, LastOut)
      end;
    ?OP_LESS_THAN ->
      case RM(Op1, M1) < RM(Op2, M2) of
        true  -> execute(W(Op3, 1), PC + 4, In, Out, LastOut);
        false -> execute(W(Op3, 0), PC + 4, In, Out, LastOut)
      end;
    ?OP_EQUALS ->
      case RM(Op1, M1) == RM(Op2, M2) of
        true  -> execute(W(Op3, 1), PC + 4, In, Out, LastOut);
        false -> execute(W(Op3, 0), PC + 4, In, Out, LastOut)
      end;
    ?OP_INPUT ->
      %% ?debugFmt("Waiting for input...", []),
      Input = In(),
      %% ?trace("Received input ~p", [Input]),
      %% ?trace("Reading input ~p -> writing to pos ~p", [Input, Op1]),
      execute(W(Op1, Input), PC + 2, In, Out, LastOut);
    ?OP_OUTPUT ->
      %% ?trace("Reading output from pos ~p -> ~p", [Op1, RM(Op1, M1)]),
      LastOut0 = RM(Op1, M1),
      Out(LastOut0),
      execute(Prog, PC + 2, In, Out, LastOut0);
    ?OP_END ->
      LastOut
  end.

%%% --- Part 2 ---

amp(Parent, Amp, DestAmp, Prog) when is_atom(Amp) ->
  register(Amp, self()),

  %% Register ourselves, and wait for start signal (so all other
  %% amplifiers are also registered.
  Parent ! {self(), registered},
  receive start -> ok end,

  DestPid = whereis(DestAmp),
  true = is_pid(DestPid),

  FinalOut =
    execute(Prog,
            fun() ->
                receive Input -> Input end
            end,
            fun(Out) ->
                DestPid ! Out
            end),

  Parent ! {self(), FinalOut}.

create_amplifiers(Prog, PhaseSettings) ->
  Params = lists:zip([{a, b},
                      {b, c},
                      {c, d},
                      {d, e},
                      {e, a}], PhaseSettings),
  Parent = self(),
  [A|_] = Pids =
    lists:map(fun({{Amp, DestAmp}, PS}) ->
                  Pid = spawn(?MODULE, amp, [Parent, Amp, DestAmp, Prog]),
                  Pid ! PS,
                  Pid
              end, Params),

  %% Sync amplifiers so they all have started before continuing (so no
  %% messages get lost).
  lists:foreach(fun(Pid) ->
                    receive {Pid, registered} -> ok end
                end, Pids),
  lists:foreach(fun(Pid) -> Pid ! start end, Pids),

  %% Send start signal to 'a'
  A ! 0,

  %% Wait for output values, saving the one from the last amplifier.
  LastAmpPid = lists:last(Pids),

  FinalOut =
    lists:foldl(
      fun(Pid, Acc) ->
          receive
            {LastAmpPid, FinalOut} -> FinalOut;
            {Pid, _FinalOut} -> Acc
          end
      end, undefined, Pids),
  FinalOut.

feedback_loop(Prog, PhaseSettings) ->
  create_amplifiers(Prog, PhaseSettings).

find_best_phase_setting(Prog) ->
  ThrustLevels =
    [begin
       PS = [X1, X2, X3, X4, X5],
       feedback_loop(Prog, PS)
     end ||
      X1 <- lists:seq(5,9),
      X2 <- lists:seq(5,9),
      X3 <- lists:seq(5,9),
      X4 <- lists:seq(5,9),
      X5 <- lists:seq(5,9),
      X1 =/= X2, X1 =/= X3, X1 =/= X4, X1 =/= X5,
      X2 =/= X3, X2 =/= X4, X2 =/= X5,
      X3 =/= X4, X3 =/= X5,
      X4 =/= X5],

  lists:max(ThrustLevels).

%%% --- Helpers ---

get_input() ->
  get_input(inputs:get_as_string(2019, 7)).

%% Read input and convert to map from PC (index) -> Value (int).
get_input(InputStr) ->
  Ints = string:tokens(InputStr, ","),
  maps:from_list(
    lists:zip(lists:seq(0, length(Ints) - 1),
              lists:map(fun list_to_integer/1, Ints))).

%% --- Tests ---
main_test_() ->
  Prog = get_input(),
  {"Part 1", ?_assertEqual(30872528,
                           find_best_phase_setting(Prog))}.

%% --- Part 2 examples ---
part2_ex1_test_() ->
  ProgStr =
    "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,"
    "27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5",
  Prog = get_input(ProgStr),
  ?_assertEqual(139629729,
                find_best_phase_setting(Prog)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
