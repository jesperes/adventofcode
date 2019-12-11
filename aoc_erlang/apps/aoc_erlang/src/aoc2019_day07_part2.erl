%%% Advent of Code solution for 2019 day 07.
%%% Created: 2019-12-05T16:25:02+00:00

%% Part 2 of day 7 solution.

-module(aoc2019_day07_part2).
-include_lib("eunit/include/eunit.hrl").

%% Internal exports
-export([amp/4]).

%%% --- Part 2 ---
amp(Parent, Amp, DestAmp, Prog) when is_atom(Amp) ->
  register(Amp, self()),

  %% Register ourselves, and wait for start signal (so all other
  %% amplifiers are also registered.
  Parent ! {self(), registered},
  receive start -> ok end,

  DestPid = whereis(DestAmp),
  true = is_pid(DestPid),

  {_, FinalState} =
    intcode:execute(
      Prog,
      fun(S) ->
          %% Wait for next input and provide it to the input
          %% instruction.
          receive Input -> {S, Input} end
      end,
      fun(Out, State) ->
          %% Send output values to the destination pid
          DestPid ! Out,

          %% Store the last seen output in the state, so that we can
          %% fish it out when the program exits.
          maps:put(out, Out, State)
      end,
      #{}),

  Parent ! {self(), maps:get(out, FinalState)}.

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
  intcode:parse(inputs:get_as_string(2019, 7)).

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
  Prog = intcode:parse(ProgStr),
  ?_assertEqual(139629729,
                find_best_phase_setting(Prog)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
