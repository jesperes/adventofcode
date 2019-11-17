-module(aoc2016_day10).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_INPUT,
        ["value 5 goes to bot 2",
         "bot 2 gives low to bot 1 and high to bot 0",
         "value 3 goes to bot 1",
         "bot 1 gives low to output 1 and high to bot 0",
         "bot 0 gives low to output 2 and high to output 0",
         "value 2 goes to bot 2"]).

parse_line(Line) ->
  [First|Rest] = string:lexemes(Line, " "),
  case First of
    "value" ->
      [Value, "goes", "to", "bot", Bot] = Rest,
      {goes_to,
       list_to_integer(Value),
       list_to_integer(Bot)};
    "bot" ->
      [Bot, "gives", "low", "to", LowType, LowDest, "and", "high", "to", HighType, HighDest] =
        Rest,
      {bot_to_bot,
       list_to_integer(Bot),
       list_to_atom(LowType),
       list_to_integer(LowDest),
       list_to_atom(HighType),
       list_to_integer(HighDest)}
  end.

parse_input(Lines) ->
  lists:map(fun parse_line/1, Lines).

%% Process instructions. Returns a tuple {RemainingInstructions, State}.
-spec process(Instrs :: list(),
              InstrAcc :: list(),
              State :: map()) ->
                 {list(), map()}.
process([], InstrAcc, State) ->
  {lists:reverse(InstrAcc), State};
process([{goes_to, Value, Bot}|Rest], InstrAcc, State) ->
  %% Put value into bot, consume instruction.
  process(Rest, InstrAcc, send_to(Value, bot, Bot, State));
process([{bot_to_bot, Bot, LowType, LowDest, HighType, HighDest} = Instr|Rest], InstrAcc, State) ->
  case maps:get({bot, Bot}, State, []) of
    Values when length(Values) < 2 ->
      %% Bot has nothing to do yet. Add instruction to accumulator so
      %% it is kept the next round.
      process(Rest, [Instr|InstrAcc], State);
    Values when length(Values) == 2 ->
      %% Bot has two chips, pass them on. Consume the instruction.
      [Low, High] = lists:sort(Values),
      State1 = send_to(Low, LowType, LowDest, State),
      State2 = send_to(High, HighType, HighDest, State1),
      process(Rest, InstrAcc, State2)
  end.

send_to(Value, Type, Dest, State) ->
  maps:update_with({Type, Dest},
                   fun(Old) -> [Value|Old] end,
                   [Value], State).

run_passes(Instrs) ->
  run_passes(Instrs, #{}).

%% Process the list of instructions until all instructions have been
%% processed.
run_passes([], Map) ->
  Map;
run_passes(Instrs, Map0) ->
  {InstrsOut, Map1} = process(Instrs, [], Map0),
  run_passes(InstrsOut, Map1).

%% Solve parts 1 and 2.
solve(Instr) ->
  State = run_passes(Instr),
  maps:fold(
    fun({bot, Bot}, V, {_, Part2} = Acc) ->
        %% Part 1: find the bot containing chips 17 and 61.
        case lists:sort(V) of
          [17,61] -> {Bot, Part2};
          _ -> Acc
        end;
       ({output, N}, [V], {Part1, Part2}) when (N >= 0) and (N =< 2) ->
        %% Part 2: multiply all values in the output bins {0, 1, 2}.
        {Part1, Part2 * V};
       (_, _, Acc) -> Acc
    end,
    {0, 1}, State).

%% ===========================================================================
%% Tests
%% ===========================================================================

parse_input_test() ->
  ?assertEqual([ {bot_to_bot, 2, bot, 1, bot, 0}
               , {goes_to, 1, 2}
               ],
               parse_input([ "bot 2 gives low to bot 1 and high to bot 0"
                           , "value 1 goes to bot 2"
                           ])).

run_passes_test_() ->
  ?_assertEqual(#{{bot,0} => [3,5],
                  {bot,1} => [2,3],
                  {bot,2} => [2,5],
                  {output,0} => [5],
                  {output,1} => [2],
                  {output,2} => [3]},
                run_passes(parse_input(?TEST_INPUT))).

main_test_() ->
  Instrs = parse_input(inputs:get_as_lines(2016, 10)),
  {"Part 1 & 2", ?_assertEqual({161, 133163}, solve(Instrs))}.
