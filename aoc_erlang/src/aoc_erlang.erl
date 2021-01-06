-module(aoc_erlang).

-export([main/1]).

-include("aoc_puzzle.hrl").

-mode(compile).

-spec main([term()]) -> no_return().
main(Args) ->
  process_flag(trap_exit, true),
  OptSpecList =
    [{inputs, $i, "inputs", string, "Directory containing input files."}],
  {ok, {Options, _Rest}} = getopt:parse(OptSpecList, Args),
  InputFileDir = proplists:get_value(inputs, Options),
  Puzzles = aoc_puzzle:find_puzzles(all, all),
  Solutions =
    lists:map(
      fun(P) ->
          solve_puzzle(P, InputFileDir)
      end, Puzzles),

  tabulate(Solutions),
  erlang:halt(0).

get_input(#aoc_puzzle{year = Year,
                      day = Day,
                      has_input_file = HasInput},
          InputFileDir) ->
  Filename =
    lists:flatten(
      io_lib:format("~s/inputs/~w/input~2..0w.txt",
                    [InputFileDir, Year, Day])),
  case HasInput of
    true -> {ok, Binary} = file:read_file(Filename), Binary;
    false -> <<>>
  end.

timestamp() ->
  {Mega, Secs, Micro} = erlang:timestamp(),
  Micro + (1000000 * Secs) + (1000000000000 * Mega).

run_with_timing(M, F, A) ->
  io:format("~p ~p... ", [M, F]),
  Repeats = 10,
  MaxMicros = 1000000,
  Start = timestamp(),
  {T, R, V, _} =
    lists:foldl(
      fun(_, {_, _, _, stop} = Acc) ->
          Acc;
         (_, {TotalTime, TotalRepeats, Result, Cont}) ->
          {T, Result0} = timer:tc(M, F, A),

          if (Result =/= undefined) andalso (Result0 =/= Result) ->
              throw({result_mismatch, Result0, Result});
             true -> ok
          end,

          case timestamp() of
            TS when TS - Start > MaxMicros ->
              {TotalTime + T, TotalRepeats + 1, Result0, stop};
            _ ->
              {TotalTime + T, TotalRepeats + 1, Result0, Cont}
          end
      end, {0, 0, undefined, continue}, lists:seq(1, Repeats)),
  io:format("(~p repeats, ~.3f ms)~n", [R, T / 1000.0]),
  {trunc(T / R), V}.

solve_puzzle(Puzzle, InputFileDir) ->
  Binary = get_input(Puzzle, InputFileDir),
  M = Puzzle#aoc_puzzle.module,
  {ParseTime, ParsedInput} = run_with_timing(M, parse, [Binary]),
  {Exp1, Exp2} = Puzzle#aoc_puzzle.expected,
  {Part1Time, Part1Result} = run_with_timing(M, solve1, [ParsedInput]),
  {Part2Time, Part2Result} = run_with_timing(M, solve2, [ParsedInput]),

  Puzzle#aoc_puzzle{
    parse = ParseTime,
    part1 = Part1Time,
    part2 = Part2Time,
    part1_result = Part1Result,
    part2_result = Part2Result,
    status = { Exp1 =:= Part1Result,
               Exp2 =:= Part2Result }}.

tabulate(Solutions) ->
  Sep = ",",
  Table =
    lists:join(Sep,
               ["Year",
                "Day",
                "Name",
                "Parsing",
                "Part 1",
                "Part 2",
                "Total",
                "Part 1 result",
                "Part 2 result",
                "Part 1 status",
                "Part 2 status"]) ++ "\n" ++

    lists:map(
      fun(Puzzle) ->
          lists:join(Sep,
                     [io_lib:format("~p", [Puzzle#aoc_puzzle.year]),
                      io_lib:format("~p", [Puzzle#aoc_puzzle.day]),
                      Puzzle#aoc_puzzle.name,
                      io_lib:format("~.3f", [Puzzle#aoc_puzzle.parse / 1000.0]),
                      io_lib:format("~.3f", [Puzzle#aoc_puzzle.part1 / 1000.0]),
                      io_lib:format("~.3f", [Puzzle#aoc_puzzle.part2 / 1000.0]),
                      io_lib:format("~.3f", [(Puzzle#aoc_puzzle.parse +
                                                Puzzle#aoc_puzzle.part1 +
                                                Puzzle#aoc_puzzle.part2)
                                             / 1000.0]),
                      io_lib:format("~p", [Puzzle#aoc_puzzle.part1_result]),
                      io_lib:format("~p", [Puzzle#aoc_puzzle.part2_result]),
                      if element(1, Puzzle#aoc_puzzle.status) -> "OK"; true -> "FAILED" end,
                      if element(2, Puzzle#aoc_puzzle.status) -> "OK"; true -> "FAILED" end]) ++ "\n"
      end, Solutions) ++ "\n" ++

    lists:join(Sep,
               ["",
                "",
                "Total",
                io_lib:format("~.3f", [lists:sum(lists:map(fun(P) -> P#aoc_puzzle.parse end, Solutions)) / 1000.0]),
                io_lib:format("~.3f", [lists:sum(lists:map(fun(P) -> P#aoc_puzzle.part1 end, Solutions)) / 1000.0]),
                io_lib:format("~.3f", [lists:sum(lists:map(fun(P) -> P#aoc_puzzle.part2 end, Solutions)) / 1000.0]),
                io_lib:format("~.3f", [lists:sum(lists:map(fun(P) -> (P#aoc_puzzle.parse +
                                                                        P#aoc_puzzle.part1 +
                                                                        P#aoc_puzzle.part2) end, Solutions)) / 1000.0])
                ]),

  io:format("All times are in milliseconds.~n", []),
  io:format("~s~n", [tabulate(list_to_binary(Table), Sep)]).

tabulate(Binary, Sep) ->
  TempFile = "/tmp/tabulate" ++ integer_to_list(rand:uniform(1 bsl 127)),
  try
    ok = file:write_file(TempFile, Binary),
    os:cmd("tabulate -f grid -s" ++ Sep ++ " " ++ TempFile)
  after
    file:delete(TempFile)
  end.
