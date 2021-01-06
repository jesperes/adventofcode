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
  io:format("Running ~p puzzles...", [length(Puzzles)]),
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
  io:format(".", []),
  Repeats = 1,
  MaxMicros = 1000000, %% 1s
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
  {trunc(T / R), V}.

solve_puzzle(Puzzle, InputFileDir) ->
  io:format("[~p]", [Puzzle#aoc_puzzle.day]),
  Binary = get_input(Puzzle, InputFileDir),
  M = Puzzle#aoc_puzzle.module,
  {Exp1, Exp2} = Puzzle#aoc_puzzle.expected,
  {ParseTime, ParsedInput} = run_with_timing(M, parse, [Binary]),
  {Part1Time, Part1Result} = run_with_timing(M, solve1, [ParsedInput]),
  {Part2Time, Part2Result} =
    case Puzzle#aoc_puzzle.day of
      25 -> {0, 0};
      _ ->
        run_with_timing(M, solve2, [ParsedInput])
    end,

  Puzzle#aoc_puzzle{
    parse = ParseTime,
    part1 = Part1Time,
    part2 = Part2Time,
    part1_result = Part1Result,
    part2_result = Part2Result,
    status = { Exp1 =:= Part1Result,
               Exp2 =:= Part2Result }}.

fmt(N) when is_integer(N) ->
  integer_to_list(N);
fmt(N) when is_float(N) ->
  io_lib:format("~.3f", [N]);
fmt(N) when is_atom(N) ->
  io_lib:format("~s", [N]);
fmt(N) when is_list(N) ->
  io_lib:format("~s", [N]);
fmt(N) ->
  io_lib:format("~p", [N]).

tabulate(Solutions) ->
  Sep = ";",

  TotalParse = lists:sum(lists:map(fun(P) -> P#aoc_puzzle.parse end, Solutions)),
  TotalPart1 = lists:sum(lists:map(fun(P) -> P#aoc_puzzle.part1 end, Solutions)),
  TotalPart2 = lists:sum(lists:map(fun(P) -> P#aoc_puzzle.part2 end, Solutions)),
  GrandTotal = lists:sum(lists:map(fun(P) -> (P#aoc_puzzle.parse +
                                                P#aoc_puzzle.part1 +
                                                P#aoc_puzzle.part2) end, Solutions)),

  ParsePerPuzzle = TotalParse / length(Solutions),
  Part1PerPuzzle = TotalPart1 / length(Solutions),
  Part2PerPuzzle = TotalPart2 / length(Solutions),
  TotalPerPuzzle = GrandTotal / length(Solutions),

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
          {Exp1, Exp2} = Puzzle#aoc_puzzle.expected,
          P1 = Puzzle#aoc_puzzle.part1_result,
          P2 = Puzzle#aoc_puzzle.part2_result,
          P1Status =
            if element(1, Puzzle#aoc_puzzle.status) ->
                "OK";
               true ->
                io_lib:format("FAILED (~p != ~p)", [P1, Exp1])
            end,
          P2Status =
            if element(2, Puzzle#aoc_puzzle.status) ->
                "OK";
               true ->
                io_lib:format("FAILED (~p != ~p)", [P2, Exp2])
            end,

          lists:join(Sep,
                     [ %% Year
                       fmt(Puzzle#aoc_puzzle.year)

                       %% Day
                     , fmt(Puzzle#aoc_puzzle.day)

                       %% Name
                     , Puzzle#aoc_puzzle.name

                       %% Parsing
                     , case Puzzle#aoc_puzzle.has_input_file of
                         true -> fmt(Puzzle#aoc_puzzle.parse / 1000.0);
                         false -> "-"
                       end

                       %% Part 1
                     , fmt(Puzzle#aoc_puzzle.part1 / 1000.0)

                       %% Part 2
                     , case Puzzle#aoc_puzzle.day of
                         25 -> "-";
                         _ -> fmt(Puzzle#aoc_puzzle.part2 / 1000.0)
                       end

                       %% Total time
                     , fmt((Puzzle#aoc_puzzle.parse +
                              Puzzle#aoc_puzzle.part1 +
                              Puzzle#aoc_puzzle.part2)
                           / 1000.0)

                       %% Part 1 result
                     , fmt(Puzzle#aoc_puzzle.part1_result)

                       %% Part 2 result
                     , case Puzzle#aoc_puzzle.day of
                        25 -> "-";
                        _ -> fmt(Puzzle#aoc_puzzle.part2_result)
                      end

                       %% Part 1 status
                     , P1Status

                       %% Part 2 status
                     , case Puzzle#aoc_puzzle.day of
                         25 -> "-";
                         _ -> P2Status
                       end
                     ]) ++ "\n"
      end, Solutions) ++ "\n" ++


    lists:join(Sep,
               ["",
                "",
                "Total",
                fmt(TotalParse / 1000.0),
                fmt(TotalPart1 / 1000.0),
                fmt(TotalPart2 / 1000.0),
                fmt(GrandTotal / 1000.0)
                ]) ++ "\n" ++

    lists:join(Sep,
               ["",
                "",
                "Per puzzle",
                fmt(ParsePerPuzzle / 1000.0),
                fmt(Part1PerPuzzle / 1000.0),
                fmt(Part2PerPuzzle / 1000.0),
                fmt(TotalPerPuzzle / 1000.0)
                ]),

  Bin = list_to_binary(Table),

  io:format("~n~n", []),
  io:format("Language: erlang~n", []),
  io:format("System version: ~s~n", [erlang:system_info(system_version)]),
  io:format("~n~s~n", [tabulate(Bin, Sep)]),

  ok = file:write_file("table.csv", Bin),
  tabulate(Bin, Sep, "table.html", "html").

tabulate(Binary, Sep) ->
  TempFile = "/tmp/tabulate" ++ integer_to_list(rand:uniform(1 bsl 127)),
  try
    ok = file:write_file(TempFile, Binary),
    os:cmd("tabulate -s '" ++ Sep ++ "' " ++ TempFile)
  after
    file:delete(TempFile)
  end.

tabulate(Binary, Sep, Filename, Format) ->
  TempFile = "/tmp/tabulate" ++ integer_to_list(rand:uniform(1 bsl 127)),
  try
    ok = file:write_file(TempFile, Binary),
    [] = os:cmd("tabulate -f " ++ Format ++ " -s '" ++ Sep ++ "' -o " ++ Filename ++ " " ++ TempFile)
  after
    file:delete(TempFile)
  end.
