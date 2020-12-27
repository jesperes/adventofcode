-module(aoc_display_plain).

-behavior(aoc_display).
-include("aoc_puzzle.hrl").

-export([ init/1
        , update_step_time/3
        , update_part_result/3
        , summarize/1
        ]).

-spec init(Puzzles :: [aoc_puzzle()]) -> ok.
init(_Puzzles) ->
  io:format("Solving puzzles... ", []).

-spec update_step_time(PuzzleId :: aoc_puzzle_id(),
                       Part :: part1 | part2,
                       Time :: integer()) -> ok.
update_step_time({_Year, _Day}, _Part, _Time) ->
  ok.

-spec update_part_result(PuzzleId :: aoc_puzzle_id(),
                         Part :: part1 | part2,
                         Result :: term()) -> ok.
update_part_result({_Year, _Day}, _Part, _Result) ->
  ok.

%% Format microseconds as microseconds.
fmt_time(undefined) ->
  "--";
fmt_time(T) when is_integer(T) ->
  try
    if T < 1000000 ->
        io_lib:format("~.3g ms", [T / 1000]);
       true ->
        io_lib:format("~.3g s", [T / 1000000])
    end
  catch _:_ ->
      "??"
  end.

safe_sum(L) -> safe_sum(L, 0).

safe_sum([], Acc) -> Acc;
safe_sum([N|Rest], Acc) ->
  try
    safe_sum(Rest, N + Acc)
  catch _:_ ->
      safe_sum(Rest, Acc)
  end.

-spec summarize(Puzzles :: [aoc_puzzle()]) -> ok.
summarize(Puzzles) ->
  io:format("done.~n", []),

  Sep = "@",
  Header = lists:join(Sep, ["Year", "Day", "Name", "Parsing", "Part 1", "Part 2", "Part 1 result", "Part 2 result", "Part 1 status", "Part 2 status"]) ++ "\n",
  Lines =
    lists:sort(
      [lists:join(Sep, [io_lib:format("~w", [P#aoc_puzzle.year]),
                        io_lib:format("~w", [P#aoc_puzzle.day]),
                        io_lib:format("~s", [P#aoc_puzzle.name]),
                        io_lib:format("~s", [fmt_time(P#aoc_puzzle.parse)]),
                        io_lib:format("~s", [fmt_time(P#aoc_puzzle.part1)]),
                        io_lib:format("~s", [fmt_time(P#aoc_puzzle.part2)]),
                        io_lib:format("~p", [P#aoc_puzzle.part1_result]),
                        io_lib:format("~p", [P#aoc_puzzle.part2_result]),
                        io_lib:format("~s", [status_part1(P)]),
                        io_lib:format("~s", [status_part2(P)])
                       ]) ++ "\n"
       || P <- maps:values(Puzzles)]),

  ParseTotal = safe_sum([P#aoc_puzzle.parse || P <- maps:values(Puzzles)]),
  Part1Total = safe_sum([P#aoc_puzzle.part1 || P <- maps:values(Puzzles)]),
  Part2Total = safe_sum([P#aoc_puzzle.part2 || P <- maps:values(Puzzles)]),
  Total =
    lists:join(Sep, ["Total",
                     "",
                     "",
                     io_lib:format("~s", [fmt_time(ParseTotal)]),
                     io_lib:format("~s", [fmt_time(Part1Total)]),
                     io_lib:format("~s", [fmt_time(Part2Total)])
                    ]) ++ "\n",

  TableStr = tabulate(list_to_binary(Header ++ Lines ++ Total), Sep),
  io:format("~n~s~n", [TableStr]).

status_part1(Puzzle) ->
  if Puzzle#aoc_puzzle.part1_result =:= undefined ->
      "-";
     Puzzle#aoc_puzzle.part1_result =:= element(1, Puzzle#aoc_puzzle.expected) ->
      "OK";
     true ->
      io_lib:format("FAILED (expected ~p, got ~p)",
                    [element(1, Puzzle#aoc_puzzle.expected),
                     Puzzle#aoc_puzzle.part1_result])
  end.

status_part2(Puzzle) ->
  if Puzzle#aoc_puzzle.part2_result =:= undefined ->
      "-";
     Puzzle#aoc_puzzle.part2_result =:= element(2, Puzzle#aoc_puzzle.expected) ->
      "OK";
     true ->
      io_lib:format("FAILED (expected ~p, got ~p)",
                    [element(2, Puzzle#aoc_puzzle.expected),
                     Puzzle#aoc_puzzle.part2_result])
  end.



tabulate(Binary, Sep) ->
  TempFile = "/tmp/tabulate" ++ integer_to_list(rand:uniform(1 bsl 127)),
  try
    ok = file:write_file(TempFile, Binary),
    os:cmd("tabulate -f grid -s" ++ Sep ++ " " ++ TempFile)
  after
    file:delete(TempFile)
  end.
