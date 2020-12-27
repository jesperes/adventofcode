%% Display behavior module for showing AoC puzzles running.
-module(aoc_display).

-include("aoc_puzzle.hrl").

-callback init(Puzzles :: [aoc_puzzle()]) -> ok.

-callback update_step_time(PuzzleId :: aoc_puzzle_id(),
                           Part :: parse | part1 | part2,
                           Time :: integer()) -> ok.

-callback update_part_result(PuzzleId :: aoc_puzzle_id(),
                             Part :: parse | part1 | part2,
                             Result :: term()) -> ok.

-callback summarize(Puzzles :: [aoc_puzzle()]) -> ok.
