%% Display behavior module for showing AoC puzzles running.
-module(aoc_display).

-include("aoc_puzzle.hrl").

-callback init(Puzzles :: aoc_puzzle_map()) -> {ok, State :: term()}.

-callback update_step_time(PuzzleId :: aoc_puzzle_id(),
                           Part :: parse | part1 | part2,
                           Time :: integer(),
                           State :: term()) -> {ok, NewState :: term()}.

-callback update_part_result(PuzzleId :: aoc_puzzle_id(),
                             Part :: parse | part1 | part2,
                             Result :: term(),
                             State :: term()) -> {ok, NewState :: term()}.

-callback summarize(Puzzles :: aoc_puzzle_map(), State :: term()) -> ok.
