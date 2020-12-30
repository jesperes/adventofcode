%% aoc_puzzle.hrl

-type aoc_puzzle_status() :: {boolean() | unknown, boolean() | unknown}.

-record(aoc_puzzle,
        { module :: module(),
          year :: integer(),
          day :: integer(),
          name = "" :: string(),
          has_input_file = true :: boolean(),

          parse :: integer() | undefined,
          part1 :: integer() | undefined,
          part2 :: integer() | undefined,

          part1_result :: term(),
          part2_result :: term(),

          expected = {unknown, unknown} :: {term(), term()},
          status = {unknown, unknown}   :: aoc_puzzle_status()
        }).

-type aoc_puzzle() :: #aoc_puzzle{}.
-type aoc_puzzle_id() :: {integer(), integer()}.
-type aoc_puzzle_map() :: #{aoc_puzzle_id() => aoc_puzzle()}.
