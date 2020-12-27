%% aoc_puzzle.hrl

-record(aoc_puzzle,
        { module :: module(),
          year :: integer(),
          day :: integer(),
          name = "" :: string(),

          parse :: integer(),
          part1 :: integer(),
          part2 :: integer(),

          result = {unknown, unknown}   :: {term(), term()},
          expected = {unknown, unknown} :: {term(), term()}
        }).

-type aoc_puzzle() :: #aoc_puzzle{}.
-type aoc_puzzle_id() :: {integer(), integer()}.
