%% aoc_puzzle.hrl

-record(aoc_puzzle,
        { module :: module(),
          year :: integer(),
          day :: integer(),
          name = "" :: string()
        }).

-type aoc_puzzle() :: #aoc_puzzle{}.
