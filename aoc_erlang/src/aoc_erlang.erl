-module(aoc_erlang).

-export([main/1]).

main(_Args) ->
  {ok, _Pid} = aoc_server:start_link(),
  Puzzles = aoc_puzzle:find_puzzles(all, all),
  aoc_server:solve(Puzzles).
