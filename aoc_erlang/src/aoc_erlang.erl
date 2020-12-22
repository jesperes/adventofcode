-module(aoc_erlang).

-export([main/1]).

main(_Args) ->
  {ok, _Pid} = aoc_server:start_link(),
  Puzzles = aoc_puzzle:find_puzzles(all, all),
  io:format("~p~n", [Puzzles]),
  Result = aoc_server:solve(Puzzles),
  io:format("Result = ~p~n", [Result]).
