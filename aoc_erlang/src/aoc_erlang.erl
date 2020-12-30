-module(aoc_erlang).

-export([main/1]).

-spec main([term()]) -> no_return().
main(Args) ->
  process_flag(trap_exit, true),
  OptSpecList =
    [{inputs, $i, "inputs", string, "Directory containing input files."}],
  {ok, {Options, _Rest}} = getopt:parse(OptSpecList, Args),
  InputFileDir = proplists:get_value(inputs, Options),
  {ok, Pid} = aoc_server:start_link(InputFileDir, aoc_display_fancy),
  Puzzles = aoc_puzzle:find_puzzles(all, all),
  %% io:format("Found puzzles: ~p~n", [Puzzles]),
  aoc_server:solve(Puzzles),
  wait_for_server(Pid).

%% aoc_server will shutdown when all puzzles are run, so wait for that
%% to happen before exiting.
-spec wait_for_server(pid()) -> no_return().
wait_for_server(Pid) ->
  receive
    {'EXIT', Pid, normal} ->
      erlang:halt(0);
    Other ->
      io:format("Got unexpected msg: ~p~n", [Other]),
      erlang:halt(1)
  end.
