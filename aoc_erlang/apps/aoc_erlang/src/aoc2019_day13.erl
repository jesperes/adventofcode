%%% Advent of Code solution for 2019 day 13.
%%% Created: 2019-12-13T06:37:49+00:00

-module(aoc2019_day13).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Prog) ->
  {_, Output} = intcode:execute(Prog),
  Output0 = lists:reverse(Output),
  io:format("~p~n", [Output0]),
  count_blocks(Output0, #{}).

count_blocks([], Map) -> maps:size(Map);
count_blocks([X, Y, 2|Rest], Map) ->
  count_blocks(Rest, maps:put({X, Y}, 1, Map));
count_blocks([_, _, _|Rest], Map) ->
  count_blocks(Rest, Map).

%% part2(_Input) ->
%%   ?debugMsg("Not implemented."),
%%   0.

%% Input reader (place downloaded input file in
%% priv/inputs/2019/input13.txt).
get_input() ->
  intcode:parse(inputs:get_as_string(2019, 13)).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(236, part1(Input))}
  %% , {"Part 2", ?_assertEqual(0, part2(Input))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
