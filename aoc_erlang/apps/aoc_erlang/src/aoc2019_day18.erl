%%% Advent of Code solution for 2019 day 18.
%%% Created: 2019-12-18T18:36:27+00:00

-module(aoc2019_day18).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(_Input) ->
  %%
  %% Only one entrance (marked @) is present among the open passages
  %% (marked .) and stone walls (#), but you also detect an assortment
  %% of keys (shown as lowercase letters) and doors (shown as
  %% uppercase letters). Keys of a given letter open the door of the
  %% same letter: a opens A, b opens B, and so on. You aren't sure
  %% which key you need to disable the tractor beam, so you'll need to
  %% collect all of them.
  %%
  %% Example:
  %%
  %% #################
  %% #i.G..c...e..H.p#
  %% ########.########
  %% #j.A..b...f..D.o#
  %% ########@########
  %% #k.E..a...g..B.n#
  %% ########.########
  %% #l.F..d...h..C.m#
  %% #################
  %%
  %% Do shortest path search (Dijkstra?) with nodes being {KeyPos,
  %% KeysDiscovered}. The keys discovered affects how what paths to
  %% other nodes are available, so a key reached through a different
  %% set of keys represents a different node in the search graph.
  %%
  %% BFS can be used to find all adjacent nodes, so from {KeyPos,
  %% KeysDiscovered}, we perform a BFS to find all keys which can be
  %% reached from KeyPos, given the set of keys already discovered.
  %%
  ?debugMsg("Not implemented."),
  0.

part2(_Input) ->
  ?debugMsg("Not implemented."),
  0.

%% Input reader (place downloaded input file in
%% priv/inputs/2019/input18.txt).
get_input() ->
  inputs:get_as_binary(2019, 18).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(0, part1(Input))}
  , {"Part 2", ?_assertEqual(0, part2(Input))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
