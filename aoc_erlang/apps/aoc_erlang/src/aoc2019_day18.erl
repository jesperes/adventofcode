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

%% Find keys reachable from Pos, holding Keys.
find_keys(Grid, Pos, Keys) ->
  ok.

get_input() ->
  inputs:get_as_binary(2019, 18).

%% ============================================================
%% Parsing text input into a map
%% ============================================================

parse(Binary) ->
  [{Width, _}|_] = binary:matches(Binary, <<"\n">>),
  ?assertEqual($\n, binary:at(Binary, Width)),
  String = binary_to_list(Binary),
  parse(String, 0, Width, #{ width => Width
                           , height => byte_size(Binary) div Width
                           }).

parse([], _, Width, Grid) ->
  Grid;
parse([$\n|Rest], N, Width, Grid) ->
  parse(Rest, N + 1, Width, Grid);
parse([C|Rest], N, Width, Grid) ->
  Pos = xy_from_offset(N, Width),
  Atm = list_to_atom([C]),
  parse(Rest, N + 1, Width,
        maps:merge(Grid, #{ Atm => Pos, Pos => Atm})).

xy_from_offset(N, Width) ->
  {N rem (Width + 1), N div (Width + 1)}.

%% %% Tests
%% main_test_() ->
%%   Input = get_input(),

%%   [ {"Part 1", ?_assertEqual(0, part1(Input))}
%%   , {"Part 2", ?_assertEqual(0, part2(Input))}
%%   ].





ex1_test() ->
  Binary = <<"#########\n",
             "#b.A.@.a#\n",
             "#########\n">>,

  Grid = parse(Binary),
  %% ?debugFmt("~n~p", [Grid]),
  Width = maps:get(width, Grid),
  Height = maps:get(height, Grid),
  ?assertEqual(9, Width),
  ?assertEqual(3, Height),

  Start = maps:get('@', Grid),
  ?assertEqual({5, 1}, Start),

  NbrFun = fun({X, Y} = P, Grid) ->
               %% ?debugFmt("Nbrs for ~p~n", [P]),
               [{1, {X0, Y0}} ||
                 {X0, Y0} <- [{X - 1, Y},
                              {X + 1, Y},
                              {X, Y + 1},
                              {X, Y - 1}],
                 maps:get({X0, Y0}, Grid) =/= '#'
               ]
           end,

  EndFun = fun(Node, _) -> false end,

  {finished, Result} = dijkstra:dijkstra(Grid, Start, NbrFun, EndFun),

  ?debugFmt("~nResult = ~p", [Result]),
  ?debugFmt("~nShortest paths = ~p",
            [[{Node, dijkstra:shortest_path(Result, maps:get(Node, Grid))} ||
               Node <- [a, b]]]).















%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
