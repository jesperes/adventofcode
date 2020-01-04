%%% Advent of Code solution for 2019 day 06.
%%% Created: 2019-12-06T06:41:22+00:00

-module(day06).
-include_lib("eunit/include/eunit.hrl").

part1(Bin) ->
  Graph = make_digraph(Bin, [acyclic]),
  sum_of_orbits(Graph).

part2(Bin) ->
  Graph = make_digraph(Bin, [cyclic]),
  orbital_dist(Graph).

sum_of_orbits(Graph) ->
  lists:sum(
    lists:map(fun('COM') -> 0;
                 (V) -> length(digraph:get_path(Graph, V, 'COM')) - 1
              end, digraph:vertices(Graph))).

orbital_dist(Graph) ->
  length(digraph:get_path(Graph, 'YOU', 'SAN')) - 3.

make_digraph(Bin, DigraphOpts) ->
  Atom = fun list_to_atom/1,
  Graph = digraph:new(DigraphOpts),
  lists:map(fun(Line) ->
                [A, B] = lists:map(Atom, string:tokens(Line, ")")),
                digraph:add_vertex(Graph, A),
                digraph:add_vertex(Graph, B),
                digraph:add_edge(Graph, B, A, {B, A}),
                digraph:add_edge(Graph, A, B, {A, B})
            end, string:tokens(
                   string:trim(binary_to_list(Bin)), "\n")),
  Graph.

%% Tests
main_test_() ->
  {ok, Input} = file:read_file("../inputs/input06.txt"),

  [ {"Part 1", ?_assertEqual(300598, part1(Input))}
  , {"Part 2", ?_assertEqual(520, part2(Input))}
  ].

ex1_test_() ->
  Bin = <<"COM)B\n",
          "B)C\n",
          "C)D\n",
          "D)E\n",
          "E)F\n",
          "B)G\n",
          "G)H\n",
          "D)I\n",
          "E)J\n",
          "J)K\n",
          "K)L\n">>,
  Graph = make_digraph(Bin, [acyclic]),
  ?_assertEqual(42, sum_of_orbits(Graph)).

ex2_test_() ->
  Bin = <<"COM)B\n",
          "B)C\n",
          "C)D\n",
          "D)E\n",
          "E)F\n",
          "B)G\n",
          "G)H\n",
          "D)I\n",
          "E)J\n",
          "J)K\n",
          "K)L\n",
          "K)YOU\n",
          "I)SAN\n">>,
  Graph = make_digraph(Bin, [cyclic]),
  ?_assertEqual(4, orbital_dist(Graph)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
