%%% Advent of Code solution for 2019 day 18.
%%% Created: 2019-12-18T18:36:27+00:00

-module(aoc2019_day18).
-include_lib("eunit/include/eunit.hrl").

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
%% Use dijkstra's algorithm with nodes being a tuple of {Pos,
%% KeysFound}.
part1(Binary, AllKeys) ->
  Grid = parse(Binary),
  StartPos = maps:get($@, Grid),
  StartNode = {StartPos, []},
  Grid0 = maps:put(keys, AllKeys, Grid),
  {found, Node, State} = dijkstra:dijkstra(Grid0, StartNode, fun neighbors/2),
  length(dijkstra:shortest_path(State, Node)) - 1.

-type pos()            :: {X :: integer(), Y :: integer()}.
-type keylist()        :: term().
-type search_node()    :: {pos(), keylist()}.
-type graph()          :: map().

%% Neighbor-function. Returns the list of neighbors to a given node.
%% In this particular implementation, returns a list of positions +
%% list of keys taken along that path.
-spec neighbors(Node :: search_node(),
                Graph :: graph()) -> list(search_node()).
neighbors({{X, Y}, KeysIn} = _Node, Graph) ->
  case KeysIn =:= maps:get(keys, Graph) of
    true -> found;
    false ->
      Dist = 1,
      [{Dist, {{X0, Y0}, collect_keys({X0, Y0}, KeysIn, Graph)}} ||
        {X0, Y0} <- [{X - 1, Y},
                     {X + 1, Y},
                     {X, Y + 1},
                     {X, Y - 1}],
        is_open({X0, Y0}, KeysIn, Graph)]
  end.

collect_keys(Pos, KeysIn, Graph) ->
  C = maps:get(Pos, Graph),
  case is_key(C) of
    true -> lists:usort([C|KeysIn]);
    _    -> KeysIn
  end.

%% Return true if we can pass through the given node.
is_open(Pos, Keys, Graph) ->
  D = maps:get(Pos, Graph, $#),
  case is_wall(D) of
    true -> false;
    false ->
      case is_door(D) of
        false -> true;
        true ->
          K = matching_key(D),
          lists:member(K, Keys)
      end
  end.

is_wall(C) -> C =:= $#.
is_key(C) -> (C >= $a) and (C =< $z).
is_door(C) -> (C >= $A) and (C =< $Z).
matching_key(D) -> D + 32.

get_input() ->
  inputs:get_as_binary(2019, 18).

%% ============================================================
%% Parsing text input into a map
%% ============================================================

parse(Binary) ->
  [{Width, _}|_] = binary:matches(Binary, <<"\n">>),
  ?assertEqual($\n, binary:at(Binary, Width)),
  String = binary_to_list(Binary),
  parse(String, 0, Width,
        #{ width => Width %% without newline
         , height => byte_size(Binary) div (Width + 1)
         }).

parse([], _, _Width, Grid) ->
  Grid;
parse([$\n|Rest], N, Width, Grid) ->
  parse(Rest, N + 1, Width, Grid);
parse([C|Rest], N, Width, Grid) ->
  Pos = xy_from_offset(N, Width),
  parse(Rest, N + 1, Width,
        maps:merge(Grid, #{ C => Pos, Pos => C})).

xy_from_offset(N, Width) ->
  {N rem (Width + 1), N div (Width + 1)}.

%% ============================================================
%% Tests
%% ============================================================

main_test_() ->
  Input = get_input(),
  AllKeys = lists:seq($a, $z),

  [ {"Part 1", timeout, 60, ?_assertEqual(3856, part1(Input, AllKeys))}
    %% , {"Part 2", ?_assertEqual(0, part2(Input))}
  ].

ex1_test_() ->
  Binary = <<"########################\n",
             "#f.D.E.e.C.b.A.@.a.B.c.#\n",
             "######################.#\n",
             "#d.....................#\n",
             "########################\n">>,
  ?_assertEqual(86, part1(Binary, "abcdef")).

ex2_test_() ->
  Binary = <<"#################\n",
             "#i.G..c...e..H.p#\n",
             "########.########\n",
             "#j.A..b...f..D.o#\n",
             "########@########\n",
             "#k.E..a...g..B.n#\n",
             "########.########\n",
             "#l.F..d...h..C.m#\n",
             "#################\n"
           >>,

  Keys = lists:seq($a, $p),
  ?_assertEqual(136, part1(Binary, Keys)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
