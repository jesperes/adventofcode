%%% Advent of Code solution for 2019 day 20.
%%% Created: 2019-12-31T15:41:26+00:00

-module(aoc2019_day20).
-include_lib("eunit/include/eunit.hrl").

%% Part 1

part1(Binary) ->
  Maze = {_, Portals} = parse(Binary),
  StartPos = maps:get('AA', Portals),
  {found, Node, State} = dijkstra:dijkstra(Maze, StartPos, fun neighbors/2),
  Path = dijkstra:shortest_path(State, Node),
  length(Path) - 1.

neighbors({_, _} = Pos, {Map, Portals}) ->
  case maps:get('ZZ', Portals) of
    End when End =:= Pos -> found;
    _ ->
      lists:filtermap(
        fun(AdjPos) ->
            case maps:get(AdjPos, Map, undef) of
              $. -> {true, {1, AdjPos}};
              _ -> false
            end
        end, adj(Pos)) ++
        %% Add the portal as a neighbor, if there is one.
        case maps:get(Pos, Portals, undef) of
          undef -> [];
          Portal -> [{1, Portal}]
        end
  end.

%% Part 2

part2(Binary) ->
  Maze = {_, Portals} = parse(Binary),
  StartPos = {0, maps:get('AA', Portals)},
  {found, Node, State} = dijkstra:dijkstra(Maze, StartPos, fun neighbors2/2),
  Path = dijkstra:shortest_path(State, Node),
  length(Path) - 1.

%% For part 2 we need to track which level we are on, and disallow
%% moving outside the top-most level.
neighbors2({Level, {_, _} = Pos}, {Map, Portals}) ->
  case maps:get('ZZ', Portals) of
    End when (End =:= Pos) and (Level == 0) -> found;
    _ ->
      lists:filtermap(
        fun(AdjPos) ->
            case maps:get(AdjPos, Map, undef) of
              $. -> {true, {1, {Level, AdjPos}}};
              _ -> false
            end
        end, adj(Pos)) ++
        %% Add the portal as a neighbor, if there is one.
        case maps:get(Pos, Portals, undef) of
          undef -> [];
          Portal ->
            %% Track movement to adjacent levels.
            NextLevel =
              case maps:get({type, Pos}, Portals) of
                outer -> Level - 1;
                inner -> Level + 1
              end,
            %% Don't allow teleporting outside the outermost level.
            if NextLevel >= 0 ->
                [{1, {NextLevel, Portal}}];
               true ->
                []
            end
        end
  end.

adj({X, Y}) ->
  [{X - 1, Y},
   {X + 1, Y},
   {X, Y - 1},
   {X, Y + 1}].

%% Input reader (place downloaded input file in
%% priv/inputs/2019/input20.txt).
get_input() ->
  inputs:get_as_binary(2019, 20).

%% ==================================================================
%% Code for parsing the maze. This is a bit more work than usual,
%% because we need to locate all the portals.
%% ==================================================================

%%
%% Portals can either be
%%
%% Right: AB.
%%
%% Left: .AB
%%
%% Up: .
%%     A
%%     B
%%
%% Down: A
%%       B
%%       .
%%
%% Portals always read in natural order (Up->Down or Left->Right)
%%

parse(Binary) ->
  Lines = string:tokens(binary_to_list(Binary), "\n"),
  Map =
    lists:foldl(
      fun({Y, Line}, Acc) ->
          parse_line(Y, Line, Acc)
      end, #{}, lists:zip(lists:seq(0, length(Lines) - 1),
                          Lines)),
  {Map, find_portals(Map)}.

parse_line(Y, Line, Acc) ->
  lists:foldl(
    fun({X, D}, InnerAcc) when (D =:= $#) or (D =:= $.) ->
        maps:put({X, Y}, D, InnerAcc);
       ({X, D}, InnerAcc) when (D >= $A) and (D =< $Z) ->
        Pos = {X, Y},
        maps:put(Pos, D,
                 maps:update_with(
                   D, fun(Old) -> [Pos|Old] end, [Pos],
                   InnerAcc));
       (_, InnerAcc) ->
        InnerAcc
    end, Acc, lists:zip(lists:seq(0, length(Line) - 1),
                        Line)).

find_portals(Map) ->
  Size = get_maze_size(Map),

  Portals =
    lists:foldl(
      fun(C, Acc) ->
          Portals = find_portals(C, Map),
          lists:foldl(
            fun({Name, Pos}, InnerAcc) ->
                case maps:get(Name, InnerAcc, undef) of
                  undef ->
                    maps:put(Name, Pos, InnerAcc);
                  OtherPos ->
                    %% For part 2 we need to be able to tell if a
                    %% portal is on the inner or outer side of the
                    %% donut so that we can track the level.
                    maps:merge(InnerAcc,
                               #{ Pos => OtherPos
                                , OtherPos => Pos
                                , {type, Pos} =>
                                    portal_type(Pos, Size)
                                , {type, OtherPos} =>
                                    portal_type(OtherPos, Size)
                                })
                end
            end, Acc, Portals)
      end, #{}, lists:seq($A, $Z)),

  %% Filter out one-sided "portals" (i.e. 'AA' and 'ZZ').
  maps:filter(fun(K, V) when K =:= V -> false;
                 (_, _) -> true
              end,  Portals).

%% Figure out which direction a portal goes. This is necessary so that
%% we know if we are going inwards or outwards.
portal_type({2, _}, _) -> outer;
portal_type({_, 2}, _) -> outer;
portal_type({X, _}, {W, _}) when X == (W - 3) -> outer;
portal_type({_, Y}, {_, H}) when Y == (H - 3) -> outer;
portal_type(_, _) -> inner.

%% Return the {Width,Height} of the maze.
get_maze_size(Map) ->
  {XCoords, YCoords} =
    lists:unzip(
      maps:keys(
        maps:filter(
          fun({X, Y}, _) -> is_integer(X) and is_integer(Y);
             (_, _) -> false
          end, Map))),
  MaxX = lists:max(XCoords),
  MaxY = lists:max(YCoords),
  {MaxX + 1, MaxY + 1}.

%% Find all the portals beginning with Letter.
find_portals(Letter, Map) ->
  case maps:get(Letter, Map, undefined) of
    undefined -> [];
    LetterPos ->
      lists:filtermap(
        fun(Pos) ->
            get_portal(Letter, Pos, Map)
        end, LetterPos)
  end.

%% If Letter is the first letter of a portal, returns {true, {Name,
%% Pos}} where Name is the name of the portal (e.g. AA), and Pos is
%% the portal location (i.e. the tile immediately outside the '.'.
get_portal(Letter, {X, Y}, Map) ->
  ToA = fun list_to_atom/1,

  %% Check if the key is vertical or horizontal
  IsVert =
    (not maps:is_key({X + 1, Y}, Map))
    and (not maps:is_key({X - 1, Y}, Map)),

  IsHoriz =
    (not maps:is_key({X, Y - 1}, Map))
    and (not maps:is_key({X, Y + 1}, Map)),

  if IsVert ->
      Downwards = (maps:get({X, Y + 2}, Map, undef) =:= $.)
        and (not maps:is_key({X, Y - 1}, Map)),
      Upwards = (maps:get({X, Y - 2}, Map, undef) =:= $.)
        and (not maps:is_key({X, Y + 1}, Map)),
      if Downwards ->
          Letter2 = maps:get({X, Y + 1}, Map),
          {true, {ToA([Letter, Letter2]), {X, Y + 2}}};
         Upwards ->
          Letter2 = maps:get({X, Y - 1}, Map),
          {true, {ToA([Letter2, Letter]), {X, Y - 2}}};
         true ->
          false
      end;
     IsHoriz ->
      Right = (maps:get({X + 2, Y}, Map, undef) =:= $.)
        and (not maps:is_key({X - 1, Y}, Map)),
      Left = (maps:get({X - 2, Y}, Map, undef) =:= $.)
        and (not maps:is_key({X + 1, Y}, Map)),
      if Right ->
          Letter2 = maps:get({X + 1, Y}, Map),
          {true, {ToA([Letter, Letter2]), {X + 2, Y}}};
         Left ->
          Letter2 = maps:get({X - 1, Y}, Map),
          {true, {ToA([Letter2, Letter]), {X - 2, Y}}};
         true ->
          false
      end;
     true ->
      false
  end.

%% Tests
main_test_() ->
  Input = get_input(),
  [ {"Part 1", ?_assertEqual(442, part1(Input))}
  , {"Part 2", ?_assertEqual(5208, part2(Input))}
  ].

ex1_test_() ->
  %%                    1
  %%          01234567890123456789
  Binary = <<"         A           \n", % 0
             "         A           \n", % 1
             "  #######.#########  \n", % 2
             "  #######.........#  \n", % 3
             "  #######.#######.#  \n", % 4
             "  #######.#######.#  \n", % 5
             "  #######.#######.#  \n", % 6
             "  #####  B    ###.#  \n", % 7
             "BC...##  C    ###.#  \n", % 8
             "  ##.##       ###.#  \n", % 9
             "  ##...DE  F  ###.#  \n", % 10
             "  #####    G  ###.#  \n", % 11
             "  #########.#####.#  \n", % 12
             "DE..#######...###.#  \n", % 13
             "  #.#########.###.#  \n", % 14
             "FG..#########.....#  \n", % 15
             "  ###########.#####  \n", % 16
             "             Z       \n", % 17
             "             Z       \n">>,

  ?_assertEqual(23, part1(Binary)).

ex2_test_() ->
  Binary =
    <<"                   A               \n",
      "                   A               \n",
      "  #################.#############  \n",
      "  #.#...#...................#.#.#  \n",
      "  #.#.#.###.###.###.#########.#.#  \n",
      "  #.#.#.......#...#.....#.#.#...#  \n",
      "  #.#########.###.#####.#.#.###.#  \n",
      "  #.............#.#.....#.......#  \n",
      "  ###.###########.###.#####.#.#.#  \n",
      "  #.....#        A   C    #.#.#.#  \n",
      "  #######        S   P    #####.#  \n",
      "  #.#...#                 #......VT\n",
      "  #.#.#.#                 #.#####  \n",
      "  #...#.#               YN....#.#  \n",
      "  #.###.#                 #####.#  \n",
      "DI....#.#                 #.....#  \n",
      "  #####.#                 #.###.#  \n",
      "ZZ......#               QG....#..AS\n",
      "  ###.###                 #######  \n",
      "JO..#.#.#                 #.....#  \n",
      "  #.#.#.#                 ###.#.#  \n",
      "  #...#..DI             BU....#..LF\n",
      "  #####.#                 #.#####  \n",
      "YN......#               VT..#....QG\n",
      "  #.###.#                 #.###.#  \n",
      "  #.#...#                 #.....#  \n",
      "  ###.###    J L     J    #.#.###  \n",
      "  #.....#    O F     P    #.#...#  \n",
      "  #.###.#####.#.#####.#####.###.#  \n",
      "  #...#.#.#...#.....#.....#.#...#  \n",
      "  #.#####.###.###.#.#.#########.#  \n",
      "  #...#.#.....#...#.#.#.#.....#.#  \n",
      "  #.###.#####.###.###.#.#.#######  \n",
      "  #.#.........#...#.............#  \n",
      "  #########.###.###.#############  \n",
      "           B   J   C               \n",
      "           U   P   P               \n">>,

  ?_assertEqual(58, part1(Binary)).

ex3_test_() ->
  Binary =
    <<"             Z L X W       C               \n",
      "             Z P Q B       K               \n",
      "  ###########.#.#.#.#######.###############\n",
      "  #...#.......#.#.......#.#.......#.#.#...#\n",
      "  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###\n",
      "  #.#...#.#.#...#.#.#...#...#...#.#.......#\n",
      "  #.###.#######.###.###.#.###.###.#.#######\n",
      "  #...#.......#.#...#...#.............#...#\n",
      "  #.#########.#######.#.#######.#######.###\n",
      "  #...#.#    F       R I       Z    #.#.#.#\n",
      "  #.###.#    D       E C       H    #.#.#.#\n",
      "  #.#...#                           #...#.#\n",
      "  #.###.#                           #.###.#\n",
      "  #.#....OA                       WB..#.#..ZH\n",
      "  #.###.#                           #.#.#.#\n",
      "CJ......#                           #.....#\n",
      "  #######                           #######\n",
      "  #.#....CK                         #......IC\n",
      "  #.###.#                           #.###.#\n",
      "  #.....#                           #...#.#\n",
      "  ###.###                           #.#.#.#\n",
      "XF....#.#                         RF..#.#.#\n",
      "  #####.#                           #######\n",
      "  #......CJ                       NM..#...#\n",
      "  ###.#.#                           #.###.#\n",
      "RE....#.#                           #......RF\n",
      "  ###.###        X   X       L      #.#.#.#\n",
      "  #.....#        F   Q       P      #.#.#.#\n",
      "  ###.###########.###.#######.#########.###\n",
      "  #.....#...#.....#.......#...#.....#.#...#\n",
      "  #####.#.###.#######.#######.###.###.#.#.#\n",
      "  #.......#.......#.#.#.#.#...#...#...#.#.#\n",
      "  #####.###.#####.#.#.#.#.###.###.#.###.###\n",
      "  #.......#.....#.#...#...............#...#\n",
      "  #############.#.#.###.###################\n",
      "               A O F   N                   \n",
      "               A A D   M                   \n">>,

  ?_assertEqual(396, part2(Binary)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
