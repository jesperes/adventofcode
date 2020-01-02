%%% Advent of Code solution for 2019 day 20.
%%% Created: 2019-12-31T15:41:26+00:00

-module(aoc2019_day20).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Binary) ->
  Maze = {_, Portals} = parse(Binary),
  StartPos = maps:get('AA', Portals),
  {found, Node, State} = dijkstra:dijkstra(Maze, StartPos, fun evalfun/2),
  Path = dijkstra:shortest_path(State, Node),

  %% {Map, _} = Maze,
  %% ?debugFmt("~n~s~n",
  %%           [grid:to_str(
  %%              grid:grid_with_path(Map, Path, $@))]),
  length(Path) - 1.

evalfun({_, _} = Pos, {Map, Portals}) ->
  case maps:get('ZZ', Portals) of
    End when End =:= Pos -> found;
    _ ->
      lists:filtermap(
        fun(AdjPos) ->
            case maps:get(AdjPos, Map, undef) of
              $. -> {true, {1, AdjPos}};
              $Z -> {true, {1, AdjPos}};
              _ -> false
            end
        end, adj(Pos)) ++
        %% Add the portal as a neighbor, if there is one.
        case maps:get(Pos, Portals, undef) of
          undef -> [];
          Portal -> [{1, Portal}]
        end
  end.

adj({X, Y}) ->
  [{X - 1, Y},
   {X + 1, Y},
   {X, Y - 1},
   {X, Y + 1}].

%% part2(_Input) ->
%%   0.

%% Input reader (place downloaded input file in
%% priv/inputs/2019/input20.txt).
get_input() ->
  inputs:get_as_binary(2019, 20).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(442, part1(Input))}
    %%   , {"Part 2", ?_assertEqual(0, part2(Input))}
  ].

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
  Portals =
    lists:foldl(
      fun(C, Acc) ->
          Portals = find_portals(C, Map),
          lists:foldl(
            fun({Name, Pos}, InnerAcc) ->
                case maps:get(Name, InnerAcc, undef) of
                  undef ->
                    maps:merge(InnerAcc,
                               #{Name => Pos,
                                 Pos => Pos});
                  OtherPos ->
                    maps:merge(InnerAcc,
                               #{Pos => OtherPos,
                                 OtherPos => Pos})
                end
            end, Acc, Portals)
      end, #{}, lists:seq($A, $Z)),

  %% Filter out one-sided "portals" (i.e. 'AA' and 'ZZ').
  maps:filter(fun(K, V) when K =:= V -> false;
                 (_, _) -> true
              end,  Portals).


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

ex1_test() ->
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

  ?assertEqual(23, part1(Binary)).


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
