%%% Advent of Code solution for 2019 day 20.
%%% Created: 2019-12-31T15:41:26+00:00

-module(aoc2019_day20).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
%% part1(_Input) ->
%%   0.

%% part2(_Input) ->
%%   0.

%% Input reader (place downloaded input file in
%% priv/inputs/2019/input20.txt).
%% get_input() ->
%%   inputs:get_as_binary(2019, 20).

%% Tests
%% main_test_() ->
%%   Input = get_input(),

%%   [ {"Part 1", ?_assertEqual(0, part1(Input))}
%%   , {"Part 2", ?_assertEqual(0, part2(Input))}
%%   ].

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

%% Returns a map {Portal, Positions}, where Positions is a list of the
%% positions of the exits/entrances of the portal. This will be either
%% one (for 'AA' and 'ZZ') or two (all others).
find_portals(Map) ->
  lists:foldl(
    fun(C, Acc) ->
        Portals = find_portals(C, Map),
        lists:foldl(
          fun({Name, Pos}, InnerAcc) ->
              maps:update_with(Name,
                               fun(Old) -> [Pos|Old] end,
                               [Pos], InnerAcc)
          end, Acc, Portals)
    end, #{}, lists:seq($A, $Z)).

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
          {true, {ToA([Letter, Letter2]), {X, Y + 1}}};
         Upwards ->
          Letter2 = maps:get({X, Y - 1}, Map),
          {true, {ToA([Letter2, Letter]), {X, Y - 1}}};
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
          {true, {ToA([Letter, Letter2]), {X + 1, Y}}};
         Left ->
          Letter2 = maps:get({X - 1, Y}, Map),
          {true, {ToA([Letter2, Letter]), {X - 1, Y}}};
         true ->
          false
      end;
     true ->
      false
  end.

ex1_test() ->
  Binary = <<"         A           \n",
             "         A           \n",
             "  #######.#########  \n",
             "  #######.........#  \n",
             "  #######.#######.#  \n",
             "  #######.#######.#  \n",
             "  #######.#######.#  \n",
             "  #####  B    ###.#  \n",
             "BC...##  C    ###.#  \n",
             "  ##.##       ###.#  \n",
             "  ##...DE  F  ###.#  \n",
             "  #####    G  ###.#  \n",
             "  #########.#####.#  \n",
             "DE..#######...###.#  \n",
             "  #.#########.###.#  \n",
             "FG..#########.....#  \n",
             "  ###########.#####  \n",
             "             Z       \n",
             "             Z       \n">>,

  Res = parse(Binary),
  ?assertEqual(0, Res).


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
