%%% Advent of Code solution for 2020 day 12.
%%% Created: 2020-12-12T05:24:44+00:00

-module(aoc2020_day12).
-include_lib("eunit/include/eunit.hrl").

%% ======================================================================
%% Part 1
%% ======================================================================

part1(Lines) ->
  {Xfinal, Yfinal, _} =
    lists:foldl(
      fun(S, {X, Y, Dir}) ->
          case re:run(S, "([NSEWLRF])(\\d+)", [{capture, all_but_first, list}]) of
            {match, [M1, M2]} ->
              Value = list_to_integer(M2),
              case M1 of
                "N" -> {X, Y - Value, Dir};
                "E" -> {X + Value, Y, Dir};
                "S" -> {X, Y + Value, Dir};
                "W" -> {X - Value, Y, Dir};
                "L" -> {X, Y, left(Dir, Value)};
                "R" -> {X, Y, right(Dir, Value)};
                "F" ->
                  {X0, Y0} = forward(X, Y, Value, Dir),
                  {X0, Y0, Dir}
              end
          end
      end, {0, 0, 90}, Lines),
  abs(Xfinal) + abs(Yfinal).

%% Helpers
left(Dir, Value) -> ((Dir + 360) - Value) rem 360.
right(Dir, Value) -> ((Dir + 360) + Value) rem 360.

forward(X, Y, Value, 0) -> {X, Y - Value};
forward(X, Y, Value, 90) -> {X + Value, Y};
forward(X, Y, Value, 180) -> {X, Y + Value};
forward(X, Y, Value, 270) -> {X - Value, Y}.


%% ======================================================================
%% Part 2
%% ======================================================================

part2(Lines) ->
  {_, {Xfinal, Yfinal}} =
    lists:foldl(
      fun(S, {WP, Ship}) ->
          {WpX, WpY} = WP,
          {ShipX, ShipY} = Ship,
          Next =
            case re:run(S, "([NSEWLRF])(\\d+)", [{capture, all_but_first, list}]) of
              {match, [M1, M2]} ->
                V = list_to_integer(M2),
                case M1 of
                  "N" -> {{WpX, WpY - V}, Ship};
                  "E" -> {{WpX + V, WpY}, Ship};
                  "S" -> {{WpX, WpY + V}, Ship};
                  "W" -> {{WpX - V, WpY}, Ship};
                  "L" -> {wp_left(WP, V), Ship};
                  "R" -> {wp_right(WP, V), Ship};
                  "F" -> {WP, {ShipX + WpX * V, ShipY + WpY * V}}
                end
            end,
          Next
      end, {{10, -1}, {0, 0}}, Lines),
  abs(Xfinal) + abs(Yfinal).

%% Rotate waypoint left, N degrees
wp_left({X, Y}, 0) -> {X, Y};
wp_left({X, Y}, N) -> wp_left({Y, -X}, N - 90).

%% Rotate waypoint right, N degrees
wp_right({X, Y}, 0) -> {X, Y};
wp_right({X, Y}, N) -> wp_right({-Y, X}, N - 90).



%% Input reader (place downloaded input file in
%% priv/inputs/2020/input12.txt).
get_input() ->
  inputs:get_as_lines(2020, 12).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(845, part1(Input))}
  , {"Part 2", ?_assertEqual(27016, part2(Input))}
  ].

test_input() ->
  ["F10",
   "N3",
   "F7",
   "R90",
   "F11"].

ex1_test_() ->
  ?_assertEqual(25, part1(test_input())).

ex2_test_() ->
  ?_assertEqual(286, part2(test_input())).


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
