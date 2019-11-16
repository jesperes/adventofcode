-module(aoc2016_day08).
-include_lib("eunit/include/eunit.hrl").

%%% Prints the display using ?debugFmt.
%% print_display([]) ->
%%   ?debugMsg("===================================="),
%%   true;
%% print_display([Row|Rows]) ->
%%   ?debugFmt("~s", [Row]),
%%   print_display(Rows).

decode_instr(Instr) ->
  [Op|Rest] = string:lexemes(Instr, " "),
  case list_to_atom(Op) of
    rect ->
      [Dim] = Rest,
      [Width,Height] = string:lexemes(Dim, "x"),
      {rect, list_to_integer(Width), list_to_integer(Height)};
    rotate ->
      [Direction,Op1,_,Op2] = Rest,
      [Var, Value] = string:lexemes(Op1, "="),
      { rotate,
        list_to_atom(Direction),
        { list_to_atom(Var),
          list_to_integer(Value)},
        list_to_integer(Op2)
      };
    _ ->
      true
  end.

rotate(X, N) ->
  {L1, L2} = lists:split(length(X) - (N rem length(X)), X),
  L2 ++ L1.

%%% Replace a member in a list
replace_list_member([], _, _) ->
  [];
replace_list_member([_|List], 0, Member) ->
  [Member|List];
replace_list_member([X|List], N, Member) ->
  [X|replace_list_member(List, N-1, Member)].

%%% Generate a display of the given dimensions.
generate_display(Width, Height) ->
  [[ $. || _ <- lists:seq(1, Width)] || _ <- lists:seq(1, Height)].

%%% Replace the N first characters in a string.
replace_prefix(0, _, Xs) ->
  Xs;
replace_prefix(_, _, []) ->
  [];
replace_prefix(N, Char, [_X|Xs]) ->
  [Char|replace_prefix(N-1, Char, Xs)].


%%% Return the Nth column as a list
nth_column([], _) ->
  [];
nth_column([Row|Rows], N) ->
  [lists:nth(N + 1, Row)|nth_column(Rows, N)].

%%% Replace the Nth column with the given list.
replace_nth_column([], [], _) ->
  [];
replace_nth_column([Row|Rows], [X|Xs], N) ->
  [replace_list_member(Row, N, X)|replace_nth_column(Rows, Xs, N)].

%%% Execute a single instruction. Returns the modified display.
%%% ----------------------------------------------------------------------
execute_single_instr({rect, Width, Height}, Display) ->
  %% Fill WidthxHeight rectangle from top-left corner.
  NewDisplay = lists:map(fun(Row) ->
                             replace_prefix(Width, $#, Row)
                         end, lists:sublist(Display, Height))
    ++ lists:sublist(Display, Height + 1, length(Display) - Height),
  ?assert(length(NewDisplay) == length(Display)),
  NewDisplay;
execute_single_instr({rotate, row, {y, Row}, Steps}, Display) ->
  %% Rotate Row by the given number of Steps.
  RowToRotate = lists:nth(Row + 1, Display),
  RotatedRow = rotate(RowToRotate, Steps),
  NewDisplay = replace_list_member(Display, Row, RotatedRow),
  ?assert(length(NewDisplay) == length(Display)),
  NewDisplay;
execute_single_instr({rotate, column, {x, Col}, Steps}, Display) ->
  %% Rotate Col by the given number of Steps.
  ColumnToRotate = nth_column(Display, Col),
  RotatedColumn = rotate(ColumnToRotate, Steps),
  NewDisplay = replace_nth_column(Display, RotatedColumn, Col),
  ?assert(length(NewDisplay) == length(Display)),
  NewDisplay.

%%% Execute a list of instructions on the given display. Returns the
%%% modified display.
execute_instr([], Display) ->
  Display;
execute_instr([Instr|Rest], Display) ->
  %%?debugFmt("==[ ~w ]==", [Instr]),
  NewDisplay = execute_single_instr(Instr, Display),
  %%print_display(NewDisplay),
  execute_instr(Rest, NewDisplay).

execute_instr(InstrList, Width, Height) ->
  Display = generate_display(Width, Height),
  %% ?debugMsg("==[ Initial display ]=="),
  %% print_display(Display),
  execute_instr(InstrList, Display).

count_pixels([]) -> 0;
count_pixels([Row|Rows]) ->
  length(lists:filter(fun(X) -> X == $# end, Row)) +
    count_pixels(Rows).

%%% ======================================================================
%%% Tests
%%% ======================================================================

-spec decode_and_execute(Instrs :: list(string()),
                         Size :: {integer(), integer()}) ->
                            {NumPixels :: integer(),
                             Display :: list(string())}.
decode_and_execute(Instrs, {W, H} = _Size) ->
  InstrList = lists:map(fun decode_instr/1, Instrs),
  Display = execute_instr(InstrList, W, H),
  {count_pixels(Display), Display}.

main_test_() ->

  [{"Test input",
    ?_assertEqual({6, [".#..#.#",
                       "#.#....",
                       ".#....."]},
                  decode_and_execute(["rect 3x2",
                                      "rotate column x=1 by 1",
                                      "rotate row y=0 by 4",
                                      "rotate column x=1 by 1"], {7, 3}))}
  , {"Part 1 & 2",
     %% Unfortunately we are not able to compute the solution
     %% ourselves; since this would require OCR functionality, we just
     %% verify that the solution is correct.
     ?_assertEqual({115,
                    ["####.####.####.#...##..#.####.###..####..###...##.",
                     "#....#....#....#...##.#..#....#..#.#......#.....#.",
                     "###..###..###...#.#.##...###..#..#.###....#.....#.",
                     "#....#....#......#..#.#..#....###..#......#.....#.",
                     "#....#....#......#..#.#..#....#.#..#......#..#..#.",
                     "####.#....####...#..#..#.#....#..#.#.....###..##.."]},
                   decode_and_execute(inputs:get_as_lines(2016, 8),
                                      {50, 6}))}
  ].

rotate_test() ->
  "34512" = rotate("12345", 3),
  "7123456" = rotate("1234567", 1),
  "1234567" = rotate("1234567", 7),
  "5671234" = rotate("1234567", 10).

nth_column_test() ->
  "258" = nth_column(["123", "456", "789"], 1).

execute_single_instr_test() ->
  Display = execute_single_instr({rect, 3, 3}, generate_display(4, 4)),
  ?assertEqual(["###.",
                "###.",
                "###.",
                "...."], Display),

  Display0 = execute_single_instr({rotate, column, {x, 1}, 2}, Display),
  ?assertEqual(["###.",
                "#.#.",
                "###.",
                ".#.."], Display0),

  Display1 = execute_single_instr({rotate, row, {y, 1}, 2}, Display),
  ?assertEqual(["###.",
                "#.##",
                "###.",
                "...."], Display1).

replace_nth_column_test() ->
  ["1a3", "4b6", "7c9"] = replace_nth_column(["123", "456", "789"], "abc", 1).

replace_prefix_test() ->
  "####...." = replace_prefix(4, $#, "........").

decode_instr_test() ->
  [{rect, 2, 3},
   {rotate, row, {y, 3}, 2},
   {rotate, column, {x, 3}, 2}] =
    lists:map(fun decode_instr/1,
              ["rect 2x3",
               "rotate row y=3 by 2",
               "rotate column x=3 by 2"]).

%%% Regression tests to reproduce a bug causing the pixel count to be
%%% incorrect.
bug_execute_instr_test() ->
  Display = execute_instr([{rect, 1, 1},
                           {rotate, row, {y, 0}, 5},
                           {rect, 1, 1}],
                          generate_display(50, 6)),
  ["#....#............................................",
   "..................................................",
   "..................................................",
   "..................................................",
   "..................................................",
   ".................................................."] = Display.

bug_execute_instr_2_test() ->
  Display = execute_instr(
              [{rect, 1, 1}],
              ["...#...",
               "......."]),

  ["#..#...",
   "......."] = Display.
