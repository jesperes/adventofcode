-module(day08).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

%%% Prints the display using ?debugFmt.
print_display([]) ->
    true;
print_display([Row|Rows]) ->
    ?debugFmt("~s", [Row]),
    print_display(Rows).

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

decode_instr_test() ->
    [{rect, 2, 3},
     {rotate, row, {y, 3}, 2},
     {rotate, column, {x, 3}, 2}] = 
	lists:map(fun decode_instr/1, 
		  ["rect 2x3",
		   "rotate row y=3 by 2",
		   "rotate column x=3 by 2"]).

rotate(X, N) ->
    {L1, L2} = lists:split(length(X) - N, X),
    L2 ++ L1.

rotate_test() ->    
    "34512" = rotate("12345", 3),
    "7123456" = rotate("1234567", 1).

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

replace_prefix_test() ->
    "####...." = replace_prefix(4, $#, "........").

%%% Return the Nth column as a list
nth_column([], _) ->
    [];
nth_column([Row|Rows], N) ->
    [lists:nth(N + 1, Row)|nth_column(Rows, N)].

nth_column_test() ->
    "258" = nth_column(["123", "456", "789"], 1).

%%% Replace the Nth column with the given list.
replace_nth_column([], [], _) ->
    [];
replace_nth_column([Row|Rows], [X|Xs], N) ->
    [replace_list_member(Row, N, X)|replace_nth_column(Rows, Xs, N)].

replace_nth_column_test() ->
    ["1a3", "4b6", "7c9"] = replace_nth_column(["123", "456", "789"], "abc", 1).

%%% Execute a single instruction. Returns the modified display.
%%% ----------------------------------------------------------------------
execute_single_instr({rect, Width, Height}, Display) ->
    %% Fill WidthxHeight rectangle from top-left corner.
    lists:map(fun(Row) ->
		      replace_prefix(Width, $#, Row)
	      end, lists:sublist(Display, Height))
	++ lists:sublist(Display, Height, length(Display) - Height);
execute_single_instr({rotate, row, {y, Row}, Steps}, Display) ->
    %% Rotate Row by the given number of Steps.
    RowToRotate = lists:nth(Row + 1, Display),
    RotatedRow = rotate(RowToRotate, Steps),
    replace_list_member(Display, Row, RotatedRow);
execute_single_instr({rotate, column, {x, Col}, Steps}, Display) ->
    %% Rotate Col by the given number of Steps.    
    ColumnToRotate = nth_column(Display, Col),
    RotatedColumn = rotate(ColumnToRotate, Steps),
    replace_nth_column(Display, RotatedColumn, Col).

execute_single_instr_test() ->
    Display = execute_single_instr({rect, 3, 3}, generate_display(4, 4)),
    ["###.", 
     "###.",
     "###.",
     "...."] = Display,

    Display0 = execute_single_instr({rotate, column, {x, 1}, 2}, Display),
    ["###.",
     "#.#.",
     "###.",
     ".#.."] = Display0.

%%print_display(Display0).


%%% Execute a list of instructions on the given display. Returns the
%%% modified display.
execute_instr([], Display) ->
    Display;
execute_instr([Instr|Rest], Display) ->
    NewDisplay = execute_single_instr(Instr, Display),
    execute_instr(Rest, NewDisplay).

execute_instr(InstrList, Width, Height) ->
    Display = generate_display(Width, Height),
    execute_instr(InstrList, Display).

count_pixels([]) ->
    0;
count_pixels([Row|Rows]) ->
    length(lists:filter(fun(X) -> X == $# end, Row)) +
	count_pixels(Rows).

small_input_test() ->
    InstrList = lists:map(fun decode_instr/1, 
			  ["rect 3x2",
			   "rotate column x=1 by 1",
			   "rotate row y=0 by 4",
			   "rotate column x=1 by 1"]),
    Display = execute_instr(InstrList, 7, 3),
    [".#..#.#",
     "#.#....",
     ".#....."] = Display,
    6 = count_pixels(Display).

large_input_test() ->
    InstrList = lists:map(fun decode_instr/1, utils:read_file_lines("input08.txt")),
    Display = execute_instr(InstrList, 50, 6),
    6 = length(Display),
    ?debugFmt("Number of pixels: ~w", [count_pixels(Display)]).

