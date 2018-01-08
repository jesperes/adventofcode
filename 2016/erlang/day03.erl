-module(day03).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

valid_triangle(X, Y, Z) when (X + Y > Z) and
			     (Z + X > Y) and
			     (Z + Y > X) ->
    true;
valid_triangle(_, _, _) ->
    false.

valid_triangle_test() ->   
    ?assert(valid_triangle(1, 2, 2)),
    ?assertNot(valid_triangle(1, 2, 5)).

line_to_integers(Line) -> 
    lists:map(fun list_to_integer/1, string:tokens(Line, " ")).

line_to_integers_test() ->
    [1, 2, 3] = line_to_integers("1 2 3").

file_to_list_of_ints(File) ->
    lists:map(fun line_to_integers/1, utils:read_file_lines(File)).

valid_triangles(List) ->
    lists:filter(fun([X,Y,Z]) -> valid_triangle(X, Y, Z) end, List).

large_input_test() ->
    List = file_to_list_of_ints("input03.txt"),
    ValidTriangles = valid_triangles(List),
    ?debugFmt("Part1: ~w", [length(ValidTriangles)]).

%% Converts a list of 3-lists [[a,b,c],[...]|...] into 3 lists of
%% integers formed by traversing each column vertically.
vertical_ints_to_3lists([]) -> 
    [[], [], []];
vertical_ints_to_3lists([[A, B, C]|Rest]) ->
    [As, Bs, Cs] = vertical_ints_to_3lists(Rest),
    [[A|As], [B|Bs], [C|Cs]].

split_by_three([]) ->
    [];
split_by_three([X1, X2, X3|Rest]) ->
    [[X1, X2, X3]|split_by_three(Rest)].

convert_vertical_ints(List) ->
    %% Convert 3-lists going vertically into three separate lists
    [A, B, C] = vertical_ints_to_3lists(List),
    
    %% Append them and split them into groups of three.
    split_by_three(A ++ B ++ C).

vertical_ints_to_3lists_test() ->
    List = convert_vertical_ints([[100, 300, 500],
				  [101, 301, 501],
				  [102, 302, 502],
				  [200, 400, 600],
				  [201, 401, 601],
				  [202, 402, 603]]),
    [[100, 101, 102]|_] = List.

large_input_part2_test() ->
    List = file_to_list_of_ints("input03.txt"),
    ConvertedList = convert_vertical_ints(List),
    ValidTriangles = valid_triangles(ConvertedList),
    ?debugFmt("Part2: ~w", [length(ValidTriangles)]).

