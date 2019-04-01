-module(puzzle02).

-export([start/0]).

start() ->
    {ok, Bin} = file:read_file("input.txt"),
    Lines = string:tokens(binary_to_list(Bin), "\n"),
    {lists:map(fun do_line1/1, Lines),
     lists:map(fun do_line2/1, Lines)}.

do_line1(Line) -> do_line(Line, fun move/2, 5).
do_line2(Line) -> do_line(Line, fun move2/2, 5).

do_line([], _, Pos) when Pos =< $9 -> Pos + $0;
do_line([], _, Pos) -> Pos;
do_line([C|Rest], Fun, Pos) -> do_line(Rest, Fun, Fun(Pos, C)).

%% 1 2 3
%% 4 5 6
%% 7 8 9
move(1, $L) -> 1;
move(1, $U) -> 1;
move(1, $D) -> 4;
move(1, $R) -> 2;
move(2, $L) -> 1;
move(2, $U) -> 2;
move(2, $D) -> 5;
move(2, $R) -> 3;
move(3, $L) -> 2;
move(3, $U) -> 3;
move(3, $D) -> 6;
move(3, $R) -> 3;
move(4, $L) -> 4;
move(4, $U) -> 1;
move(4, $D) -> 7;
move(4, $R) -> 5;
move(5, $L) -> 4;
move(5, $U) -> 2;
move(5, $D) -> 8;
move(5, $R) -> 6;
move(6, $L) -> 5;
move(6, $U) -> 3;
move(6, $D) -> 9;
move(6, $R) -> 6;
move(7, $L) -> 7;
move(7, $U) -> 4;
move(7, $D) -> 7;
move(7, $R) -> 8;
move(8, $L) -> 7;
move(8, $U) -> 5;
move(8, $D) -> 8;
move(8, $R) -> 9;
move(9, $L) -> 8;
move(9, $U) -> 6;
move(9, $D) -> 9;
move(9, $R) -> 9.

%%     1
%%   2 3 4
%% 5 6 7 8 9
%%   A B C
%%     D
move2(1, $R) -> 1;
move2(1, $L) -> 1;
move2(1, $U) -> 1;
move2(1, $D) -> 3;
move2(2, $R) -> 3;
move2(2, $L) -> 2;
move2(2, $U) -> 2;
move2(2, $D) -> 6;
move2(3, $R) -> 4;
move2(3, $L) -> 2;
move2(3, $U) -> 1;
move2(3, $D) -> 7;
move2(4, $R) -> 4;
move2(4, $L) -> 3;
move2(4, $U) -> 4;
move2(4, $D) -> 8;
move2(5, $R) -> 6;
move2(5, $L) -> 5;
move2(5, $U) -> 5;
move2(5, $D) -> 5;
move2(6, $R) -> 7;
move2(6, $L) -> 5;
move2(6, $U) -> 2;
move2(6, $D) -> $A;
move2(7, $R) -> 8;
move2(7, $L) -> 6;
move2(7, $U) -> 3;
move2(7, $D) -> $B;
move2(8, $R) -> 9;
move2(8, $L) -> 7;
move2(8, $U) -> 4;
move2(8, $D) -> $C;
move2(9, $R) -> 9;
move2(9, $L) -> 8;
move2(9, $U) -> 9;
move2(9, $D) -> 9;
move2($A, $R) -> $B;
move2($A, $L) -> $A;
move2($A, $U) -> 6;
move2($A, $D) -> $A;
move2($B, $R) -> $C;
move2($B, $L) -> $A;
move2($B, $U) -> 7;
move2($B, $D) -> $D;
move2($C, $R) -> $C;
move2($C, $L) -> $B;
move2($C, $U) -> 8;
move2($C, $D) -> $C;
move2($D, $R) -> $D;
move2($D, $L) -> $D;
move2($D, $U) -> $B;
move2($D, $D) -> $D.
