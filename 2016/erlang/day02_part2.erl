-module(day02_part2).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

-define(SMALL_INPUT, 
	["ULL",
	 "RRDDD",
	 "LURDL",
	 "UUUUD"]).

%%     1
%%   2 3 4
%% 5 6 7 8 9
%%   A B C
%%     D

next_digit(1, $D) -> 3;

next_digit(2, $R) -> 3;
next_digit(2, $D) -> 6;

next_digit(3, $U) -> 1;
next_digit(3, $L) -> 2;
next_digit(3, $R) -> 4;
next_digit(3, $D) -> 7;

next_digit(4, $L) -> 3;
next_digit(4, $D) -> 8;

next_digit(5, $R) -> 6;

next_digit(6, $U) -> 2;
next_digit(6, $L) -> 5;
next_digit(6, $R) -> 7;
next_digit(6, $D) -> a;
    
next_digit(7, $U) -> 3;
next_digit(7, $L) -> 6;
next_digit(7, $R) -> 8;
next_digit(7, $D) -> b;

next_digit(8, $U) -> 4;
next_digit(8, $R) -> 9;
next_digit(8, $L) -> 7;
next_digit(8, $D) -> c;

next_digit(9, $L) -> 8;

next_digit(a, $U) -> 6;
next_digit(a, $R) -> b;

next_digit(b, $U) -> 7;
next_digit(b, $R) -> c;
next_digit(b, $D) -> d;
next_digit(b, $L) -> a;

next_digit(c, $U) -> 8;
next_digit(c, $L) -> b;

next_digit(d, $U) -> b;

next_digit(X, _) -> X.

next_digit_test() ->
    5 = next_digit(5, $U),
    6 = next_digit(5, $R),
    7 = next_digit(b, $U).

get_single_code([], Digit) -> 
    Digit;
get_single_code([Letter|Letters], Digit) ->
    NextDigit = next_digit(Digit, Letter),
    get_single_code(Letters, NextDigit).

get_single_code_test() ->
    5 = get_single_code("ULL", 5),
    d = get_single_code("RRDDD", 5),
    b = get_single_code("LURDL", d),
    3 = get_single_code("UUUUD", b).

get_code_from_list([], _) -> 
    [];
get_code_from_list([First|Rest], StartDigit) ->
    NextDigit = get_single_code(First, StartDigit),
    [NextDigit|get_code_from_list(Rest, NextDigit)].

small_input_test() ->
    [5, d, b, 3] = get_code_from_list(["ULL",
				       "RRDDD",
				       "LURDL",
				       "UUUUD"], 5).

large_input_test() ->
    Lines = utils:read_file_lines("input02.txt"),
    Code = get_code_from_list(Lines, 1),
    ?debugFmt("Day02: code = ~w~n", [Code]).


    
