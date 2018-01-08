-module(day02).
%%-export([bathroom_code/0]).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

-define(SMALL_INPUT, 
	["ULL",
	 "RRDDD",
	 "LURDL",
	 "UUUUD"]).

%% 1 2 3
%% 4 5 6
%% 7 8 9

next_digit(1, $R) -> 2;
next_digit(1, $D) -> 4;

next_digit(2, $L) -> 1;
next_digit(2, $D) -> 5;
next_digit(2, $R) -> 3;

next_digit(3, $L) -> 2;
next_digit(3, $D) -> 6;

next_digit(4, $R) -> 5;
next_digit(4, $D) -> 7;
next_digit(4, $U) -> 1;

next_digit(5, $U) -> 2;
next_digit(5, $D) -> 8;
next_digit(5, $R) -> 6;
next_digit(5, $L) -> 4;

next_digit(6, $U) -> 3;
next_digit(6, $L) -> 5;
next_digit(6, $D) -> 9;
    
next_digit(7, $U) -> 4;
next_digit(7, $R) -> 8;

next_digit(8, $U) -> 5;
next_digit(8, $R) -> 9;
next_digit(8, $L) -> 7;

next_digit(9, $U) -> 6;
next_digit(9, $L) -> 8;

next_digit(X, _) -> X.

next_digit_test() ->
    2 = next_digit(5, $U),
    4 = next_digit(5, $L),
    6 = next_digit(5, $R),
    8 = next_digit(5, $D),
    1 = next_digit(1, $L).


get_single_code([], Digit) -> 
    Digit;
get_single_code([Letter|Letters], Digit) ->
    NextDigit = next_digit(Digit, Letter),
    get_single_code(Letters, NextDigit).

get_single_code_test() ->
    1 = get_single_code("ULL", 5),
    9 = get_single_code("RRDDD", 1),
    8 = get_single_code("LURDL", 9),
    5 = get_single_code("UUUUD", 8).

get_code_from_list([], _) -> 
    [];
get_code_from_list([First|Rest], StartDigit) ->
    NextDigit = get_single_code(First, StartDigit),
    [NextDigit|get_code_from_list(Rest, NextDigit)].

small_input_test() ->
    [1, 9, 8, 5] = get_code_from_list(["ULL",
				       "RRDDD",
				       "LURDL",
				       "UUUUD"], 5).
large_input_test() ->
    Lines = utils:read_file_lines("input02.txt"),
    Code = get_code_from_list(Lines, 1),
    ?debugFmt("Day02: code = ~w~n", [Code]).


    
