%%% Advent of Code solution for 2020 day 02.
%%% Created: 2020-12-02T05:50:52+00:00

-module(aoc2020_day02).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Input) ->
  length(lists:filter(fun is_valid_password_1/1, parse_pwd_list(Input))).

part2(Input) ->
  length(lists:filter(fun is_valid_password_2/1, parse_pwd_list(Input))).

parse_pwd_list(Input) ->
  lists:map(
    fun(S) ->
        [A, B, [Letter], Pwd] = string:tokens(S, ":- "),
        {list_to_integer(A),
         list_to_integer(B),
         Letter,
         Pwd}
    end, Input).

is_valid_password_1({A, B, Letter, Pwd}) ->
  N = number_of(Letter, Pwd),
  N >= A andalso N =< B.

number_of(_, "") -> 0;
number_of(C, [C|Rest]) ->
  1 + number_of(C, Rest);
number_of(C, [_|Rest]) ->
  number_of(C, Rest).

is_valid_password_2({A, B, Letter, Pwd}) ->
  case {lists:nth(A, Pwd) =:= Letter, lists:nth(B, Pwd) =:= Letter} of
    {true, false} -> true;
    {false, true} -> true;
    _ -> false
  end.

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input02.txt).
get_input() ->
  inputs:get_as_lines(2020, 02).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(660, part1(Input))}
  , {"Part 2", ?_assertEqual(530, part2(Input))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
