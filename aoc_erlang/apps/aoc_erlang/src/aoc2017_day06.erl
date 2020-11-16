%%% Advent of Code solution for 2017 day 06.
%%% Created: 2020-07-30T10:52:19+00:00

-module(aoc2017_day06).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
%% part1(Input) ->
%%   Banks = Input,
%%   Len = length(Banks),
%%   {Size, Num} = find_fullest_bank(Banks),
%%   Banks0 = lists:keyreplace(Num, 2, Banks, {0, Num}),
%%   redistribute(Banks0, (Num + 1) rem Len, Size, Len).

%% part2(_Input) ->
%%   ?debugMsg("Not implemented."),
%%   0.

%% redistribute(Banks, _, 0, _) ->
%%   Banks;
%% redistribute(Banks, Bank, Memory, Len) ->
%%   {value, {Size, _}, Banks0} = lists:keytake(Bank, 2, Banks),
%%   Banks1 = lists:keystore(Bank, 2, Banks0, {Size + 1, Bank}),
%%   redistribute(Banks1, (Bank + 1) rem Len, Memory - 1, Len).

%% find_fullest_bank(Banks) ->
%%   [{S, N}|_] = lists:reverse(lists:sort(Banks)),
%%   {S, N}.

%% get_input() ->
%%   Banks = [2, 8, 8, 5, 4, 2, 3, 1, 5, 5, 1, 2, 15, 13, 5, 14],
%%   Indexes = lists:seq(1, length(Banks)),
%%   lists:zip(Banks, Indexes).

%% Tests
main_test_() ->
  [].
  %% Input = get_input(),


  %% [ {"Part 1", ?_assertEqual(0, part1(Input))}
  %% , {"Part 2", ?_assertEqual(0, part2(Input))}
  %% ].

%% redistribute_test() ->
%%   Banks = [ {0, 1}
%%           , {2, 2}
%%           , {7, 3}
%%           , {0, 4}
%%           ],

%%   ?assertEqual(ok, redistribute(Banks, 3, 7, 4)).


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
