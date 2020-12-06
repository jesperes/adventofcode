%%% Advent of Code solution for 2020 day 06.
%%% Created: 2020-12-06T06:25:52+00:00

-module(aoc2020_day06).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Input) ->
  lists:sum(
    lists:map(fun num_yes_answers/1,
              string:split(Input, "\n\n", all))).

%% Returns the number of questions to which *anyone* answered yes.
%% This is simply the number of distinct letters in the input strings.
num_yes_answers(Group) ->
  length(lists:usort(
           lists:flatten(
             string:split(Group, "\n", all)))).

part2(Input) ->
  lists:sum(
    lists:map(fun all_yes_answers/1,
              string:split(Input, "\n\n", all))).

%% Returns the number of questions to which *everyone* answered
%% yes. This is the intersection of letters; i.e. the number of
%% letters which occurs in all answer strings.
all_yes_answers(Groups) ->
  [First|Rest] = string:split(Groups, "\n", all),
  sets:size(
    lists:foldl(fun(Group, Acc) ->
                    sets:intersection(Acc, sets:from_list(Group))
                end, sets:from_list(First), Rest)).

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input06.txt).
get_input() ->
  inputs:get_as_string(2020, 06).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(6680, part1(Input))}
  , {"Part 2", ?_assertEqual(3117, part2(Input))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
