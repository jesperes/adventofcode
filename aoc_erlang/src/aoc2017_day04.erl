%%% Advent of Code solution for 2017 day 04.
%%% Created: 2019-12-09T08:44:23+00:00

-module(aoc2017_day04).
-include_lib("eunit/include/eunit.hrl").

%% Part 1: count number of passphrases with duplicate words

has_no_dup(Words) ->
  lists:sort(Words) =:= lists:usort(Words).

has_no_anagram(Words) ->
  [] =:= [W1 || W1 <- Words, W2 <- Words,
                W1 =/= W2, lists:sort(W1) =:= lists:sort(W2)].

count(Fun, List) ->
  length(lists:filter(Fun, List)).

part1(Lines) ->
  count(fun has_no_dup/1, Lines).

part2(Lines) ->
  count(fun(Words) ->
            has_no_dup(Words) andalso has_no_anagram(Words)
        end, Lines).


%% Input reader (place downloaded input file in
%% priv/inputs/2017/input04.txt).
get_input() ->
  lists:map(fun(Line) ->
                string:tokens(Line, " ")
            end, inputs:get_as_lines(2017, 04)).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(466, part1(Input))}
  , {"Part 2", ?_assertEqual(251, part2(Input))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
