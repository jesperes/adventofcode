%%% Advent of Code solution for 2019 day 16.
%%% Created: 2019-12-16T05:30:06+00:00

-module(aoc2019_day16).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Input) ->
  first8(fft_repeat_str(Input, 100)).

fft_repeat_str(String, N) ->
  Digits = to_digits(String),
  to_string(fft_repeat(Digits, N)).

fft_repeat(Digits, N) ->
  lists:foldl(fun(_, Acc) ->
                  fft(Acc)
              end, Digits, lists:seq(1, N)).

fft(Digits) ->
  ok.
  %% lists:map(
  %%   fun(Pos) ->
  %%       L = pattern(Pos, length(Digits)),
  %%       Mult = fun({X, Y}) -> X * Y end,
  %%       abs(lists:sum(lists:map(Mult, lists:zip(L, Digits)))) rem 10
  %%   end, lists:seq(1, length(Digits))).

first8(L) ->
  {F, _} = lists:split(8, L),
  F.

pattern(Pos, SegLen) ->
  case ((Pos + 1) div SegLen) rem 4 of
    0 -> 0;
    1 -> 1;
    2 -> 0;
    3 -> -1
  end.



%% Return a list of N, repeated M times.
repeat(N, M) ->
  [N || _ <- lists:seq(1, M)].

to_digits(S) ->
  lists:map(fun(C) -> C - $0 end, S).

to_string(L) ->
  lists:map(fun(C) -> $0 + C end, L).


part2(_Input) ->
  ?debugMsg("Not implemented."),
  0.

%% Input reader (place downloaded input file in
%% priv/inputs/2019/input16.txt).
get_input() ->
  inputs:get_as_string(2019, 16).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", timeout, 60, ?_assertEqual("84970726", part1(Input))}
  %% , {"Part 2", ?_assertEqual(0, part2(Input))}
  ].

pattern_test_() ->
  ?_assertEqual([0, 0, 1, 1, 1, 0, 0, 0, -1, -1, -1],
                pattern(3, 11)).

ex1_test_() ->
  [ ?_assertEqual("01029498", fft_repeat_str("12345678", 4))
  , ?_assertEqual("24176176",
                  first8(fft_repeat_str("80871224585914546619083218645595", 100)))
  , ?_assertEqual("73745418",
                  first8(fft_repeat_str("19617804207202209144916044189917", 100)))
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
