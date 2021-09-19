%%% Advent of Code solution for 2019 day 16.
%%% Created: 2019-12-16T05:30:06+00:00

-module(aoc2019_day16).
-include_lib("eunit/include/eunit.hrl").

fft(String, N) ->
  Digits = to_digits(String),
  Fft = lists:foldl(fun(_, Acc) ->
                        fft(Acc)
                    end, Digits, lists:seq(1, N)),
  {F, _} = lists:split(8, to_string(Fft)),
  F.

fft(Digits) ->
  Len = length(Digits),
  io:format("~n---~n", []),
  lists:map(
    fun(SegLen) ->
        List = lists:zip(Digits, lists:seq(0, length(Digits) - 1)),
        abs(lists:foldl(fun({Digit, Pos}, Acc) ->
                            Digit * pattern(Pos, SegLen) + Acc
                        end, 0, List)) rem 10
    end, lists:seq(1, Len)).

pattern(Pos, SegLen) ->
  case ((Pos + 1) div SegLen) rem 4 of
    0 -> 0;
    1 -> 1;
    2 -> 0;
    3 -> -1
  end.

%% Return a list of N, repeated M times.
repeat(N, M) ->
  lists:flatten([N || _ <- lists:seq(1, M)]).

to_digits(S) -> lists:map(fun(C) -> C - $0 end, S).
to_string(L) -> lists:map(fun(C) -> $0 + C end, L).

%% For part 2, we only care about digits at an offset which is in the
%% second half of the string, which allows us to optimize things.
fft2(String, N) ->
  {S, _} = lists:split(7, String),
  Offset = list_to_integer(S),
  {_, Digits} = lists:split(Offset, repeat(to_digits(String), 10000)),
  FFT =
    lists:foldl(fun(_, Acc) ->
                    rfft2(Acc)
                end, Digits, lists:seq(1, N)),
  {F, _} = lists:split(8, FFT),
  to_string(F).

%% This is basically a mapfoldr, but slightly faster.
rfft2(Digits) ->
  {_, Digits0} = do_rfft2(Digits),
  Digits0.
do_rfft2([]) -> {0, []};
do_rfft2([D|Digits]) ->
  {D0, Digits0} = do_rfft2(Digits),
  D1 = (D0 + D) rem 10,
  {D1, [D1|Digits0]}.

%% Input reader (place downloaded input file in
%% priv/inputs/2019/input16.txt).
get_input() ->
  inputs:get_as_string(2019, 16).

%% Tests
main_test_() ->
  Input = get_input(),
  [ {"Part 1", timeout, 60, ?_assertEqual("84970726", fft(Input, 100))}
  , {"Part 2", timeout, 60, ?_assertEqual("47664469", fft2(Input, 100))}
  ].

pattern_test_() ->
  [ ?_assertEqual([1, 0, -1, 0, 1, 0, -1, 0],
                  lists:map(fun(Pos) -> pattern(Pos, 1) end, lists:seq(0, 7)))
  , ?_assertEqual([0, 1, 1, 0, 0, -1, -1, 0, 0],
                  lists:map(fun(Pos) -> pattern(Pos, 2) end, lists:seq(0, 8)))
  ].

ex1_test_() ->
  ?_assertEqual("01029498", fft("12345678", 4)).

ex2_test_() ->
  ?_assertEqual("84462026", fft2("03036732577212944063491565474664", 100)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
