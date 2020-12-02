-module(aoc2016_day16).
-include_lib("eunit/include/eunit.hrl").

%% Replace all 0 <-> 1 and reverse the list at the same time.
rinvert([], Acc) -> Acc;
rinvert([$1|Xs], Acc) -> rinvert(Xs, [$0|Acc]);
rinvert([$0|Xs], Acc) -> rinvert(Xs, [$1|Acc]).

iterate(A) ->
  A ++ "0" ++ rinvert(A, []).

iterate(A, Size) ->
  A0 = iterate(A),
  if length(A0) >= Size -> A0;
     true               -> iterate(A0, Size)
  end.


checksum(S) ->
  C0 = checksum0(S),
  if length(C0) rem 2 == 0 -> checksum(C0);
     true                  -> C0
  end.

checksum0([]) -> [];
checksum0([A, A|Xs]) -> "1" ++ checksum0(Xs);
checksum0([A, B|Xs]) when A /= B -> "0" ++ checksum0(Xs).

fill_disk(InitState, Size) ->
  S = iterate(InitState, Size),
  S0 = lists:sublist(S, Size),
  checksum(S0).

%%% Tests

unit_test() ->
  ?assertEqual("100", iterate("1")),
  ?assertEqual("001", iterate("0")),
  ?assertEqual("11111000000", iterate("11111")),
  ?assertEqual("1111000010100101011110000", iterate("111100001010")),
  ?assertEqual("10000011110010000111110", iterate("10000", 20)),
  ?assertEqual("100", checksum("110010110100")),
  ?assertEqual("01100", fill_disk("10000", 20)).

main_test_() ->
  [ {"Part 1",
     ?_assertEqual("10100101010101101", fill_disk("11101000110010100", 272))}
  , {"Part 2", timeout, 10,
     ?_assertEqual("01100001101101001", fill_disk("11101000110010100", 35651584))}
  ].
