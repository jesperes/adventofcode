-module(aoc2016_day09).
-include_lib("eunit/include/eunit.hrl").

read_marker([$)|Xs], Acc) ->
  {Acc, Xs};
read_marker([$\n|Xs], Acc) ->
  %% Ignore whitespace
  read_marker(Xs, Acc);
read_marker([X|Xs], Acc) ->
  {Acc0, Xs0} = read_marker(Xs, Acc),
  {[X|Acc0], Xs0}.

repeat(0, _) ->
  [];
repeat(N, Text) when N > 0 ->
  Text ++ repeat(N - 1, Text);
repeat(_, _) ->
  [].

decompress([]) ->
  [];
decompress([$\n|Rest]) ->
  decompress(Rest);
decompress([$(|Rest]) ->
  {MarkerText, AfterMarker} = read_marker(Rest, []),
  [NumChars, RepeatCount] = lists:map(fun list_to_integer/1, string:lexemes(MarkerText, "x")),

  %% Separate the text to repeat from the text coming after.
  {TextToRepeat, AfterExpandedText} = lists:split(NumChars, AfterMarker),

  %% Repeat the text and continue compressing.
  repeat(RepeatCount, TextToRepeat) ++ decompress(AfterExpandedText);
decompress([X|Rest]) ->
  [X|decompress(Rest)].


find_marker_str(Index, Binary) ->
    case binary:at(Binary, Index) of
	$) ->
	    [];
	Ch ->
	    [Ch|find_marker_str(Index + 1, Binary)]
    end.

find_marker(Index, Binary) ->
    MarkerStr = find_marker_str(Index, Binary),
    [NumChars, RepeatCount] =
	lists:map(fun list_to_integer/1, string:lexemes(MarkerStr, "x")),
    {NumChars, RepeatCount, Index + length(MarkerStr) + 1}.

find_marker_test() ->
    ?assertEqual({2, 2, 6}, find_marker(2, <<"a(2x2)b">>)).


%%% decompress/4
%%%
%%% Compute the decompressed length of a binary according to the rules
%%% stated in the 2016 day 9 part 2 problem. As opposed to the part 1
%%% solution, this does not copy any part of the binary. Instead it
%%% will only compute the length of each repeated string. The input we
%%% run this on will recursively repeat strings such that the full
%%% decompressed string is somewhere around ~12GB.
%%%
%%% However, since all the work is done by reading
%%% sub-strings/characters from the binary, we need to be very careful
%%% with boundary conditions and off-by-one errors.
%%%
%%% Start + Length: specifies the portion of the binary to compute the
%%% decompressed length of.
%%%
%%% Binary: The input binary.
decompress2(Start, Length, _, BinSize) when (Start >= BinSize) or
                                            (Length == 0) ->
  0;
decompress2(Start, Length, Binary, BinSize) ->
  case binary:at(Binary, Start) of
    $\n ->
      %% skip newlines
      decompress2(Start + 1, Length - 1, Binary, BinSize);
    $( ->
      %% Scan marker
      {NumChars, RepeatCount, IndexAfterMarker} = find_marker(Start + 1, Binary),

      %% Recursively descend into the text to be repeated
      %% according to the marker.
      SubLength = decompress2(IndexAfterMarker, NumChars, Binary, BinSize),

      %% Compute where we are to continue decompressing, i.e.
      %% after the marker and the text to be repeated.
      NextIndex = IndexAfterMarker + NumChars,

      %% Compute the remaining length. When decompressing
      %% expanded markers, this will not be the full length
      %% remaining of the entire string, but only of the
      %% expanded marker we are decompressing.
      LengthRemaining = Length - (NextIndex - Start),

      %% Decompress the rest of the string and add the computed
      %% length of the decompressed marker string.  (This is
      %% where the actual optimization occurs: instead of
      %% actually concatenating copies of the repeated text, we
      %% simply calculate the resulting length. The left term
      %% here will in the "long input case" approach 12^9.)
      SubLength * RepeatCount + decompress2(NextIndex,
                                            LengthRemaining,
                                            Binary, BinSize);
    _ ->
      1 + decompress2(Start + 1, Length - 1, Binary, BinSize)
  end.

decompress2(Binary) ->
  decompress2(0, byte_size(Binary), Binary, byte_size(Binary)).

%%% ===========================================================================
%%% Tests
%%% ===========================================================================

main_test_() ->
  [ {"Part 1",
     ?_assertEqual(74532, length(decompress(inputs:get_as_string(2016, 9))))}
  , {"Part 2",
     ?_assertEqual(11558231665, decompress2(inputs:get_as_binary(2016, 9)))}
  ].

unit_test_() ->
  [ ?_assertEqual("ADVENT", decompress("ADVENT"))
  , ?_assertEqual("ABBBBBC", decompress("A(1x5)BC"))
  , ?_assertEqual("XYZXYZXYZ", decompress("(3x3)XYZ"))
  , ?_assertEqual("ABCBCDEFEFG", decompress("A(2x2)BCD(2x2)EFG"))
  , ?_assertEqual("(1x3)A", decompress("(6x1)(1x3)A"))
  , ?_assertEqual("X(3x3)ABC(3x3)ABCY", decompress("X(8x2)(3x3)ABCY"))
  , ?_assertEqual({"123", "rest"}, read_marker("123)rest", []))
  , ?_assertEqual("2x2", find_marker_str(2, <<"a(2x2)b">>))
  , ?_assertError(badarg, find_marker_str(2, <<"abcd">>))
  , ?_assertError(badarg, find_marker_str(20, <<"a(2x2)b">>))
  , ?_assertEqual(length("XABCABCABCABCABCABCY"), decompress2(<<"X(8x2)(3x3)ABCY">>))
  , ?_assertEqual(241920, decompress2(<<"(27x12)(20x12)(13x14)(7x10)(1x12)A">>))
  , ?_assertEqual(445, decompress2(<<"(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN">>))
  ].
