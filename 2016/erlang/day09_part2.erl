-module(day09_part2).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

find_marker_str(Index, Binary) ->
    case binary:at(Binary, Index) of
	$) ->
	    [];
	Ch ->
	    [Ch|find_marker_str(Index + 1, Binary)]
    end.

find_marker_str_test() ->
    ?assertEqual("2x2", find_marker_str(2, <<"a(2x2)b">>)),
    ?assertError(badarg, find_marker_str(2, <<"abcd">>)),
    ?assertError(badarg, find_marker_str(20, <<"a(2x2)b">>)).

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
decompress(Start, Length, _, BinSize) when (Start >= BinSize) or
					   (Length == 0) ->
    0;
decompress(Start, Length, Binary, BinSize) ->
    case binary:at(Binary, Start) of
	$\n ->
	    %% skip newlines
	    decompress(Start + 1, Length - 1, Binary, BinSize);
	$( ->
	    %% Scan marker
	    {NumChars, RepeatCount, IndexAfterMarker} = find_marker(Start + 1, Binary),
	    
	    %% Recursively descend into the text to be repeated
	    %% according to the marker.
	    SubLength = decompress(IndexAfterMarker, NumChars, Binary, BinSize),

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
	    SubLength * RepeatCount + decompress(NextIndex, 
						 LengthRemaining,
						 Binary, BinSize);
	_ ->
	    1 + decompress(Start + 1, Length - 1, Binary, BinSize)
    end.

decompress(Binary) ->
    decompress(0, byte_size(Binary), Binary, byte_size(Binary)).

decompress_test() ->
    ?assertEqual(length("XABCABCABCABCABCABCY"), decompress(<<"X(8x2)(3x3)ABCY">>)),
    ?assertEqual(241920, decompress(<<"(27x12)(20x12)(13x14)(7x10)(1x12)A">>)),
    ?assertEqual(445, decompress(<<"(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN">>)),
    {ok, Data} = file:read_file("input09.txt"),
    ?debugFmt("Decompressed size: ~w", [decompress(Data)]).
