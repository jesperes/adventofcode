-module(day09).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

read_marker([$)|Xs], Acc) ->
    {Acc, Xs};
read_marker([$\n|Xs], Acc) ->
    %% Ignore whitespace
    read_marker(Xs, Acc);
read_marker([X|Xs], Acc) ->
    {Acc0, Xs0} = read_marker(Xs, Acc),
    {[X|Acc0], Xs0}.
read_marker_test() ->
    {"123", "rest"} = read_marker("123)rest", []).

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
    
    %%% Separate the text to repeat from the text coming after.
    {TextToRepeat, AfterExpandedText} = lists:split(NumChars, AfterMarker),

    %%% Repeat the text and continue compressing.
    repeat(RepeatCount, TextToRepeat) ++ decompress(AfterExpandedText);
decompress([X|Rest]) ->
    [X|decompress(Rest)].

decompress_test() ->
    "ADVENT" = decompress("ADVENT"),
    "ABBBBBC" = decompress("A(1x5)BC"),
    "XYZXYZXYZ" = decompress("(3x3)XYZ"),
    "ABCBCDEFEFG" = decompress("A(2x2)BCD(2x2)EFG"),
    "(1x3)A" = decompress("(6x1)(1x3)A"),
    "X(3x3)ABC(3x3)ABCY" = decompress("X(8x2)(3x3)ABCY"),
    {ok, Data} = file:read_file("input09.txt"),
    X = binary_to_list(Data),
    Decompressed = decompress(X),
    ?debugFmt("Decompressed length: ~w", [length(Decompressed)]).
