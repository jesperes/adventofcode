-module(day05).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

to_hex(C) when C < 10 ->
    C + $0;
to_hex(C) ->
    $a - 10 + C.

to_hex_test() ->
    $4 = to_hex(4),
    $a = to_hex(10),
    $f = to_hex(15).

has_zero_prefix(0, <<X:4,_Rest/bitstring>>) ->
    {true, X};
has_zero_prefix(N, <<0:4,Rest/bitstring>>) ->
    has_zero_prefix(N - 1, Rest);
has_zero_prefix(_, <<_Rest/bitstring>>) ->
    false.

has_zero_prefix_test() ->
    {true, _} = has_zero_prefix(5, crypto:hash(md5, "abc3231929")),
    false = has_zero_prefix(5, crypto:hash(md5, "abc3231926")).

analyze_hash(DoorID, Index) ->
    has_zero_prefix(5, crypto:hash(md5, DoorID ++ integer_to_list(Index))).

analyze_hash_test() ->
    {true, 1} = analyze_hash("abc", 3231929),
    {true, 8} = analyze_hash("abc", 5017308),
    {true, 15} = analyze_hash("abc", 5278568).

compute_nth_password_char(Index, Input) ->
    case analyze_hash(Input, Index) of
	{true, C} ->
	    Chex = to_hex(C),
	    ?debugFmt("Password char ~w: ~s", [Index, [Chex]]),
	    {Index, Chex};
	false ->
	    compute_nth_password_char(Index + 1, Input)
    end.

%%% compute_password/3
compute_password(0, _, _) ->
    [];
compute_password(N, Index, Input) ->
    {NextIndex, P} = compute_nth_password_char(Index, Input),
    [P|compute_password(N - 1, NextIndex + 1, Input)].

compute_password(Input) ->
    Password = compute_password(8, 0, Input),
    ?debugFmt("Password(~s) = ~s~n", [Input, Password]),
    Password.

compute_password_1_test() ->
    compute_password("abc").

compute_password_2_test() ->
    compute_password("ojvtpuvg").

