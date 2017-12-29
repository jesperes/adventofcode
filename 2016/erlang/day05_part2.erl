-module(day05_part2).
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

has_zero_prefix(0, <<Pos:4,Y:4,_Rest/bitstring>>) when Pos < 8 ->
    {true, Pos, Y};
has_zero_prefix(N, <<0:4,Rest/bitstring>>) ->
    has_zero_prefix(N - 1, Rest);
has_zero_prefix(_, <<_Rest/bitstring>>) ->
    false.

has_zero_prefix_test() ->
    {true, _, _} = has_zero_prefix(5, crypto:hash(md5, "abc3231929")),
    false = has_zero_prefix(5, crypto:hash(md5, "abc3231926")).

analyze_hash(DoorID, Index) ->
    has_zero_prefix(5, crypto:hash(md5, DoorID ++ integer_to_list(Index))).

analyze_hash_test() ->
    {true, 1, 5} = analyze_hash("abc", 3231929),
    false = analyze_hash("abc", 5017308),
    {true, 4, 14} = analyze_hash("abc", 5357525).

replace_char_in_string(_, _, _, []) ->
    [];
replace_char_in_string(Pos, CurrPos, Char, [X|Xs]) when Pos /= CurrPos ->
    [X|replace_char_in_string(Pos, CurrPos + 1, Char, Xs)];
replace_char_in_string(Pos, Pos, Char, [_|Xs]) ->
    [Char|replace_char_in_string(Pos, Pos + 1, Char, Xs)].

replace_char_in_string(Pos, Char, String) ->
    replace_char_in_string(Pos, 1, Char, String).

replace_char_in_string_test() ->
    "abx123" = replace_char_in_string(3, $x, "abc123").
    
compute_nth_password_char(Index, Input, Password) ->
    case analyze_hash(Input, Index) of
	{true, Pos, C} ->
	    Chex = to_hex(C),
	    case lists:nth(Pos + 1, Password) of
		$_ ->
		    %% Password position has not been assigned, we 
		    %% are done and can proceed with the next character.
		    %% ?debugFmt("Password char ~w (pos ~w): ~s", [Index, Pos, [Chex]]),
		    {Index, replace_char_in_string(Pos + 1, Chex, Password)};
		_ ->
		    %% We already have a character for this position
		    %% in the password
		    compute_nth_password_char(Index + 1, Input, Password)
	    end;
	false ->
	    compute_nth_password_char(Index + 1, Input, Password)
    end.

%%% compute_password/3
compute_password(0, _, _, Password) ->
    Password;
compute_password(N, Index, Input, Password) ->
    {NextIndex, IncompletePwd} = compute_nth_password_char(Index, Input, Password),
    ?debugMsg(IncompletePwd),
    compute_password(N - 1, NextIndex + 1, Input, IncompletePwd).

compute_password(Input) ->
    Password = compute_password(8, 0, Input, "________"),
    ?debugFmt("Password(~s) = ~s~n", [Input, Password]),
    Password.

compute_password_1_test_() ->
    {timeout, 60,
     fun() ->
	     "05ace8e3" = compute_password("abc")
     end}.

compute_password_2_test_() ->
    {timeout, 60,
     fun() ->
	     compute_password("ojvtpuvg")
     end}.

