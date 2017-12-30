-module(day07_part2).
%% -compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

split_ip([], _) ->
    {[], []};
split_ip([$[|Xs], N) ->
    split_ip(Xs, N + 1);
split_ip([$]|Xs], N) ->
    split_ip(Xs, N - 1);
split_ip([X|Xs], N) when N == 0 ->
    {A, B} = split_ip(Xs, N),
    {[X|A], B};
split_ip([X|Xs], N) when N >= 0 ->
    {A, B} = split_ip(Xs, N),
    {A, [X|B]}.
split_ip(Ip) ->
    split_ip(Ip, 0).

split_ip_test() ->
    {"abefij", "cdgh"} = split_ip("ab[cd]ef[gh]ij").

find_aba([], Acc) ->
    Acc;
find_aba([X,Y,X|Xs], Acc) when X /= Y ->
    find_aba([Y,X|Xs], [[X,Y,X]|Acc]);
find_aba([_|Xs], Acc) ->
    find_aba(Xs, Acc).

find_bab([], _) ->
    false;
find_bab([X|Abas], Str) ->
    %% ?debugFmt("~w", [X]),
    [A,B,A] = X,
    case string:find(Str, [B,A,B]) of
	nomatch ->
	    find_bab(Abas, Str);
	_ ->
	    true
    end.
    
supports_ssl(X) ->
    {A, B} = split_ip(X),
    Abas = find_aba(A, []),
    %% ?debugFmt("~w", [Abas]),
    find_bab(Abas, B).

supports_ssl_test() ->
    true = supports_ssl("aba[bab]xyz"),
    false = supports_ssl("xyx[xyx]xyx"),
    true = supports_ssl("aaa[kek]eke"),
    true = supports_ssl("zazbz[bzb]cdb").

large_test() ->
    N = length(lists:filter(fun(X) ->
				    supports_ssl(X)
			    end,
			    utils:read_file_lines("input07.txt"))),
    ?debugFmt("Number of IPs supporting SSL: ~w", [N]).
