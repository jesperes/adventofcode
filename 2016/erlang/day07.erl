-module(day07).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

%%% Returns true iff at least one "abba" sequence has been found,
%%% and no "abba" sequence has been found inside brackets.
contains_abba([], _Inside, Found, FoundInBracket) ->
    Found and not FoundInBracket;
contains_abba([X,Y,Y,X|Xs], Inside, _Found, FoundInBracket) when X /= Y ->
    contains_abba(Xs, Inside, true, Inside or FoundInBracket);
contains_abba([$[|Xs], _Inside, Found, FoundInBracket) ->
    contains_abba(Xs, true, Found, FoundInBracket);
contains_abba([$]|Xs], _Inside, Found, FoundInBracket) ->
    contains_abba(Xs, false, Found, FoundInBracket);
contains_abba([_|Xs], Inside, Found, FoundInBracket) ->
    contains_abba(Xs, Inside, Found, FoundInBracket).

contains_abba(X) ->
    contains_abba(X, false, false, false).

contains_abba_test() ->
    true = contains_abba("abba[mnop]qrst"),
    false = contains_abba("abcd[bddb]xyyx"),
    false = contains_abba("aaaa[qwer]tyui"),
    true = contains_abba("ioxxoj[asdfgh]zxcvbn").

large_test() ->
    N = length(lists:filter(fun(X) ->
				    contains_abba(X)
			    end,
			    utils:read_file_lines("input07.txt"))),
    ?debugFmt("Number of IPs supporting TLS: ~w", [N]).
