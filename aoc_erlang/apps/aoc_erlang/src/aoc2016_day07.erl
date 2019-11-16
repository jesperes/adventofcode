-module(aoc2016_day07).
-include_lib("eunit/include/eunit.hrl").

%%% Returns true iff at least one "abba" sequence has been found,
%%% and no "abba" sequence has been found inside brackets.
is_tls([], _In, Found, FoundBr) ->
  Found and not FoundBr;
is_tls([X,Y,Y,X|Xs], In, _Found, FoundBr) when X /= Y ->
  is_tls(Xs, In, true, In or FoundBr);
is_tls([$[|Xs], _In, Found, FoundBr) ->
  is_tls(Xs, true, Found, FoundBr);
is_tls([$]|Xs], _In, Found, FoundBr) ->
  is_tls(Xs, false, Found, FoundBr);
is_tls([_|Xs], In, Found, FoundBr) ->
  is_tls(Xs, In, Found, FoundBr).

is_tls(X) ->
  is_tls(X, false, false, false).

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
    nomatch -> find_bab(Abas, Str);
    _ -> true
  end.

supports_ssl(X) ->
  {A, B} = split_ip(X),
  Abas = find_aba(A, []),
  find_bab(Abas, B).

main_test_() ->
  Lines = inputs:get_as_lines(2016, 7),
  [ { "Part 1", ?_assertEqual(115, length(lists:filter(fun is_tls/1, Lines)))}
  , { "Part 2", ?_assertEqual(231, length(lists:filter(fun supports_ssl/1, Lines)))}
  ].

is_tls_test() ->
  ?assert(is_tls("abba[mnop]qrst")),
  ?assert(is_tls("ioxxoj[asdfgh]zxcvbn")),
  ?assertNot(is_tls("abcd[bddb]xyyx")),
  ?assertNot(is_tls("aaaa[qwer]tyui")).

split_ip_test() ->
  ?assertEqual({"abefij", "cdgh"}, split_ip("ab[cd]ef[gh]ij")).

supports_ssl_test() ->
  ?assert(supports_ssl("aba[bab]xyz")),
  ?assertNot(supports_ssl("xyx[xyx]xyx")),
  ?assert(supports_ssl("aaa[kek]eke")),
  ?assert(supports_ssl("zazbz[bzb]cdb")).
