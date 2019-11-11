-module(aoc2016_day14).

-include_lib("eunit/include/eunit.hrl").

-define(KEY_STRETCH, 2016).
-define(INPUT, "ahsbgdzn").

-export([find_nth_key/6]).

%% compute_indexes(Fun, Indexes) ->
%%   lists:foreach(
%%     fun(Index) ->
%%         Fun(?INPUT, list_to_atom(?INPUT), Index)
%%     end, Indexes).

%% compute_caches_test_() ->
%%   Indexes = lists:seq(1, 24000),

%%   {"Computing hashes...",
%%    [ {"Part 1", fun() -> compute_indexes(fun hash_at/3, Indexes) end}
%%    , {"Part 2", timeout, 500, fun() -> compute_indexes(fun hash_at2/3, Indexes) end}
%%    ]}.

main_test_() ->
  [ {"Test input (abc)",
     {timeout, 60,
      ?_assertEqual(22728, find_nth_key("abc", 'abc', 64, fun hash_at/3))}}
  , {"Part 1",
     {timeout, 60,
      ?_assertEqual(23890, find_nth_key(?INPUT, list_to_atom(?INPUT), 64, fun hash_at/3))}}
  , {"Part 2",
     timeout, 500,
     ?_assertEqual(22696, find_nth_key(?INPUT, list_to_atom(?INPUT), 64, fun hash_at2/3))}
  ].

find_nth_key(Salt, SaltA, KeyLimit, HashFun) ->
  find_nth_key(Salt, SaltA, KeyLimit, HashFun, 0, 1).

find_nth_key(Salt, SaltA, KeyLimit, HashFun, Index, CurrKey) ->
  %% ?debugFmt("Checking key ~p at index ~p", [CurrKey, Index]),
  case is_key(Salt, SaltA, Index, HashFun) of
    true when CurrKey =:= KeyLimit ->
      ?debugFmt("Found final key ~p at index ~p", [CurrKey, Index]),
      Index;
    true ->
      ?debugFmt("Found key ~p at index ~p", [CurrKey, Index]),
      find_nth_key(Salt, SaltA, KeyLimit, HashFun, Index + 1, CurrKey + 1);
    false ->
      %% ?debugFmt("Hash at index ~p is not key.", [Index]),
      find_nth_key(Salt, SaltA, KeyLimit, HashFun, Index + 1, CurrKey)
  end.

%% Return true if the hash at the given index is a key.
is_key(Salt, SaltA, Index) ->
  is_key(Salt, SaltA, Index, fun hash_at/3).

is_key(Salt, SaltA, Index, HashFun) ->
  case has3(HashFun(Salt, SaltA, Index)) of
    false -> false;
    C ->
      lists:any(fun(Idx5) ->
                    %% ?debugFmt("Checking has5 on index ~p", [Idx5]),
                    has5(HashFun(Salt, SaltA, Idx5), C)
                end, lists:seq(Index + 1, Index + 1000))
  end.

%% Compute the md5 hash of S, and cache it using Key.  Use process
%% dictionary to cache hash values (they never
%% change). http://rosettacode.org/wiki/MD5#Erlang
md5(Key, S) ->
  case get(Key) of
    undefined ->
      Hash = md5(S),
      %% ?debugFmt("md5(~p, ~p) -> ~p", [Key, S, digest_to_hexstring(Hash)]),
      put(Key, Hash),
      Hash;
    Hash ->
      %% ?debugFmt("md5(~p, ~p) -> ~p (CACHED)", [Key, S, digest_to_hexstring(Hash)]),
      Hash
  end.

md5(S) ->
  Hash = erlang:md5(S),
  %% ?debugFmt("md5(~p) -> ~p (RAW)", [S, digest_to_hexstring(Hash)]),
  Hash.

to_hex(N) when N =< 9 -> N + 48;
to_hex(N) when N >= 16#a -> N + 87.

%% digest_to_hexstring(<<>>) ->
%%   <<>>;
%% digest_to_hexstring(<<N:4,Rest/bits>>) when N =< 9 ->
%%   Hex = N + 48,
%%   <<Hex, (digest_to_hexstring(Rest))/binary>>;
%% digest_to_hexstring(<<N:4,Rest/bits>>) when N >= 16#a ->
%%   Hex = N + 87,
%%   <<Hex, (digest_to_hexstring(Rest))/binary>>.

digest_to_hexstring(Binary) ->
  digest_to_hexstring(Binary, <<>>).

digest_to_hexstring(<<>>, Acc) ->
  Acc;
digest_to_hexstring(<<N:4,Rest/bits>>, Acc) when N =< 9 ->
  digest_to_hexstring(Rest, <<Acc/binary, (N+48)>>);
digest_to_hexstring(<<N:4,Rest/bits>>, Acc) when N >= 16#a ->
  digest_to_hexstring(Rest, <<Acc/binary, (N+87)>>).

%% Compute the hash at the given Index.
hash_at(Salt, SaltA, Index) ->
  md5({SaltA, Index}, Salt ++ integer_to_list(Index)).

%% Hash function for part 2, where we repeat md5 hashing 2016 times.
hash_at2(Salt, SaltA, Index) ->
  Key = {stretched, SaltA, Index},
  case get(Key) of
    undefined ->
      %% ?debugFmt("Computing stretched hash for index ~p", [Index]),
      Hash = hash_at2(Salt, SaltA, Index, ?KEY_STRETCH, hash_at(Salt, SaltA, Index)),
      put(Key, Hash),
      Hash;
    Hash ->
      %% ?debugFmt("Stretched has for index ~p already computed...", [Index]),
      Hash
  end.

hash_at2(_Salt, _SaltA, _Index, 0, Acc) ->
  Acc;
hash_at2(Salt, SaltA, Index, N, Acc) ->
  hash_at2(Salt, SaltA, Index, N - 1, md5(digest_to_hexstring(Acc))).

%%% Helpers

%% Returns if the given MD5 digest has a 3-letter sequence. Returns
%% the letter or the atom 'false' if no such sequence is found. Note
%% that converting the MD5 digest to a string is not necessary; we can
%% check for 3- and 5-sequences on the raw binary.
has3(Binary) when bit_size(Binary) < 12 ->
  false; %% Less than 3 chars left (3 * 4)
has3(<<C:4,C:4,C:4,_/bitstring>>) ->
  C;
has3(<<_:4,Rest/bitstring>>) ->
  has3(Rest).

%% If Hash has a 5-letter sequence of C, return true, otherwise return
%% false.
has5(Binary, _) when bit_size(Binary) < 20 ->
  false; %% Less than 5 chars left (5 * 4)
has5(<<C:4,C:4,C:4,C:4,C:4,_/bitstring>>, C) ->
  true;
has5(<<_:4,Rest/bitstring>>, C) ->
  has5(Rest, C).

%% ------------------------------------------------------------
%% Unit tests
%% ------------------------------------------------------------

has3_test_() ->
  [ ?_assertNot(        has3(md5("abc0")))
  , ?_assertEqual(8,    has3(md5("abc18")))
  , ?_assertEqual(16#e, has3(md5("abc39")))
  , ?_assertEqual(9,    has3(md5("abc92")))
  ].

has5_test_() ->
  [ ?_assertNot(has5(md5("abc0"),   0))
  , ?_assert(   has5(md5("abc816"), 16#e))
  , ?_assert(   has5(md5("abc200"), 9))
  ].

hash_at_test_() ->
  [
   %% The second of these should hit the cache.
   ?_assertEqual(md5("abc0"), hash_at("abc", 'abc', 0)),
   ?_assertEqual(md5("abc0"), hash_at("abc", 'abc', 0))
  ].

digest_to_hexstring_test_() ->
  ?_assertEqual(<<"577571be4de9dcce85a041ba0410f29f">>,
                digest_to_hexstring(md5("abc0"))).

hash_at2_test_() ->
  ?_assertMatch(<<16#a1,16#07,16#ff,_/bitstring>>,
                hash_at2("abc", 'abc', 0)).

is_key_test_() ->
  [ ?_assertNot(is_key("abc", 'abc', 18))
  , ?_assert(is_key("abc", 'abc', 39))
  , ?_assertNot(is_key("abc", 'abc', 40))
  , ?_assert(is_key("abc", 'abc', 92))
  ].

is_key2_test_() ->
  [ {timeout, 60, ?_assertNot(is_key("abc", 'abc', 5, fun hash_at2/3))}
  , {timeout, 60, ?_assert(is_key("abc", 'abc', 10, fun hash_at2/3))}
  ].

-compile([export_all]).
prof() ->
  eprof:start(),
  eprof:profile(fun() ->
                    find_nth_key("abc", 'abc', 1, fun hash_at2/3)
                end),
  eprof:analyze().
