-module(aoc2016_day14).

-include_lib("eunit/include/eunit.hrl").

-define(KEY_STRETCH, 2016).
-define(INPUT, "ahsbgdzn").

%% main_test_() ->
%%   [ {"Part 1",
%%      ?_assertEqual(23890, find_nth_key(?INPUT, list_to_atom(?INPUT), 64, fun hash_at/3))}
%%   %% , {"Part 2",
%%   %%    timeout, 500,
%%   %%    ?_assertEqual(23890, find_nth_key(?INPUT, list_to_atom(?INPUT), 64, fun hash_at2/3))}
%%   ].

find_nth_key(Salt, SaltA, KeyLimit, HashFun) ->
  find_nth_key(Salt, SaltA, KeyLimit, HashFun, 0, 1).

find_nth_key(Salt, SaltA, KeyLimit, HashFun, Index, CurrKey) ->
  ?debugFmt("Checking key ~p at index ~p", [CurrKey, Index]),
  case is_key(Salt, SaltA, Index, HashFun) of
    true when CurrKey =:= KeyLimit ->
      Index;
    true ->
      ?debugFmt("Found key ~p at index ~p", [CurrKey, Index]),
      find_nth_key(Salt, SaltA, KeyLimit, HashFun, Index + 1, CurrKey + 1);
    false ->
      ?debugFmt("Hash at index ~p is not key.", [Index]),
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
                    ?debugFmt("Checking has5 on index ~p", [Idx5]),
                    has5(HashFun(Salt, SaltA, Idx5), C)
                end, lists:seq(Index + 1, Index + 1000))
  end.

%% Compute the md5 hash of S, and cache it using Key.  Use process
%% dictionary to cache hash values (they never
%% change). http://rosettacode.org/wiki/MD5#Erlang
md5(Key, S) ->
  case get(Key) of
    undefined ->
      Hash = md5_nocache(S),
      put(Key, Hash),
      Hash;
    Hash ->
      ?debugFmt("Cache hit on ~p (~p) -> ~p", [Key, S, Hash]),
      Hash
  end.

%% Uncached md5.
md5_nocache(S) ->
  lists:flatten([io_lib:format("~2.16.0b", [N])
                 || <<N>> <= erlang:md5(S)]).

hash_at(Salt, SaltA, Index) ->
  md5({SaltA, Index}, Salt ++ integer_to_list(Index)).

%% Hash function for part 2, where we repeat md5 hashing 2016 times.
hash_at2(Salt, SaltA, Index) ->
  hash_at2(Salt, SaltA, Index, ?KEY_STRETCH, hash_at(Salt, SaltA, Index)).

hash_at2(_Salt, _SaltA, _Index, 0, Acc) ->
  Acc;
hash_at2(Salt, SaltA, Index, N, Acc) ->
  hash_at2(Salt, SaltA, Index, N - 1, md5({SaltA, Index, N}, Acc)).

%%% Helpers

%% If Hash has a 3-letter sequence of any letter, return it.
has3([]) -> false;
has3([_]) -> false;
has3([_,_]) -> false;
has3([C,C,C|_Rest]) -> C;
has3([_|Rest]) -> has3(Rest).

%% If Hash has a 5-letter sequence of C, return true, otherwise return
%% false.
has5([], _) -> false;
has5([_], _) -> false;
has5([_, _], _) -> false;
has5([_, _, _], _) -> false;
has5([_, _, _, _], _) -> false;
has5([C, C, C, C, C|_], C) -> true;
has5([_|Rest], C) -> has5(Rest, C).

%% ------------------------------------------------------------
%% Unit tests
%% ------------------------------------------------------------

has3_test_() ->
  [ ?_assertEqual($b, has3("aabbbcc"))
  , ?_assertEqual($c, has3("aabbccc"))
  , ?_assertEqual($c, has3("ccc"))
  , ?_assertNot(has3("aabb"))
  , ?_assertNot(has3(""))
  , ?_assertNot(has3("a"))
  , ?_assertNot(has3("aa"))
  , ?_assertNot(has3("ab"))
  ].

has5_test_() ->
  [ ?_assert(has5("aacccccbb", $c))
  , ?_assertNot(has5("aabbbbbccc", $c))
  ].

hash_at_test_() ->
  ?_assertEqual("577571be4de9dcce85a041ba0410f29f",
                hash_at("abc", 'abc', 0)).

is_key_test_() ->
  [ ?_assertNot(is_key("abc", 'abc', 18))
  , ?_assert(is_key("abc", 'abc', 39))
  , ?_assertNot(is_key("abc", 'abc', 40))
  , ?_assert(is_key("abc", 'abc', 92))
  ].

hash_at2_test_() ->
  ?_assertEqual("a107ff634856bb300138cac6568c0f24",
                hash_at2("abc", 'abc', 0)).

prof_test_() ->
  {timeout, 3600,
   fun() ->
       eprof:start(),
       eprof:profile(fun() ->
                         find_nth_key("abc", 'abc', 1, fun hash_at2/3)
                     end),
       eprof:analyze()
   end}.
