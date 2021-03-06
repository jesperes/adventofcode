-module(aoc2016_day14).
-include_lib("eunit/include/eunit.hrl").

-define(KEY_STRETCH, 2016).
-define(INPUT, "ahsbgdzn").

-export([ profile/0
        ]).

profile() ->
  eprof:profile(fun() -> aoc2016_day14:test() end),
  eprof:analyze().

find_key(Salt, HashFun) ->
  find_key(Salt, HashFun, 0, #{}, []).

find_key(Salt, HashFun, N, Threes, Keys) ->
  Digest = HashFun(Salt ++ integer_to_list(N)),

  Keys0 =
    case has5(Digest) of
       false -> Keys;
      C5 ->
        %% Get list of indices which have a matching 3-sequence
        NewKeys =
          lists:filter(fun(I) ->
                           (N - I) =< 1000
                       end, maps:get(C5, Threes, [])),
        lists:sort(NewKeys ++ Keys)
    end,

  case length(Keys0) of
    L when L >= 64 ->
      %% We have found (at least) 64 keys, so return the 64th.
      %% Eventually we must continue until we find a 5-sequence which
      %% is more than 1000 indexes larger than the largest key we
      %% have.
      lists:nth(64, Keys0);
    _ ->
      Threes0 =
        case has3(Digest) of
          false -> Threes;
          C3 ->
            maps:update_with(
              C3,
              fun(Old) -> [N|Old] end,
              [N], Threes)
        end,

      find_key(Salt, HashFun, N + 1, Threes0, Keys0)
  end.

has3(Binary) when bit_size(Binary) < 12 ->
  false; %% Less than 3 chars left (3 * 4)
has3(<<C:4,C:4,C:4,_/bitstring>>) ->
  C;
has3(<<_:4,Rest/bitstring>>) ->
  has3(Rest).

has5(Binary) when bit_size(Binary) < 20 ->
  false; %% Less than 5 chars left (5 * 4)
has5(<<C:4,C:4,C:4,C:4,C:4,_/bitstring>>) ->
  C;
has5(<<_:4,Rest/bitstring>>) ->
  has5(Rest).

md5_stretched(S, 0) ->
  erlang:md5(S);
md5_stretched(S, N) ->
  md5_stretched(digest_to_hexstring(erlang:md5(S)), N - 1).

%% This is where this puzzle spends 90% of its time, converting
%% binaries to hexstrings. Implemented as a NIF.
digest_to_hexstring(Binary) when byte_size(Binary) == 16 ->

  aoc_nifs:digest_to_hexstring(Binary).
  %% << << (if N =< 9 -> N + $0;
  %%           true -> N + 87
  %%        end):8 >> || <<N:4>> <= Binary >>.

%% ------------------------------------------------------------
%% Unit tests
%% ------------------------------------------------------------

main_test_() ->
  [ {"Part 1",
     ?_assertEqual(23890, find_key(?INPUT, fun erlang:md5/1))}
  , {"Part 2",
     timeout, 600,
     ?_assertEqual(22696, find_key(?INPUT, fun(S) -> md5_stretched(S, 2016) end))}
  ].

has3_test_() ->
  [ ?_assertNot(        has3(erlang:md5("abc0")))
  , ?_assertEqual(8,    has3(erlang:md5("abc18")))
  , ?_assertEqual(16#e, has3(erlang:md5("abc39")))
  , ?_assertEqual(9,    has3(erlang:md5("abc92")))
  ].

has5_test_() ->
  [ ?_assertNot(        has5(erlang:md5("abc0")))
  , ?_assertEqual(16#e, has5(erlang:md5("abc816")))
  , ?_assertEqual(9,    has5(erlang:md5("abc200")))
  ].

digest_to_hexstring_test_() ->
  [?_assertEqual(<<"577571be4de9dcce85a041ba0410f29f">>,
                 digest_to_hexstring(erlang:md5("abc0"))),
   ?_assertException(error, _, digest_to_hexstring(<<>>))
  ].

md5_stretched_test_() ->
  ?_assertEqual(<<"a107ff634856bb300138cac6568c0f24">>,
                digest_to_hexstring(md5_stretched("abc0", 2016))).
