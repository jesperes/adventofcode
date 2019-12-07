-module(aoc2016_day14).
-include_lib("eunit/include/eunit.hrl").

-define(KEY_STRETCH, 2016).
-define(INPUT, "ahsbgdzn").

-export([profile/0]).

%% ****** Process <0.733.0>    -- 100.00 % of profiled time ***
%% FUNCTION                                                    CALLS        %       TIME  [uS / CALLS]
%% --------                                                    -----  -------       ----  [----------]
%% aoc2016_day14:'-main_test_/0-fun-3-'/1                      23412     0.01      21932  [      0.94]
%% aoc2016_day14:find_key/5                                    47980     0.01      26137  [      0.54]
%% erlang:'++'/2                                               48017     0.01      41114  [      0.86]
%% erlang:integer_to_list/1                                    47980     0.01      45829  [      0.96]
%% aoc2016_day14:has3/1                                      1406742     0.08     306807  [      0.22]
%% aoc2016_day14:has5/1                                      1391171     0.11     389012  [      0.28]
%% aoc2016_day14:md5_stretched/2                            47224021     2.03    7353875  [      0.16]
%% aoc2016_day14:md5_to_hexstring/1                         47200608     3.77   13682732  [      0.29]
%% aoc2016_day14:md5/1                                      47248597     4.01   14559478  [      0.31]
%% erlang:md5/1                                             47248597     5.73   20780954  [      0.44]
%% aoc2016_day14:digest_to_hexstring/1                      47200610     5.80   21027578  [      0.45]
%% aoc2016_day14:'-digest_to_hexstring/1-lbc$^0/2-0-'/2   1557620130    78.43  284548517  [      0.18]

profile() ->
  eprof:profile(fun() -> aoc2016_day14:test() end),
  eprof:analysze().

main_test_() ->
  [ {"Part 1",
     ?_assertEqual(23890, find_key(?INPUT, fun md5/1))}
  , {"Part 2",
     timeout, 600,
     ?_assertEqual(22696, find_key(?INPUT, fun(S) -> md5_stretched(S, 2016) end))}
  ].

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

        %%?debugFmt("Found 5-sequence of ~w at ~w, adding new keys ~w",
        %%          [C5, N, NewKeys]),

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
            %% ?debugFmt("Found 3-sequence of ~w at ~p", [C3, N]),
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

md5(S) ->
  erlang:md5(S).

md5_stretched(S, 0) ->
  md5(S);
md5_stretched(S, N) ->
  md5_stretched(digest_to_hexstring(md5(S)), N - 1).

digest_to_hexstring(Binary) ->
  << << (if N =< 9 -> N + $0;
            true -> N + 87
         end):8 >> || <<N:4>> <= Binary >>.

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
  [ ?_assertNot(        has5(md5("abc0")))
  , ?_assertEqual(16#e, has5(md5("abc816")))
  , ?_assertEqual(9,    has5(md5("abc200")))
  ].

digest_to_hexstring_test_() ->
  ?_assertEqual(<<"577571be4de9dcce85a041ba0410f29f">>,
                digest_to_hexstring(md5("abc0"))).

md5_stretched_test_() ->
  ?_assertEqual(<<"a107ff634856bb300138cac6568c0f24">>,
                digest_to_hexstring(md5_stretched("abc0", 2016))).
