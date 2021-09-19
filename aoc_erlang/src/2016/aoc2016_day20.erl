-module(aoc2016_day20).
-include_lib("eunit/include/eunit.hrl").

-define(MAX_IP, 4294967295).

lowest_non_blocked_ip(Ranges) ->
  lowest_non_blocked_ip(Ranges, 0).

lowest_non_blocked_ip(Ranges, N) ->
  case find_first_range(N, Ranges) of
    {_Lower, Upper} ->
      %% N is blocked because it is in the range {Lower, Upper}.
      %% Continue with the first integer *not* blocked by this range.
      lowest_non_blocked_ip(Ranges, Upper + 1);
    false ->
      %% N is not blocked.
      N
  end.

%% Return the first range which covers the given integer, or false if
%% there is no such range.
find_first_range(_N, []) -> false;
find_first_range(N, [{L, U} = Range|_]) when (N >= L) and (N =< U) ->
  Range;
find_first_range(N, [_|Rest]) ->
  find_first_range(N, Rest).

%% Return the number of IPs which are not blocked.
num_non_blocked_ips(Ranges) ->
  num_non_blocked_ips(Ranges, 0, 0).

num_non_blocked_ips(Ranges, Acc, N) when N =< ?MAX_IP ->
  %% Find the next non-blocked IP.
  case lowest_non_blocked_ip(Ranges, N) of
    NB when NB =< ?MAX_IP ->
      %% Next non-blocked IP is a valid IP, so increment acc and
      %% continue with next.
      num_non_blocked_ips(Ranges, Acc + 1, NB + 1);
    _ ->
      %% Next non-blocked IP is not a valid IP, so we're done.
      Acc
  end.

%%% Testdata

testdata() ->
  lists:sort([{5, 8},
              {0, 2},
              {4, 7}]).

realdata() ->
  Lines = inputs:get_as_lines(2016, 20),
  lists:sort(
    lists:map(fun(Line) ->
                  [L, U] = string:tokens(Line, " -"),
                  {list_to_integer(L),
                   list_to_integer(U)}
              end, Lines)).

%%% Testcases

ex1_test() ->
  ?assertEqual({4, 7}, find_first_range(4, testdata())),
  ?assertEqual({5, 8}, find_first_range(8, testdata())),
  ?assertEqual(3, lowest_non_blocked_ip(testdata())).

main_test_() ->
  [ {"Part 1", ?_assertEqual(17348574, lowest_non_blocked_ip(realdata()))}
  , {"Part 2", ?_assertEqual(104, num_non_blocked_ips(realdata()))}
  ].
