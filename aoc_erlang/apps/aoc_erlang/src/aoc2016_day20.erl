-module(aoc2016_day20).
-include_lib("eunit/include/eunit.hrl").

lowest_non_blocked_ip(Ranges) ->
  lowest_non_blocked_ip(Ranges, 0).

lowest_non_blocked_ip(Ranges, N) ->
  case is_included(N, Ranges) of
    {_, U} -> lowest_non_blocked_ip(Ranges, U + 1);
    false -> N
  end.

%% Return the first range which covers the given integer.
is_included(_N, []) -> false;
is_included(N, [{L, U} = Range|_]) when (N >= L) and (N =< U) ->
  Range;
is_included(N, [_|Rest]) ->
  is_included(N, Rest).

%%%

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
  ?assertEqual({4, 7}, is_included(4, testdata())),
  ?assertEqual({5, 8}, is_included(8, testdata())),
  ?assertEqual(3, lowest_non_blocked_ip(testdata())).

main_test_() ->
  ?debugFmt("Part 2 not implemented.", []),
  [ {"Part 1", ?_assertEqual(17348574, lowest_non_blocked_ip(realdata()))}
  ].
