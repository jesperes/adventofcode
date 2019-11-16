-module(aoc2016_day05).

-include_lib("eunit/include/eunit.hrl").

-define(INPUT, "ojvtpuvg").

to_hex(X) when X < 10 -> X + $0;
to_hex(X) -> X + $a - 10.

next_password_char(<<0,0,0:4,A:4,B:4,_/bitstring>>) ->
  {A, B};
next_password_char(_) ->
  false.

hash(Input, Index) ->
  erlang:md5(Input ++ integer_to_list(Index)).

-spec password(Input :: string()) -> { Password1 :: string(),
                                       Password2 :: string() }.
password(Input) ->
  password("", #{}, Input, 0).

password(P1, P2, Input, Index) ->
  case {length(P1), maps:size(P2)} of
    {8, 8} -> {lists:reverse(P1),
               lists:map(fun({_, X}) -> to_hex(X) end,
                         lists:sort(maps:to_list(P2)))};
    _ ->
      case next_password_char(hash(Input, Index)) of
        {A, B} ->
          NewP1 = if length(P1) < 8 -> [to_hex(A)|P1];
                     true -> P1
                  end,
          NewP2 = if A < 8 ->
                      maps:update_with(A, fun(Old) -> Old end,
                                       B, P2);
                     true ->
                      P2
                  end,
          %% io:format("P1: ~w -> ~w, P2: ~w -> ~w~n", [P1, NewP1, P2, NewP2]),
          password(NewP1, NewP2, Input, Index + 1);
        false ->
          password(P1, P2, Input, Index + 1)
      end
  end.

hash_test_() ->
  [ ?_assertNot(next_password_char(hash("abc", 0)))
  , ?_assertEqual({1, 5}, next_password_char(hash("abc", 3231929)))
  ].

main_test_() ->
  [ {"Test input", timeout, 60, ?_assertEqual({"18f47a30", "05ace8e3"}, password("abc"))}
  , {"Part 1 & 2", timeout, 60, ?_assertEqual({"4543c154", "1050cbbd"}, password(?INPUT))}
  ].
