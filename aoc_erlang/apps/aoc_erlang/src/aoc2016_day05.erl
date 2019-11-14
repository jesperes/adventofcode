-module(aoc2016_day05).

-include_lib("eunit/include/eunit.hrl").

-define(INPUT, "ojvtpuvg").

to_hex(X) when X < 10 -> X + $0;
to_hex(X) -> X + $a - 10.

%% "A hash indicates the next character in the password if its
%% hexadecimal representation starts with five zeroes. If it does, the
%% sixth character in the hash is the next character of the password."
next_password_char1(<<0,0,0:4,A:4,_/bitstring>>) ->
  to_hex(A);
next_password_char1(_) ->
  false.


next_password_char2(<<0,0,0:4,A:4,B:4,_/bitstring>>) when A < 8 ->
  {A, to_hex(B)};
next_password_char2(_) ->
  false.

hash(Input, Index) ->
  erlang:md5(Input ++ integer_to_list(Index)).

password1(Input) ->
   password1("", Input, 0).
password1(Password, _, _) when length(Password) =:= 8 ->
  lists:reverse(Password);
password1(Password, Input, Index) ->
  case next_password_char1(hash(Input, Index)) of
    false ->
      password1(Password, Input, Index + 1);
    A ->
      password1([A|Password], Input, Index + 1)
  end.

password2(Input) ->
  password2(#{}, Input, 0).

password2(Password, Input, Index) ->
  case maps:size(Password) of
    8 ->
      lists:map(fun({_, X}) -> X end, lists:sort(maps:to_list(Password)));
    _ ->
      case next_password_char2(hash(Input, Index)) of
        false ->
          password2(Password, Input, Index + 1);
        {PIndex, Char} ->
          case maps:is_key(PIndex, Password) of
            true ->
              password2(Password, Input, Index + 1);
            false ->
              password2(maps:put(PIndex, Char, Password),
                        Input, Index + 1)
          end
      end
  end.

hash_test_() ->
  [ ?_assertNot(next_password_char1(hash("abc", 0)))
  , ?_assertEqual($1, next_password_char1(hash("abc", 3231929)))
  , ?_assertEqual({1, $5}, next_password_char2(hash("abc", 3231929)))
  ].

main_test_() ->
  [ {"Test input (part 1)", ?_assertEqual("18f47a30", password1("abc"))}
  , {"Test input (part 2)", timeout, 60, ?_assertEqual("05ace8e3", password2("abc"))}
  , {"Part 1", ?_assertEqual("4543c154", password1(?INPUT))}
  , {"Part 2", timeout, 60, ?_assertEqual("1050cbbd", password2(?INPUT))}
  ].
