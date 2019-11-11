-module(digest).

-export([to_hexstring/1,
         benchmark/0
        ]).

-include_lib("eunit/include/eunit.hrl").

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

to_hexstring(Binary) ->
  to_hexstring1(Binary).

%% Implementation 1
to_hexstring1(Binary) ->
  to_hexstring1(Binary, <<>>).
to_hexstring1(<<>>, Acc) ->
  Acc;
to_hexstring1(<<N:4,Rest/bits>>, Acc) ->
  to_hexstring1(Rest, <<Acc/binary, (to_hex(N))>>).




%% Benchmarks
benchmark() ->
  Reps = 100000,
  Impls = [to_hexstring1],
  lists:foreach(
    fun(Impl) ->
        {Timer, _} =
          timer:tc(
            fun() ->
                lists:foreach(
                  fun(N) ->
                      apply(?MODULE, Impl, [erlang:md5(<<N>>)])
                  end, lists:seq(1, Reps))
            end),
        io:format("~p: ~p usecs/iter~n",
                  [Impl, Timer/Reps])
    end, Impls).
