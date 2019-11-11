-module(digest_benchmark).

-export([to_hexstring/1,
         to_hexstring1/1,
         to_hexstring2/1,
         to_hexstring3/1,
         benchmark/0
        ]).

-include_lib("eunit/include/eunit.hrl").

%% Default
to_hexstring(Binary) ->
  to_hexstring1(Binary).

%% Implementation 1
to_hexstring1(Binary) ->
  to_hexstring1(Binary, <<>>).
to_hexstring1(<<>>, Acc) ->
  Acc;
to_hexstring1(<<N:4,Rest/bits>>, Acc) when N =< 9 ->
  to_hexstring1(Rest, <<Acc/binary, (N+48)>>);
to_hexstring1(<<N:4,Rest/bits>>, Acc) ->
  to_hexstring1(Rest, <<Acc/binary, (N+87)>>).

%% Implementation 2
to_hexstring2(Binary) ->
  list_to_binary(
    lists:flatten([io_lib:format("~2.16.0b",[N])
                   || <<N>> <= Binary])).

%% Implementation 3
to_hexb(<<N:4>>) when N =< 9 ->
  <<(N+48)>>;
to_hexb(<<N:4>>) ->
  <<(N+87)>>.

to_hexstring3(Binary) ->
  << (if N =< 9 -> N+48;
         true -> N+87
      end) || <<N:4/bits>> <= Binary >>.

impls() ->
  [ to_hexstring1
  %% , to_hexstring2
  , to_hexstring3
  ].

%% Benchmarks
benchmark() ->
  Reps = lists:seq(1, 1000000),
  Impls = impls(),
  Binary = erlang:md5("abc"),
  lists:foreach(
    fun(Impl) ->
        {Timer, _} =
          timer:tc(
            fun() ->
                lists:foreach(
                  fun(N) ->
                      apply(?MODULE, Impl, [Binary])
                  end, Reps)
            end),
        io:format("~p: ~p usecs/iter~n",
                  [Impl, Timer/length(Reps)])
    end, Impls).

to_hexstring_test_() ->
  Input = <<"abc0">>,
  Hash = <<"577571be4de9dcce85a041ba0410f29f">>,
  lists:map(
    fun(Impl) ->
        {atom_to_list(Impl),
         ?_assertEqual(Hash,
                       apply(?MODULE, Impl, [erlang:md5(Input)]))}
    end, impls()).
