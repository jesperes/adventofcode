%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2019, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created :  8 Jan 2019 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(puzzle14).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

-define(KEY_STRETCH, 2016).


start(KeyNum) ->
    %% find_nth_key("abc", KeyNum).
    find_nth_key("ahsbgdzn", KeyNum).

find_nth_key(Prefix, KeyNum) when KeyNum >= 1 ->
    find_nth_key(Prefix, 0, 1, KeyNum, #{}).

find_nth_key(Prefix, CurrIdx, CurrKey, KeyNum, Cache) ->
    {Index, Cache0} = find_next_key(Prefix, CurrIdx, Cache),
    if CurrKey == KeyNum ->
            io:format("Final key ~w found at index ~w~n",
                      [CurrKey, Index]),
            Index;
       true ->
            io:format("Key ~w found at index ~w~n",
                      [CurrKey, Index]),
            find_nth_key(Prefix, Index + 1, CurrKey + 1, KeyNum, Cache0)
    end.

%% Find the next "key", starting at index N. Returns {Index, Cache}.
find_next_key(Prefix, N, Cache) ->
    %% io:format("Finding next 3-sequence...~n", []),
    {S3, Char, Cache0} = find_next_md5_with_sequence(Prefix, N, inf, 3, any, Cache),

    %% io:format("Found 3-sequence at ~w, proceeding with 5-sequence...~n", [S3]),
    
    case find_next_md5_with_sequence(Prefix, S3 + 1, S3 + 1000, 5, Char, Cache0) of
        {false, Cache1} ->
            find_next_key(Prefix, N + 1, Cache1);
        {_S5, Char, Cache1} ->
            {S3, Cache1}
    end.

%% Find next MD5 which has a subsequence of SeqLen consecutive Char.
%% If Char == any, any character will do.
find_next_md5_with_sequence(_, N, N, _, _, Cache) ->
    {false, Cache};
find_next_md5_with_sequence(Prefix, N, MaxN, SeqLen, Char, Cache) ->
    %% erlang:display({find_next_md5_with_sequence, Prefix, N, MaxN, SeqLen, Char}),

    %% Cache MD5 calculations
    {Str, Cache0} = 
        cache_get({Prefix, N}, Cache, 
                  fun(_Key) ->
                          %% erlang:display({cachemiss, Key}),
                          md5_keystretch(Prefix, N, ?KEY_STRETCH)
                  end),
    
    %% Cache result from contains sequence
    {Contains, Cache1} = 
        cache_get({Str, SeqLen}, Cache0,
                  fun(_) ->
                          contains_sequence(Str, SeqLen)
                  end),

    case Contains of 
        C when is_number(C) and (Char == any) ->
            {N, C, Cache};
        C when Char == C ->
            {N, C, Cache};
        _ ->
            find_next_md5_with_sequence(Prefix, N + 1, MaxN, SeqLen, Char, Cache1)
    end.

%%% Helpers

key(Prefix, Num) ->
    Prefix ++ integer_to_list(Num).

md5(S) ->
    %% http://rosettacode.org/wiki/MD5#Erlang
    lists:flatten([io_lib:format("~2.16.0b",[N]) || <<N>> <= erlang:md5(S)]).

md5_key(Prefix, Num) ->
    md5(key(Prefix, Num)).

md5_keystretch(Prefix, Num, Repeat) ->
    MD5 = md5_key(Prefix, Num),
    repeat_md5(MD5, Repeat).

repeat_md5(MD5, 0) ->
    MD5;
repeat_md5(MD5, N) ->
    repeat_md5(md5(MD5), N - 1).

contains_sequence(Str, N) ->
    contains_sequence(Str, nil, 0, N).
contains_sequence(_, C, M, M)        -> C;
contains_sequence([], _, _, _)       -> false;
contains_sequence([X|Xs], X, M, N)   -> contains_sequence(Xs, X, M + 1, N);
contains_sequence([Y|Xs], _X, _M, N) -> contains_sequence(Xs, Y, 1, N).

contains_sequence_test() ->
    ?assertEqual($c, contains_sequence("abcccd", 3)),
    ?assertEqual($c, contains_sequence("abccccd", 4)).

                             

%% Cached get; using Fun to compute value for key unless it already
%% exists. Returns {Value, Map}.
cache_get(Key, Map, Fun) ->
    case maps:get(Key, Map, undef) of
        undef ->
            Value = Fun(Key),
            {Value, Map#{Key => Value}};
        Value ->
            %% erlang:display({cachehit, Key}),
            {Value, Map}
    end.

