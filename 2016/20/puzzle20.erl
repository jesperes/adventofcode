%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2019, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created :  9 Jan 2019 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(puzzle20).
-export([start/0]).
-include_lib("eunit/include/eunit.hrl").

start() ->
    eunit:test(?MODULE, [verbose]),
    erlang:halt().

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
    {ok, Binary} = file:read_file("input.txt"),
    Lines = string:tokens(binary_to_list(Binary), "\n"),
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

part1_test() ->
    ?assertEqual(0, lowest_non_blocked_ip(realdata())).



  
