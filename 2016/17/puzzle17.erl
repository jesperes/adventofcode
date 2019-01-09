%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2019, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created :  9 Jan 2019 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(puzzle17).
-export([start/0]).
-include_lib("eunit/include/eunit.hrl").

start() ->
    %% eunit:test(?MODULE, [verbose]).
    astar2:astar({0, 0, ""}, 
                 fun is_end/1,
                 fun cost/1,
                 fun neighbors/1,
                 fun dist/2).
                             
%%% Search callback
is_end({3, 3, _}) -> true ;
is_end(_) -> false.

cost(Node) -> 1.
neighbors(Node) -> [].
dist(A, B) -> 1.

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || 
                      <<N>> <= erlang:md5(S)]).
