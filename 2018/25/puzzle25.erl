%%% @author  <jespe@LAPTOP-P6HKA27J>
%%% @copyright (C) 2019, 
%%% @doc
%%%
%%% @end
%%% Created :  2 Feb 2019 by  <jespe@LAPTOP-P6HKA27J>

-module(puzzle25).
-export([main/0]).
-compile([export_all]).

-define(MAX_CONSTELLATION_SEP, 3).

main() ->
    {{part1, part1()},
     {part2, ok}}.

part1() ->
    InitConsts = get_points("input.txt"),
    merge(InitConsts).

merge(Consts) ->
    NewConst = merge_consts(Consts),
    
    Len1 = maps:size(Consts),
    Len2 = maps:size(NewConst),
    
    if Len1 =/= Len2 -> merge(NewConst);
       true -> maps:size(NewConst)
    end.

%% Merge all mergable constellations
merge_consts(Consts) ->
    lists:foldl(fun({C1, C2}, Map) ->
			C2p = maps:get(C2, Map, []),
			case maps:is_key(C1, Map) of
			    true ->
				maps:update_with(C1, 
						 fun(X) ->
							 X ++ C2p
						 end, maps:remove(C2, Map));
			    _ ->
				Map
			end
		end, Consts, mergable_consts(Consts)).

%% Return a list of tuples {C1, C2} of mergable constellations.
mergable_consts(Consts) ->
    Nums = maps:keys(Consts),
    [{C1, C2} ||
	C1 <- Nums,
	C2 <- Nums,
	C1 < C2,
	is_mergeable(C1, C2, Consts)].

%% Generalized manhattan distance
dist({X1, Y1, Z1, W1}, {X2, Y2, Z2, W2}) ->
    abs(X1 - X2) + abs(Y1 - Y2) + abs(Z1 - Z2) + abs(W1 - W2).

%% Two constellations are mergeable if there is at least one pair of
%% points separated by no more than MAX_CONSTELLATION_SEP.
is_mergeable(C1, C2, Consts) ->
    lists:any(fun(Dist) ->
		      Dist =< ?MAX_CONSTELLATION_SEP
	      end, [dist(P1, P2) ||
		       P1 <- maps:get(C1, Consts, []),
		       P2 <- maps:get(C2, Consts, [])
		   ]).


%%% Parser
get_points(Filename) ->    
    {ok, Binary} = file:read_file(Filename),
    {_, Map} = 
	lists:foldl(fun(Line, {N, Map}) ->
			    Point = list_to_tuple(
				      lists:map(fun list_to_integer/1, 
						string:tokens(Line, ","))),
			    {N + 1, maps:put(N, [Point], Map)}
		    end, {0, #{}}, string:tokens(binary_to_list(Binary), "\n\r")),
    Map.
