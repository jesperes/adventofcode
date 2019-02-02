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
    %% io:format("Merging: ~p~n", [maps:size(Consts)]),
    
    Nums = lists:seq(0, maps:size(Consts) - 1),
    Mergable = 
	[{C1, C2} ||
	    C1 <- Nums,
	    C2 <- Nums,
	    C1 < C2,
	    is_mergeable(C1, C2, Consts)],
    
    {Merges, Map, Old} = 
	lists:foldl(fun({C1, C2}, {N, New, Old}) ->
			    case {maps:get(C1, Old, undefined),
				  maps:get(C2, Old, undefined)} of
				{undefined, C2p} ->
				    {N + 1,
				     maps:put(N, C2p, New),
				     maps:remove(C2, Old)};
				{C1p, undefined} ->
				    {N + 1,
				     maps:put(N, C1p, New),
				     maps:remove(C1, Old)};
				{C1p, C2p} ->
			    	    {N + 1, 
			    	     maps:put(N, C1p ++ C2p, New),
			    	     maps:remove(C2, maps:remove(C1, Old))}
			    end
		    end, {0, #{}, Consts}, Mergable),

    {_, Map0} = 
	maps:fold(fun(_, V, {N, Acc}) ->
			  {N + 1, maps:put(N, V, Acc)}
		  end, {Merges, Map}, Old),
    
    {_, Map1} = maps:fold(fun(_K, undefined, Acc) ->
				  Acc;
			     (_K, V, {N, Acc}) ->
				  {N + 1, maps:put(N, V, Acc)}
			  end, {0, #{}}, Map0),
    
    if Merges > 0 ->
	    merge(Map1);
       true ->
	    maps:size(Map1)
    end.

%% Generalized manhattan distance
dist(P1, P2) ->
    lists:foldl(fun({A, B}, Acc) ->
			abs(A - B) + Acc
		end, 0, lists:zip(P1, P2)).

%% Two constellations are mergeable if there is at least one pair of
%% points separated by no more than MAX_CONSTELLATION_SEP.
is_mergeable(C1, C2, Consts) ->
    lists:any(fun(Dist) ->
		      Dist =< ?MAX_CONSTELLATION_SEP
	      end, [dist(P1, P2) ||
		       P1 <- maps:get(C1, Consts),
		       P2 <- maps:get(C2, Consts)
		   ]).


%%% Parser
get_points(Filename) ->    
    {ok, Binary} = file:read_file(Filename),
    {_, Map} = 
	lists:foldl(fun(Line, {N, Map}) ->
			    Point = lists:map(fun list_to_integer/1, 
					      string:tokens(Line, ",")),
			    {N + 1, maps:put(N, [Point], Map)}
		    end, {0, #{}}, string:tokens(binary_to_list(Binary), "\n\r")),
    Map.
