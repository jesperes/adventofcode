%%% @author  <jespe@LAPTOP-P6HKA27J>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 22 Dec 2018 by  <jespe@LAPTOP-P6HKA27J>

-module(puzzle22).
-compile([export_all]).

start() ->
    Depth = 510,
    Target = {10,10},
    Cache = #{},
    Map = map_cave(Target, Depth, Cache),
    io:format("~s~n", [to_string(Map, 20)]),
    risk_level({0, 0}, Target, Map).

region_type_to_char(Pos, Map) ->
    [C] = atom_to_list(maps:get(Pos, Map, ' ')),
    C.

to_string(Map, Width) ->
    [[region_type_to_char({X,Y}, Map) || X <- lists:seq(0, Width)] ++ "\n" 
     || Y <- lists:seq(0, Width)].

risk_level('.') -> 0;
risk_level('=') -> 1;
risk_level('|') -> 2.
    
risk_level(Start, Target, Map) ->
    {Xs, Ys} = Start,
    {Xt, Yt} = Target,
    lists:sum([risk_level(maps:get({X, Y}, Map)) 
	       || X <- lists:seq(Xs, Xt),
		  Y <- lists:seq(Ys, Yt)]).


geologic_index({0, 0}, _Target, _Depth, Cache) ->
    maps:put({index, {0, 0}}, 0, Cache);
geologic_index(Target, Target, _Depth, Cache) ->
    maps:put({index, Target}}, 0, Cache);
geologic_index({X, 0}, _Target, _Depth, Cache) ->
    maps:put({index, {X, 0}}, X * 16807, Cache);
geologic_index({0, Y}, _Target, _Depth, Cache) ->
    maps:put({index, {0, Y}}, Y * 48271, Cache);
geologic_index({X, Y}, Target, Depth, Cache) ->
    P1 = {X - 1, Y},
    P2 = {X, Y - 1},
    C0 = erosion_level(P1, Target, Depth, Cache),
    C1 = erosion_level(P2, Target, Depth, C0),
    Index = 
	maps:get({index, P1}, C1) * 
	maps:get({index, P2}, C1),
    maps:puts({X, Y}, Index, C1).

erosion_level(Pos, Target, Depth, Cache) ->
    case maps:is_key({level, Pos}, Cache) of
	true ->
	    Cache;
	false ->
	    C0 = geologic_index(Pos, Target, Depth, Cache),
	    Index = maps:get({index, Pos}, C0),
	    ErosionLevel = (Index + Depth) rem 20183,
	    maps:put({level, ErosionLevel}, C0)
    end.

region_type(Pos, Target, Depth, Cache) ->
    C0 = erosion_level(Pos, Target, Depth, Cache),
    Mod = maps:get({level, Pos}, C0) rem 3,
    case Mod of
	0 -> '.'; %% Rocky
	1 -> '='; %% Wet
	2 -> '|'  %% Narrow
    end.

map_cave(Target, Depth, Cache) ->
    map_cave_down(0, Target, Depth, #{}).

map_cave_down(Y, Target, Depth, Map) ->
    M0 = maps:put({0, Y}, region_type({0, Y}, Target, Depth), Map),
    M1 = map_cave_diag({1, Y - 1}, Target, Depth, M0),
    case maps:is_key(Target, M1) of
	true ->
	    M1;
	false ->
	    map_cave_down(Y + 1, Target, Depth, M1)
    end.

map_cave_diag({_X, Y}, _Target, _Depth, Map) when Y < 0 ->
    Map;
map_cave_diag({X, Y} = Pos, Target, Depth, Map) ->
    M0 = maps:put(Pos, region_type(Pos, Target, Depth), Map),
    map_cave_diag({X + 1, Y - 1}, Target, Depth, M0).
