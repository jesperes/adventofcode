%%% @author  <jespe@LAPTOP-P6HKA27J>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 22 Dec 2018 by  <jespe@LAPTOP-P6HKA27J>

-module(puzzle22).
-include_lib("eunit/include/eunit.hrl").
-export([main/0]).
-compile([export_all]).


main() ->
    {{part1, part1(5913, {8, 701})},
     {part2, part2(5913, {8, 701})}}.

part1(Depth, {X, Y}) ->
    State = #{depth => Depth, target => {X, Y, torch}},    
    {RiskLevel, _} = risk_level(State),
    RiskLevel.

part2(Depth, {X, Y}) ->
    State = #{depth => Depth, target => {X, Y, torch}},    
    shortest_path(State).

geologic_index_test_() ->
    Depth = 510,
    X = 10, 
    Y = 10,
    State = #{depth => Depth, target => {X, Y, torch}},

    [
     ?_assertMatch({0, _}, geologic_index({0, 0}, State)),
     ?_assertMatch({48271, _}, geologic_index({0, 1}, State)),
     ?_assertMatch({16807, _}, geologic_index({1, 0}, State)),
     ?_assertMatch({145722555, _}, geologic_index({1, 1}, State)),
     ?_assertMatch({0, _}, geologic_index({X, Y}, State))
    ].

part1_test_() ->
    [
     ?_assertEqual(114, part1(510, {10, 10})),
     ?_assertEqual(6256, part1(5913, {8, 701}))
    ].

part2_test_() ->
    [
     ?_assertEqual(45, part2(510, {10, 10}))
    ].

geologic_index({0, 0}, State) ->
    {0, State};
geologic_index({0, Y}, State) ->
    {Y * 48271, State};
geologic_index({X, 0}, State) ->
    {X * 16807, State};
geologic_index({X, Y} = Pos, State) ->
    {Tx, Ty, _} = maps:get(target, State),
    case {Tx, Ty} of
        Pos ->
            {0, State};
        _ ->
            {E1, S0} = erosion_level({X - 1, Y}, State),
            {E2, S1} = erosion_level({X, Y - 1}, S0),
            {E1 * E2, S1}
    end.

erosion_level(Pos, State) ->
    case maps:is_key(Pos, State) of
        true ->
            ErosionLevel = maps:get(Pos, State),
            {ErosionLevel, State};
        false ->
            {GI, S0} = geologic_index(Pos, State),
            ErosionLevel = (GI + maps:get(depth, S0)) rem 20183,
            {ErosionLevel, 
             maps:put(Pos, ErosionLevel, S0)}
    end.

region_type(Pos, State) ->    
    {ErosionLevel, S0} = erosion_level(Pos, State),
    case ErosionLevel rem 3 of
        0 -> {rocky, S0};
        1 -> {wet, S0};
        2 -> {narrow, S0}
    end.

risk_level(Pos, State) ->
    {RegionType, S0} = region_type(Pos, State),
    RL = 
        case RegionType of
            rocky -> 0;
            wet -> 1;
            narrow -> 2
        end,
    {RL, S0}.

risk_level(State) ->
    risk_level({0, 0, torch}, maps:get(target, State), State).

risk_level({Xs, Ys, _}, {Xt, Yt, _}, State) ->
    PosList = 
        [{X, Y} ||
            X <- lists:seq(Xs, Xt),
            Y <- lists:seq(Ys, Yt)],
    lists:foldl(fun(Pos, {N, StateIn}) ->
                        {RL, S0} = risk_level(Pos, StateIn),
                        {RL + N, S0}
                end, {0, State}, PosList).

tools() ->
    [gear, torch, neither].

nbrs({X, Y, _, _}, State) ->
    Adj = [{Pos, Tool} ||
              Tool <- tools(),
              Pos <- [{X - 1, Y},
                      {X + 1, Y},
                      {X, Y + 1},
                      {X, Y - 1}]],
    
    Fun = fun({{Xa, Ya}, Tool}, {Acc, S0}) when Xa >= 0, Ya >= 0 ->
                  {RT, S1} = region_type({X, Y}, S0),
                  Node = 
                      case valid_region_type(Tool, RT) of
                          true ->
                              [{Xa, Ya, Tool, RT}|Acc];
                          false ->
                              Acc
                      end,
                  {Node, S1};
             (_, Acc) ->
                  Acc
          end,
    
    lists:foldl(Fun, {[], State}, Adj).

valid_region_type(gear, rocky) -> true;
valid_region_type(gear, wet) -> true;
valid_region_type(torch, rocky) -> true;
valid_region_type(torch, narrow) -> true;
valid_region_type(neither, wet) -> true;
valid_region_type(neither, narrow) -> true;
valid_region_type(_, _) -> false.

get_node(X, Y, T, S) ->
    {RT, S0} = region_type({X, Y}, S),
    {{X, Y, T, RT}, S0}.

shortest_path(#{target := {Xg, Yg, Tg}} = State) ->
    {Start, S0} = get_node(0, 0, torch, State),
    {Goal, S1} = get_node(Xg, Yg, Tg, S0),
    
    CostFn = fun({X, Y, Tool, _}, S) -> 
		     C = abs(X - Xg) + abs(Y - Yg),
		     {if Tg == Tool -> C;
			 true -> C + 7
    		      end, S}
    	     end,
    
    NbrFn = fun nbrs/2,
    
    DistFn = fun({_, _, Tool1, _}, {_, _, Tool2, _}, S) -> 
    		     {if Tool1 == Tool2 -> 1;
    			 true -> 8
    		      end, S}
    	     end,
    
    {Dist, _} = astar2:astar(Start, Goal, CostFn, NbrFn, DistFn, S1),
    Dist.


    
    

    
