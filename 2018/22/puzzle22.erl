%%% @author  <jespe@LAPTOP-P6HKA27J>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 22 Dec 2018 by  <jespe@LAPTOP-P6HKA27J>

-module(puzzle22).
-include_lib("eunit/include/eunit.hrl").
-export([main/0]).

%% 977 is too high
%% 974 is too high
%% 973 is correct

main() ->
    {{part1, part1(510, {10, 10})},
     {part2, part2(510, {10, 10})}}.

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
    [climbing_gear, torch, neither].


adjacent({X, Y, _}) ->
    [{Xa, Ya, Tool} ||
        Tool <- tools(),
        Xa <- [X - 1, X, X + 1],
        Ya <- [Y - 1, Y, Y + 1],
        Xa >= 0, Ya >= 0, {Xa, Ya} /= {X, Y},
        (Xa == X) or (Ya == Y)].
    
valid_region_type(climbing_gear, rocky) -> true;
valid_region_type(climbing_gear, wet) -> true;
valid_region_type(torch, rocky) -> true;
valid_region_type(torch, narrow) -> true;
valid_region_type(neither, wet) -> true;
valid_region_type(neither, narrow) -> true;
valid_region_type(_, _) -> false.

%% Return {ValidAdjacents, State} where ValidAdjacents are all the
%% valid adjacent states, i.e. where the tool is allowed given the
%% region's type.
filtered_adjacents(Pos, State) ->
    Adjacents = adjacent(Pos),

    {AdjacentsWithRegionType, S2} = 
        lists:mapfoldl(fun({X, Y, _Tool} = Node, S0) ->
                               {RT, S1} = region_type({X, Y}, S0),
                               {{Node, RT}, S1}
                       end, State, Adjacents),
    
    ValidAdjacents = 
        lists:filter(fun({{_, _, Tool}, RT}) ->
                             valid_region_type(Tool, RT)
                     end, AdjacentsWithRegionType),
    
    {ValidAdjacents, S2}.
                         
%% Return list of all (valid) edges for a node.
edges({_, _, Tool} = Node, State) ->
    {FAdj, S0} = filtered_adjacents(Node, State),

    %% FAdj is a list of {Node, RegionType} tuples.
    Edges = 
        lists:map(fun({{_, _, Tool1} = N, _}) when Tool == Tool1 ->
                          %% Same tool
                          {1, N};
                     ({{_, _, Tool1} = N, _}) when Tool /= Tool1 ->
                          {1 + 7, N}
                  end, 
                  FAdj),
    {{Node, Edges}, S0}.

shortest_path(State) ->
    %% dijkstra:shortest_path/3 takes a graph where the nodes are the
    %% keys, and the edges are lists of tuples {Weight, Node}.
    Xs = 0,
    Ys = 0,
    
    Start = {Xs, Ys, torch},
    Target = {Xt, Yt, torch} = maps:get(target, State),
    
    %% Construct list of nodes
    Nodes =
        [{X, Y, Tool} ||
            Tool <- tools(),
            X <- lists:seq(Xs, Xt + 100),
            Y <- lists:seq(Ys, Yt + 100)],
  
    %% Construct list of {Node, Edges}.
    {Nodes0, _S1} = 
        lists:mapfoldl(fun edges/2, State, Nodes),
    
    Graph = maps:from_list(Nodes0),
    
    dijkstra2:shortest_path(Graph, Start, Target).
