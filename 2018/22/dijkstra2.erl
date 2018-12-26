%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 23 Dec 2018 by Jesper Eskilson <>

-module(dijkstra2).

-export([shortest_path/3, test/0]).

-define(MAX_INT, 10000000000).

shortest_path(Graph, Start, End) ->
    %% Graph: the vertex set
    Q = Graph,
    
    DistMap = #{},
    PrevMap = #{},
    
    DistMap0 = maps:put(Start, 0, DistMap),
    
    {DM, PM} = shortest_path(Q, Start, End, DistMap0, PrevMap),
    maps:get(End, DM).


smallest_value(Q, DistMap) ->
    maps:fold(fun(Node, _, {N, Min}) ->
                      Dist = maps:get(Node, DistMap, ?MAX_INT),
                      if Dist < Min ->
                              {Node, Dist};
                         true ->
                              {N, Min}
                      end
              end, {undef, ?MAX_INT}, Q).

shortest_path(Q, Start, End, DistMap, PrevMap) ->
    case maps:size(Q) of
        0 ->
            {DistMap, PrevMap};
        _ ->
            %% Take from Q the node U with minimum dist(U)
            {U, Dist} = smallest_value(Q, DistMap),
            %% D0 = maps:put(U, Dist, DistMap),
            
            %% Get the edges of this node
            Edges = maps:get(U, Q),
            
            %% Remove U from Q
            Q0 = maps:remove(U, Q),
            
            %% erlang:display(U),
            
            case U of
                End ->
                    %% We are done
                    {DistMap, PrevMap};
                _ ->
                    %% Update DistMap and PrevMap
                    {DM2, PM2} = 
                        lists:foldl(fun(Edge, {DM, PM}) ->
                                            {Weight, V} = Edge,
                                            Alt = maps:get(U, DM, ?MAX_INT) + Weight,
                                            DistV = maps:get(V, DM, ?MAX_INT),
                                            if Alt < DistV ->
                                                    DM0 = maps:put(V, Alt, DM),
                                                    PM0 = maps:put(V, U, PM),
                                                    {DM0, PM0};
                                               true ->
                                                    {DM, PM}
                                            end
                                    end, {DistMap, PrevMap}, Edges),
                    
                    shortest_path(Q0, Start, End, DM2, PM2)
            end
    end.

test() ->
    Graph = #{
              a => [{7,b},{9,c},{14,f}],
              b => [{7,a},{10,c},{15,d}],
              c => [{10,b},{9,c},{11,d},{2,f}],
              d => [{15,b},{6,e},{11,c}],
              e => [{9,f},{6,d}],
              f => [{14,f},{2,c},{9,e}]
             },
    shortest_path(Graph, a, e).
    



    
