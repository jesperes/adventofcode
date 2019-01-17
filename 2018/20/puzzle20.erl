%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 20 Dec 2018 by Jesper Eskilson <>

-module(puzzle20).
-export([main/0]).
-compile([export_all]).

regex_puzzle() ->
    {ok, Binary} = file:read_file("input.txt"),    
    string:trim(binary_to_list(Binary)).

regex1() ->
    "^ENWWW(NEEE|SSE(EE|N))$".

regex2() ->
    "^ENWWWSSENNNN$".

regex3() ->
    "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$".

%% Compile the regex by converting it to Erlang syntax, then parsing
%% it as an Erlang term. Tada!
compile_re(RE) ->
    List = re_to_list(RE),
    {ok, Tokens, _} = erl_scan:string(List),
    case erl_parse:parse_term(Tokens) of
        {ok, Term} ->
            patch_re_term(Term);
        X ->
            io:format("Error '~p` while parsing: ~n~w~n", [X, List])
    end.

patch_re_term([]) -> [];
patch_re_term([nil]) -> [];
patch_re_term([nil|Rest]) -> patch_re_term(Rest);
patch_re_term([A|Rest]) when is_list(A) ->
    [patch_re_term(A)|patch_re_term(Rest)];
patch_re_term([A]) -> 
    list_to_atom([A]).

re_to_list([$^|Rest]) -> "[nil" ++ re_to_list(Rest);
re_to_list([$$]) -> ",nil].";
re_to_list([]) -> [];

re_to_list([$(|Rest]) -> ",[[nil" ++ re_to_list(Rest);
re_to_list([$)|Rest]) -> ",nil]]" ++ re_to_list(Rest);
re_to_list([$||Rest]) -> ",nil],[nil" ++ re_to_list(Rest);
re_to_list([A|Rest]) -> ",\"" ++ [A] ++ "\"" ++ re_to_list(Rest).


main() ->
    RE = compile_re(regex_puzzle()),
    %% RE = compile_re(regex2()),
    %% RE = compile_re(regex3()),
    StartPos = {0, 0},
    Map = map_rooms(RE, StartPos, #{StartPos => '.'}),
    M0 = patch_walls(Map),
    %% io:format("~s~n", [to_string(M0)]),
    DistMap = search_rooms(M0),
    {{part1, find_max_dist(DistMap)},
     {part2, num_far_away_rooms(DistMap)}}.
    

find_max_dist(DistMap) ->
    {_, MaxDist} =
        maps:fold(fun(Pos, V, {_, Max}) when V > Max ->
                          {Pos, V};
                     (_, _, Max) ->
                          Max
                  end, {undef, 0}, DistMap),
    MaxDist.

num_far_away_rooms(DistMap) ->
    maps:size(maps:filter(fun(_, V) -> V >= 1000 end, DistMap)).
                         


map_room(RoomPos, DoorPos, Walls, DoorType, Map, RE) ->
    Door = maps:get(DoorPos, Map, '?'),
    Room = maps:get(RoomPos, Map, '?'),
    
    M0 = 
        lists:foldl(fun(WallPos, M) ->
                            maps:put(WallPos, '#', M)
                    end, Map, Walls),
        
    MNext = 
        case {Door, Room} of
            {'#', _} ->
                %% There is a wall in the way.
                M0;

            {'-', '.'} ->
                %% We've been through this door before, no change
                M0;

            {'|', '.'} ->
                %% We've been through this door before, no change
                M0;
            
            {'?', '.'} ->
                %% We've been to this room before, but not through
                %% this door. Mark door, but no other change.
                maps:put(DoorPos, DoorType, M0);
            
            {'?', '?'} ->
                %% We have neither been through this door nor to the
                %% room.
                M1 = maps:put(DoorPos, DoorType, M0),
                maps:put(RoomPos, '.', M1)
        end,

    %% We must always continue mapping, because we may exit an
    %% existing room in another direction, finding new paths.
    map_rooms(RE, RoomPos, MNext).


map_rooms([], _, Map) ->
    Map;
map_rooms(['N'|Rest], {X, Y}, Map) -> 
    NextPos = {X, Y - 2},
    DoorPos = {X, Y - 1},
    Walls = [{X - 1, Y - 1},
             {X + 1, Y - 1}],
    map_room(NextPos, DoorPos, Walls, '-', Map, Rest);
map_rooms(['E'|Rest], {X, Y}, Map) -> 
    NextPos = {X + 2, Y},
    DoorPos = {X + 1, Y},
    Walls = [{X + 1, Y - 1},
             {X + 1, Y + 1}],
    map_room(NextPos, DoorPos, Walls, '|', Map, Rest);
map_rooms(['S'|Rest], {X, Y}, Map) -> 
    NextPos = {X, Y + 2},
    DoorPos = {X, Y + 1},
    Walls = [{X - 1, Y + 1},
             {X + 1, Y + 1}],
    map_room(NextPos, DoorPos, Walls, '-', Map, Rest);
map_rooms(['W'|Rest], {X, Y}, Map) -> 
    NextPos = {X - 2, Y},
    DoorPos = {X - 1, Y},
    Walls = [{X - 1, Y - 1},
             {X + 1, Y - 1}],
    map_room(NextPos, DoorPos, Walls, '|', Map, Rest);
map_rooms([List|Rest], Pos, Map) when is_list(List) ->
    M0 = lists:foldl(fun(RE, M) ->
                             map_rooms(RE, Pos, M)
                     end, Map, List),
    map_rooms(Rest, Pos, M0).
    
    
%% Mark remaining walls.                    
patch_walls(Map) ->
    {{MinX, MaxX}, {MinY, MaxY}} = bounds(Map),
    Unknowns = 
        [{X,Y} || 
            X <- lists:seq(MinX, MaxX),
            Y <- lists:seq(MinY, MaxY),
            maps:is_key({X, Y}, Map) /= true],
    
    lists:foldl(fun(Pos, M) ->
                        maps:put(Pos, '#', M)
                end, Map, Unknowns).

bounds(Map) ->
    UpperBound = maps:size(Map),
    
    MinX = maps:fold(fun({X, _}, _, M) when X < M -> X;
                        (_, _, M) -> M
                     end, UpperBound, Map),
    MinY = maps:fold(fun({_, Y}, _, M) when Y < M -> Y;
                        (_, _, M) -> M
                     end, UpperBound, Map),
    MaxX = maps:fold(fun({X, _}, _, M) when X > M -> X;
                        (_, _, M) -> M
                     end, -UpperBound, Map),
    MaxY = maps:fold(fun({_, Y}, _, M) when Y > M -> Y;
                        (_, _, M) -> M
                     end, -UpperBound, Map),
    {{MinX, MaxX}, {MinY, MaxY}}.
    
                       
room_to_list(Pos, Map) ->
    case Pos of
        {0, 0} ->
            "X";
        Pos ->
            atom_to_list(maps:get(Pos, Map, '?'))
    end.

to_string(Map) ->
    {{MinX, MaxX}, {MinY, MaxY}} = bounds(Map),
    [[room_to_list({X, Y}, Map) || X <- lists:seq(MinX, MaxX)] ++ "\n"
     || Y <- lists:seq(MinY, MaxY)].


is_door('|') -> true;
is_door('-') -> true;
is_door(_) -> false.
    

search_rooms(Map) ->
    search_rooms(Map, {0, 0}, 0, #{}).

search_rooms(Map, {PosX, PosY} = Pos, CurrentDist, DistMap) ->
    UpperBound = maps:size(Map),
    %% erlang:display({searching, Pos, at, CurrentDist}),

    case maps:get(Pos, DistMap, UpperBound) of
        PrevDist when PrevDist =< CurrentDist ->
            %% We've already reached this room through a path which is
            %% equal or shorter to this path.
            %% erlang:display({already_been_here, Pos}),
            DistMap;
        _ -> 
            %% We have not reached this room at all or we've reached
            %% it through a longer path.
            D0 = maps:put(Pos, CurrentDist, DistMap),

            AdjacentRooms = 
                [{X, Y} ||
                    X <- lists:seq(-1, 1),
                    Y <- lists:seq(-1, 1),
                    ((X == 0) or (Y == 0)) and (X /= Y)],
            
            %% erlang:display({adjacent, Pos, AdjacentRooms}),

            lists:foldl(fun({Dx, Dy}, DM) ->
                                DoorPos = {PosX + Dx, PosY + Dy},
                                RoomPos = {PosX + 2*Dx, PosY + 2*Dy},
                                Door = maps:get(DoorPos, Map, '#'),
                                Room = maps:get(RoomPos, Map, '?'),
                                case {Room, is_door(Door)} of
                                    {'.', true} ->
                                        %% erlang:display({into, RoomPos}),
                                        search_rooms(Map, RoomPos, CurrentDist + 1, DM);
                                    _ ->
                                        DM
                                end
                        end, D0, AdjacentRooms)
    end.


                                
                                
                            
                                

      
        
    
