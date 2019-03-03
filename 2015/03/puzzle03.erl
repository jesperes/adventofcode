-module(puzzle03).
-export([start/0]).

start() ->
    {ok, Bin} = file:read_file("input.txt"),
    List = binary_to_list(Bin),
    {part1(List),
     part2(List)}.

part1(List) ->
    StartPos = {0, 0},
    {_, Presents} = 
        lists:foldl(fun(C, {Pos, Map}) ->
                            NewPos = next_pos(C, Pos),
                            {NewPos, incr_map_cntr(NewPos, Map)}
                    end, {StartPos, #{StartPos => 1}}, List),
    maps:size(Presents).

part2(List) ->
    StartPos = {0, 0},
    {{_, _}, Presents} = 
        lists:foldl(fun(C, {{Pos, PosOther}, Map}) ->
                            NewPos = next_pos(C, Pos),
                            {{PosOther, NewPos}, incr_map_cntr(NewPos, Map)}
                    end, {{StartPos, StartPos}, #{StartPos => 2}}, List),
    maps:size(Presents).
    
incr_map_cntr(Key, Map) ->    
    maps:update_with(Key, fun(V) -> V + 1 end, 1, Map).

next_pos($<, {X, Y}) -> {X-1, Y};
next_pos($>, {X, Y}) -> {X+1, Y};
next_pos($v, {X, Y}) -> {X, Y+1};
next_pos($^, {X, Y}) -> {X, Y-1}. 
   

