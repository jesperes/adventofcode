-module(puzzle9).
-export([main/0]).

main() ->
    {{part1, start1()},
     {part2, start2()}}.

start1() ->
    start(419, 72164).

start2() ->
    start(419, 72164 * 100).

start(NumPlayers, LastMarble) ->
    NewScores = marble_game(1, 
                            queue:from_list([0]),
                            NumPlayers, LastMarble, #{}),
    find_max_value(NewScores).
        
find_max_value(Map) ->
    maps:fold(fun(_,V,Max) when V > Max -> V;
                 (_,_,Max) -> Max
              end, 0, Map).

score(Player, Marble, Scores) ->
    maps:update_with(Player, fun(V) -> V + Marble end, 
                     Marble, Scores).

marble_game(N, _, _NumPlayers, LastMarble, Scores) when N > LastMarble ->
    Scores;

marble_game(N, Ring, NumPlayers, LastMarble, Scores) when (N rem 23 == 0) ->
    R0 = rotate_ccw(Ring, 7),
    {Removed,NewRing} = pop(R0),
    NewScores = score(N rem NumPlayers, Removed + N, Scores),
    marble_game(N + 1, NewRing, NumPlayers, LastMarble, NewScores);

marble_game(N, Ring, NumPlayers, LastMarble, Scores) ->
    R1 = rotate_cw(Ring, 2),
    R2 = push(R1, N),    
    marble_game(N + 1, R2, NumPlayers, LastMarble, Scores).

pop(Queue) ->
    {{value, Removed},Q} = queue:out_r(Queue),
    {Removed,Q}.

push(Queue, Value) ->
    queue:in(Value, Queue).

rotate_ccw(Queue) ->
    {{value, Head}, Q0} = queue:out(Queue),
    queue:in(Head, Q0).

rotate_ccw(Queue, N) ->
    lists:foldl(
      fun(_, Q) ->
              rotate_ccw(Q)
      end, Queue, lists:seq(1, N)).

rotate_cw(Queue) ->
    {{value, Head}, Q0} = queue:out_r(Queue),
    queue:in_r(Head, Q0).

rotate_cw(Queue, N) ->
    lists:foldl(
      fun(_, Q) ->
              rotate_cw(Q)
      end, Queue, lists:seq(1, N)).
