-module(puzzle9).
-export([start1/0]).

start1() ->
    test(),
    start(9, 25).

start(NumPlayers, LastMarble) ->
    NewScores = marble_game(1, 
                            queue:from_list([0]),
                            NumPlayers, LastMarble, #{}),
    find_max_value(NewScores).
        
find_max_value(Map) ->
    maps:fold(fun(_,V,Max) when V > Max ->
                      V;
                 (_,_,Max) ->
                      Max
              end, 0, Map).

score(Player, Marble, Scores) ->
    maps:update_with(Player, fun(V) ->
                                     V + Marble
                             end, Marble, Scores).

marble_game(N, _, _NumPlayers, LastMarble, Scores) when N > LastMarble ->
    Scores;

marble_game(N, Ring, NumPlayers, LastMarble, Scores) when (N rem 23 == 0) ->
    R0 = rotateCCW(Ring, 7),
    {Removed,NewRing} = pop(R0),
    erlang:display({removed, Removed, N}),
    erlang:display({N, queue:to_list(NewRing)}),
    NewScores = score(N rem NumPlayers, Removed + N, Scores),
    marble_game(N + 1, NewRing, NumPlayers, LastMarble, NewScores);

marble_game(N, Ring, NumPlayers, LastMarble, Scores) ->
    R1 = rotateCW(Ring, 2),
    R2 = push(R1, N),    
    erlang:display({N, queue:to_list(R2)}),
    marble_game(N + 1, R2, NumPlayers, LastMarble, Scores).

pop(Queue) ->
    {{value, Removed},Q} = queue:out(Queue),
    {Removed,Q}.

push(Queue, Value) ->
    queue:in_r(Value, Queue).

rotateCW(Queue, N) ->
    lists:foldl(
      fun(_, Q) ->
              {{value, Head}, Q0} = queue:out(Q), %% removeLast
              queue:in(Head, Q0) %% addFirst
      end, Queue, lists:seq(1, N)).

rotateCCW(Queue, N) ->
    lists:foldl(
      fun(_, Q) ->
              {{value, Head}, Q0} = queue:out_r(Q), %% removeFirst
              queue:in_r(Head, Q0) %% addLast
      end, Queue, lists:seq(1, N)).

test() ->
    test_pop(),
    test_push(),
    test_rotate_ccw().

test_pop() ->
    Q = queue:from_list([1,2,3,4]),
    {1, _} = pop(Q).

test_push() ->
    Q = queue:from_list([1,2,3,4]),
    Q1 = push(Q, 42),
    [42|_] = queue:to_list(Q1).

test_rotate_cw() ->      
    Q = queue:from_list([1,2,3,4,5,6]),
    [3,4,5,6,1,2] = queue:to_list(rotateCCW(Q, 2)).

test_rotate_ccw() ->      
    Q = queue:from_list([1,2,3,4,5,6]),
    [5,6,1,2,3,4] = queue:to_list(rotateCW(Q, 2)).
    
