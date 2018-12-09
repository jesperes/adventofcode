-module(puzzle9).
-export([start1/0,start2/0,test/0]).

start1() ->
    %% start(419, 72164).
    test().
%% start(419, 72164).

start2() ->
    test(),
    start(419, 72164 * 100).

start(NumPlayers, LastMarble) ->
    {Time, Value} =
        timer:tc(
          fun() ->
                  NewScores = marble_game(1, 
                                          queue:from_list([0]),
                                          NumPlayers, LastMarble, #{}),
                  find_max_value(NewScores)
          end),
    {Time / (1000 * 1000), Value}.

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

test() ->
    {_, 32} = start(9, 25).

marble_game(N, _, _NumPlayers, LastMarble, Scores) when N > LastMarble ->
    Scores;

marble_game(N, Ring, NumPlayers, LastMarble, Scores) when (N rem 23 == 0) ->
    erlang:display(queue:to_list(Ring)),
    progress(N),
    R1 = rotateCCW(
           rotateCCW(
             rotateCCW(
               rotateCCW(
                 rotateCCW(
                   rotateCCW(
                     rotateCCW(Ring))))))),
    {{value, Removed},NewRing} = queue:out(R1),
    NewScores = score(N rem NumPlayers, Removed + N, Scores),
    marble_game(N + 1, NewRing, NumPlayers, LastMarble, NewScores);

marble_game(N, Ring, NumPlayers, LastMarble, Scores) ->
    erlang:display(queue:to_list(Ring)),
    progress(N),
    R1 = rotateCW(rotateCW(Ring)),
    R2 = queue:in(N, R1),
    marble_game(N + 1, R2, NumPlayers, LastMarble, Scores).

progress(N) when N rem 1000 == 0 ->
    io:format("Placing marble ~w...~n", [N]);
progress(_) ->
    ok.

rotateCW(Queue) ->
    {{value, Head},Q0} = queue:out(Queue),
    queue:in_r(Head, Q0).

rotateCCW(Queue) ->
    {{value, Tail},Q0} = queue:out_r(Queue),
    queue:in(Tail, Q0).
