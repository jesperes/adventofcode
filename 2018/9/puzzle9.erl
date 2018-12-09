-module(puzzle9).
-export([start1/0,start2/0,test/0]).

start1() ->
    %% start(419, 72164).
    test(),
    start(419, 72164).

start2() ->
    test(),
    start(419, 72164 * 100).

start(NumPlayers, LastMarble) ->
    {Time, Value} =
        timer:tc(
          fun() ->
                  Scores = #{},
                  NewScores = marble_game(1, [0], 0, NumPlayers, LastMarble, Scores),
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
    {_, 8317} = start(10, 1618).

marble_game(N, _, _CurrentPos, _NumPlayers, LastMarble, Scores) when N > LastMarble ->
    Scores;

marble_game(N, Ring, CurrentPos, NumPlayers, LastMarble, Scores) when (N rem 23 == 0) ->
    Len = length(Ring),
    Player = N rem NumPlayers,
    MarbleToRemove = ((CurrentPos - 7) + Len) rem Len,
    {L1, [Removed|L2]} = lists:split(MarbleToRemove, Ring),
    NewScores = score(Player, N + Removed, Scores),
    {NewRing, NewCurrentPos} = 
        case L2 of
            [] -> 
                {L1, 0};
            _ ->
                {L1 ++ L2, MarbleToRemove}
        end,
    marble_game(N + 1, NewRing, NewCurrentPos, NumPlayers, LastMarble, NewScores);

marble_game(N, Ring, CurrentPos, NumPlayers, LastMarble, Scores) ->
    progress(N),
    Len = length(Ring),
    {NewRing, NewCurrentPos} = 
        case (CurrentPos + 2) rem Len of
            0 ->
                {Ring ++ [N], Len};
            P ->
                {L1, L2} = lists:split(P, Ring),
                {L1 ++ [N|L2], P}
        end,
    marble_game(N + 1, NewRing, NewCurrentPos, NumPlayers, LastMarble, Scores).

progress(N) when N rem 1000 == 0 ->
    io:format("Placing marble ~w...~n", [N]);
progress(_) ->
    ok.
