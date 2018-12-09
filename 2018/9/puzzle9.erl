-module(puzzle9).
-export([start1/0,start2/0]).

start1() ->
    %% start(419, 72164).
    start(30, 5807).

start2() ->
    start(419, 72164 * 100).

start(NumPlayers, LastMarble) ->
    test(),
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

%% Insert element at index, return resulting list.
insert(Elem, 0, List) ->
    [Elem|List];
insert(Elem, N, [First|List]) ->
    [First|insert(Elem,N-1,List)].

%% Remove element at index, return {Elem, Remaining}.
remove(0, [Elem|List]) ->
    {Elem, List};
remove(N, [Elem|List]) ->
    {Removed, Remaining} = remove(N-1,List),
    {Removed, [Elem|Remaining]}.

test() ->
    [1,3,4,1,5,6] = insert(1, 3, [1,3,4,5,6]),
    {4, [1,2,3,5]} = remove(3, [1,2,3,4,5]).

marble_game(N, _, _CurrentPos, _NumPlayers, LastMarble, Scores) when N > LastMarble ->
    Scores;

marble_game(N, Ring, CurrentPos, NumPlayers, LastMarble, Scores) when (N rem 23 == 0) ->
    Player = N rem NumPlayers,
    MarbleToRemove = ((CurrentPos - 7) + length(Ring)) rem length(Ring),
    {L1, [Removed|L2]} = lists:split(MarbleToRemove, Ring),
    NewScores = score(Player, N + Removed, Scores),
    {NewRing, NewCurrentPos} = 
        case L2 of
            [] -> 
                {L1, 0};
            _ ->
                {L1 ++ L2, length(L1)}
        end,
    marble_game(N + 1, NewRing, NewCurrentPos, NumPlayers, LastMarble, NewScores);

marble_game(N, Ring, CurrentPos, NumPlayers, LastMarble, Scores) ->
    progress(N),
    {NewRing, NewCurrentPos} = 
        case (CurrentPos + 2) rem length(Ring) of
            0 ->
                {Ring ++ [N], length(Ring)};
            P ->
                {L1, L2} = lists:split(P, Ring),
                {L1 ++ [N|L2], length(L1)}
        end,
    marble_game(N + 1, NewRing, NewCurrentPos, NumPlayers, LastMarble, Scores).

progress(N) when N rem 1000 == 0 ->
    io:format("Placing marble ~w...~n", [N]);
progress(_) ->
    ok.
