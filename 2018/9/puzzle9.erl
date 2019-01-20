-module(puzzle9).
-export([main/0]).

-compile([export_all]).

%% 419 players; last marble is worth 72164 points

main() ->
    {{part1, start1()},
     {part2, start2()}}.

start1() ->
    start(419, 72164).

start2() ->
    start(419, 72164 * 100).

start(Players, LastMarblePt) ->
    marble_game([], 0, [], Players, LastMarblePt).




%% Marbles are numbered 0, 1, 2, ... n




    
marble_game(CCW, CurrentMarble, CW, Players, LastMarblePt) ->
    %% CW is list containing the marbles clockwise
    %% CCW is a list containing the marbles counter clock-wise
    ok.

ring_new() ->
    {[], []}.

%% Insert N at the current position.
ring_insert(N, {CCW, CW}) ->
    {CCW, [N|CW]}.


%% Rotate the ring counter-clockwise one step.
ring_rotate_ccw({CCW, [C|CW]}) ->
    {[C|CCW], CW};
ring_rotate_ccw({CCW, []}) ->
    %% CW part is empty, so split the list in two and reverse the last
    %% half
    {NewCCW, NewCW} = lists:split(length(CCW) div 2, CCW),
    ring_rotate_ccw({NewCCW, lists:reverse(NewCW)}).

    

    
    
