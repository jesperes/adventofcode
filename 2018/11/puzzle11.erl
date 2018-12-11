-module(puzzle11).
-export([start1/0,start2/0]).

-define(GRID_SERIAL_NUMBER, 8).

start1() ->
    test1(),
    find_max_power_grid(1133).

start2() ->
    find_max_power_grid_anysize(1133).

hundred_digit(X) ->
    (X div 100) rem 10.

power_level(X,Y,GSN) ->
    RackId = X + 10,
    PL0 = RackId * Y + GSN,
    hundred_digit(PL0 * RackId) - 5.

power_level_3x3(X,Y,GSN) ->
    power_level_nxn(X,Y,3,GSN).

power_level_nxn(X,Y,Size,GSN) ->
    lists:foldl(fun(N,Acc) -> N + Acc end, 0, 
                [power_level(X1,Y1,GSN) || 
                    Y1 <- lists:seq(Y, Y + Size - 1),
                    X1 <- lists:seq(X, X + Size - 1)]).

find_max_power_grid(GSN) ->
    PowerGrids = 
        [ {X,Y,power_level_3x3(X,Y,GSN)} || 
            Y <- lists:seq(1, 298),
            X <- lists:seq(1, 298) ],
    lists:foldl(fun({X,Y,PL}, {_,_,MaxPL}) when PL > MaxPL ->
                        {X,Y,PL};
                   (_, Max) ->
                        Max
                end, {undef, undef, 0}, PowerGrids).

find_max_power_grid_anysize(GSN) ->
    %% An exhausting search for all power grids of all sizes is very
    %% time-consuming (there are ~9 million of them). But examples
    %% given in the puzzle indicate that the power grids with maximum
    %% power have sizes around 10..20, so limit the search there and
    %% hope for the best. :)
    PowerGrids = 
        [ {X, Y, Size, power_level_nxn(X,Y,Size,GSN)} || 
            Size <- lists:seq(10, 20),
            Y <- lists:seq(1, 300 - Size + 1),
            X <- lists:seq(1, 300 - Size + 1) ],

    lists:foldl(fun({X,Y,S,PL}, {_,_,_,MaxPL}) when PL > MaxPL ->
                        {X,Y,S,PL};
                   (_, Max) ->
                        Max
                end, {undef, undef, undef, 0}, PowerGrids).

test1() ->
    4 = power_level(3,5,8),
    -5 = power_level(122,79,57),
    0 = power_level(217,196,39),
    4 = power_level(101,153,71),
    29 = power_level_3x3(33,45,18),
    30 = power_level_3x3(21,61,42),
    {33,45,29} = find_max_power_grid(18).
