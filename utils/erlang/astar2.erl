%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2019, Jesper Eskilson
%%% @doc
%%%
%%% Attempt at a slightly simpler (less general) implementation of the
%%% A* search algorithm.
%%%
%%% @end
%%% Created :  4 Jan 2019 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(astar2).
-export([astar/5]).
-include_lib("eunit/include/eunit.hrl").

astar(Start, End, CostFn, NbrFn, DistFn) ->
    O = sets:from_list([Start]), %% OpenSet
    C = sets:new(),              %% ClosedSet
    CF = #{},                    %% CameFrom
    Gs = #{Start => 0},          %% GScores
    Fs = gb_sets:from_list([{CostFn(Start), Start}]),
    astar(End, O, C, CF, Gs, Fs, CostFn, NbrFn, DistFn).

astar(End, O, C, CF, Gs, Fs, CostFn, NbrFn, DistFn) ->
    case sets:size(O) of
        0 -> search_exhausted;
        _ -> 
            {{_, Curr}, Fs0} = gb_sets:take_smallest(Fs),
            astar0(Curr, End, O, C, CF, Gs, Fs0, CostFn, NbrFn, DistFn)
    end.

astar0(End, End, _O, _C, CF, _Gs, _Fs, _CostFn, _NbrFn, _DistFn) ->
    lists:reverse(path_recon(End, CF));
astar0(Curr, End, O, C, CF, Gs, Fs, CostFn, NbrFn, DistFn) ->
    O0 = sets:del_element(Curr, O), %% remove curr from open
    C0 = sets:add_element(Curr, C), %% add curr to close
    Res = lists:foldl(fun astar_nbr/2, {Curr, O0, C0, CF, Gs, Fs, CostFn, DistFn}, NbrFn(Curr)),
    {_, O1, C1, CF0, Gs0, Fs0, _, _} = Res,
    astar(End, O1, C1, CF0, Gs0, Fs0, CostFn, NbrFn, DistFn).

%% Function to fold over the neighbors in the recursive step.
astar_nbr(Nbr, {Curr, O, C, CF, Gs, Fs, CostFn, DistFn} = AccIn) ->
    InClosed = sets:is_element(Nbr, C),
    if InClosed ->
            %% Neighbor is already evaluated.
            AccIn;
       true ->
            %% Add (possibly new) neighbor to open set
            O0 = sets:add_element(Nbr, O),
            NewGs = maps:get(Curr, Gs) + DistFn(Curr, Nbr),
            OldGs = maps:get(Nbr, Gs, inf),
            if NewGs < OldGs ->
                    %% Record new path if better
                    {CF0, Gs0, Fs0} = record_best_path(Nbr, Curr, CF, Gs, Fs, NewGs, CostFn),
                    {Curr, O0, C, CF0, Gs0, Fs0, CostFn, DistFn};
               true ->
                    AccIn
            end
    end.

%%% Helper functions
record_best_path(Nbr, Curr, CF, Gs, Fs, NewGs, CostFn) ->    
    {CF#{Nbr => Curr}, % update came-from map
     Gs#{Nbr => NewGs}, % update neighbor's gscore
     gb_sets:add({NewGs + CostFn(Nbr), Nbr}, Fs)}.

path_recon(Curr, CF) ->
    case maps:is_key(Curr, CF) of
        true ->  [Curr|path_recon(maps:get(Curr, CF), CF)];
        false -> [Curr]
    end.

%%% Tests
t1_test() ->
    Grid = <<"S....#....",
	     ".....#....",
	     "..##.#.#..",
	     "..##...#..",
	     ".......##.",
	     ".......#..",
	     "########..",
	     ".........#",
	     "....######",
	     ".........G">>,
    Size = 10,

    PosToCoord = fun({Start, _}, W) ->
                         {Start rem W, Start div W}
                 end,
    Obstacles = sets:from_list(
		  [PosToCoord(Pos, Size)
		   || Pos <- binary:matches(Grid, <<"#">>)]),

    Start = {0, 0},
    {Xg, Yg} = Goal = {9, 9},
    MinX = MinY = 0,
    MaxX = MaxY = 9,

    CostFn = fun({X, Y}) -> abs(X - Xg) + abs(Y - Yg) end,
    AdjFn = fun({X, Y}) ->
                    [{Xa, Ya} ||
                        Xa <- lists:seq(X - 1, X + 1),
                        Ya <- lists:seq(Y - 1, Y + 1),
                        Xa >= MinX, Xa =< MaxX,
                        Ya >= MinY, Ya =< MaxY,
                        {Xa, Ya} /= {X, Y}]
            end,
    
    NbrFn = fun(Curr) ->
                    lists:filter(fun(N) ->
                                         not sets:is_element(N, Obstacles)
                                 end, AdjFn(Curr))
            end,
    
    PosToStrFn = 
        fun({X, Y}, Path) ->
                case lists:member({X,Y}, Path) of
                    true -> $*;
                    false -> binary:at(Grid, Y * Size + X)
                end
        end,
    
    DistFn = fun({Xa, Ya}, {Xb, Yb}) -> abs(Xa - Xb) + abs(Ya - Yb) end,

    Red0 = proplists:get_value(reductions, process_info(self())),
    Path = astar(Start, Goal, CostFn, NbrFn, DistFn),
    Red1 = proplists:get_value(reductions, process_info(self())),
    erlang:display({reductions, Red1 - Red0}),

    X = [[PosToStrFn({X,Y}, Path) ||
     	     X <- lists:seq(0, Size - 1)] ++ "\n" ||
     	    Y <- lists:seq(0, Size - 1)],
    
    io:format("Path:~n~s~n", [X]),
    ?assertEqual(24, length(Path)).
