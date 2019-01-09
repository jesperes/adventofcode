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
    astar0(Start, CostFn, NbrFn, DistFn, 
          fun(E) -> E == End end).

astar0(Start, CostFn, NbrFn, DistFn, EndFn) ->
    OC = #{Start => open},
    CF = maps:new(),		    %% CameFrom
    Gs = #{Start => 0},
    Fs = gb_sets:singleton({CostFn(Start), Start}),
    astar0(OC, CF, Gs, Fs, CostFn, NbrFn, DistFn, EndFn).

astar0(OC, CF, Gs, Fs, CostFn, NbrFn, DistFn, EndFn) ->
    case gb_sets:size(Fs) of
        0 ->
            search_exhausted;
        _ ->
            {{_, Curr}, Fs0} = gb_sets:take_smallest(Fs),
            case EndFn(Curr) of
                true ->
                    path_recon(Curr, CF);
                false ->
                    OC0 = OC#{Curr => closed},
                    {_, OC1, CF0, Gs0, Fs1, _, _} =
                        lists:foldl(
                          fun astar_nbr/2, 
                          {Curr, OC0, CF, Gs, Fs0, CostFn, DistFn}, NbrFn(Curr)),
                    astar0(OC1, CF0, Gs0, Fs1, CostFn, NbrFn, DistFn, EndFn)
            end
    end.

%% Function to fold over the neighbors in the recursive step.
astar_nbr(Nbr, {Curr, OC, CF, Gs, Fs, CostFn, DistFn} = AccIn) ->
    case maps:get(Nbr, OC, open) of
	closed ->
            %% Neighbor is already evaluated.
            AccIn;
	open ->
            %% Add (possibly new) neighbor to open set
            OC0 = OC#{Nbr => open},

	    %% Check if this path to the neighbor is better. If so
	    %% store it and continue.
            NewGs = maps:get(Curr, Gs) + DistFn(Curr, Nbr),
            OldGs = maps:get(Nbr, Gs, inf),
            if NewGs < OldGs ->
                    %% Record new path if better
		    {Curr, OC0,   
		     maps:put(Nbr, Curr, CF),	% update came-from map
		     maps:put(Nbr, NewGs, Gs), % update neighbor's gscore
		     gb_sets:add({NewGs + CostFn(Nbr), Nbr}, Fs),
		     CostFn, DistFn};
	       true -> AccIn
            end
    end.

%%% Helper functions
path_recon(Curr, CF) ->
    case maps:is_key(Curr, CF) of
        true ->  [Curr|path_recon(maps:get(Curr, CF), CF)];
        false -> [Curr]
    end.

%%% Tests

ex_search(Grid, Size) ->
    PosToCoord = fun({Start, _}) ->
                         {Start rem Size, Start div Size}
                 end,

    Obstacles = sets:from_list(
		  [PosToCoord(Pos)
		   || Pos <- binary:matches(Grid, <<"#">>)]),
    
    GetPosOf = fun(Binary) ->
		       [Pos] = binary:matches(Grid, Binary),
		       PosToCoord(Pos)
	       end,

    {Xg, Yg} = Goal = GetPosOf(<<"G">>),
    Start = GetPosOf(<<"S">>),
    MinX = MinY = 0,
    MaxX = MaxY = Size - 1,

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
                    true -> $x;
                    false -> binary:at(Grid, Y * Size + X)
                end
        end,
    
    DistFn = fun({Xa, Ya}, {Xb, Yb}) -> abs(Xa - Xb) + abs(Ya - Yb) end,
    
    Repeat = 1000,
    {Time, _} = 
	timer:tc(fun() ->
			 lists:foreach(
			   fun(_N) ->
				   astar(Start, Goal, CostFn, NbrFn, DistFn)
			   end, lists:seq(1, Repeat))
		 end),

    ?debugFmt("Average time/iter: ~w us", [Time / Repeat]),
			 
    Path = astar(Start, Goal, CostFn, NbrFn, DistFn),
    case Path of
	search_exhausted ->
	    search_exhausted;
	_ ->
	    X = [[PosToStrFn({X,Y}, Path) ||
		     X <- lists:seq(0, Size - 1)] ++ "\n" ||
		    Y <- lists:seq(0, Size - 1)],    
	    
	    ?debugFmt("Path length: ~w", [length(Path)]),
	    ?debugFmt("Path:~n~s~n", [X]),
	    Path
    end.

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
    ?assertEqual(24, length(ex_search(Grid, 10))).
    
t2_test() ->
    Grid = <<".....#....",
	     ".....#.###",
	     "..##.#.#..",
	     "..##...#..",
	     "......###.",
	     "..#..S#...",
	     "..#####.#.",
	     "..##....#.",
	     "..#..#.##.",
	     ".....#.#G.">>,
    ?assertEqual(19, length(ex_search(Grid, 10))).
    
t3_test() ->
    Grid = <<"S.........",
	     "...######.",
	     "#.......#.",
	     "#...###.#.",
	     "#.#.#...#.",
	     "#.#.#.#.#.",
	     "###.#####.",
	     "..#.#.....",
	     "..###.####",
	     ".........G">>,
    ?assertEqual(23, length(ex_search(Grid, 10))).

t4_test() ->
    Grid = <<"S....",
	     ".....",
	     "..###",
	     "..#..",
	     "..#.G">>,
          
    ?assertEqual(search_exhausted, ex_search(Grid, 5)).
