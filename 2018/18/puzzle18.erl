%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 18 Dec 2018 by Jesper Eskilson <>

-module(puzzle18).
-export([main/0]).
-compile([export_all]).

main() ->
    {{part1, false},
     {part2, false}}.

testdata() ->
    <<".#.#...|#.\n",
      ".....#|##|\n",
      ".|..|...#.\n",
      "..|#.....#\n",
      "#.#|||#|#|\n",
      "...#.||...\n",
      ".|....|...\n",
      "||...#|.#|\n",
      "|.||||..|.\n",
      "...#.|..|.\n">>.

testgrid() ->
    Lines = string:tokens(binary_to_list(testdata()), "\n"),
    parse_lines(Lines, 0, #{}).

inputgrid() ->
    {ok, Binary} = file:read_file("input.txt"),
    Lines = string:tokens(binary_to_list(Binary), "\n"),
    parse_lines(Lines, 0, #{}).

start() ->
    test(),
    realinput().

test() ->
    MaxX = 10,
    MaxY = 10,
    Grid = testgrid(),
    G0 = do_iterations(10, Grid, MaxX, MaxY),
    1147 = get_resource_value(G0).

realinput() ->
    MaxX = 50,
    MaxY = 50,
    Iters = 1,
    FinalGrid =
        lists:foldl(fun(N, Acc) ->
                            G0 = do_iterations(Iters, Acc, MaxX, MaxY),
                            RV = get_resource_value(G0),
                            io:format("iter(~p) -> ~p;~n", [N * Iters, RV]),
                            G0
                    end, inputgrid(), lists:seq(1, 10000)),
    
    %% io:format("~s~n", [grid_to_string(FinalGrid, MaxX, MaxY)]),
    get_resource_value(FinalGrid).


%% The total resource value is cyclical with a period of 28, and
%% starting at e.g. 968.

-define(CYCLE_LENGTH, 28).
-define(CYCLE_BASE, 968).
    
iter(968) -> 178398;
iter(969) -> 175593;
iter(970) -> 175593;
iter(971) -> 175840;
iter(972) -> 176782;
iter(973) -> 179034;
iter(974) -> 180800;
iter(975) -> 183218;
iter(976) -> 186472;
iter(977) -> 188679;
iter(978) -> 192560;
iter(979) -> 193050;
iter(980) -> 197568;
iter(981) -> 200260;
iter(982) -> 199841;
iter(983) -> 204516;
iter(984) -> 208547;
iter(985) -> 211584;
iter(986) -> 214635;
iter(987) -> 214635;
iter(988) -> 216389;
iter(989) -> 218160;
iter(990) -> 217540;
iter(991) -> 212576;
iter(992) -> 208362;
iter(993) -> 196305;
iter(994) -> 189610;
iter(995) -> 181044.

%% Only defined when N >= ?CYCLE_BASE.
cyclic_pattern(N) when N >= ?CYCLE_BASE ->
    iter(((N - ?CYCLE_BASE) rem ?CYCLE_LENGTH) + ?CYCLE_BASE).

get_resource_value(Grid) ->
    {SumWood, SumLumber} = 
        maps:fold(fun(_, $|, {Wood, Lumber}) ->
                          {Wood + 1, Lumber};
                     (_, $#, {Wood, Lumber}) ->
                          {Wood, Lumber + 1};
                     (_, _, Acc) ->
                          Acc
                  end, {0, 0}, Grid),
    SumWood * SumLumber.
                      

do_iterations(0, Grid, _, _) ->
    Grid;
do_iterations(N, Grid, MaxX, MaxY) ->
    G0 = iteration(Grid, MaxX, MaxY),
    do_iterations(N - 1, G0, MaxX, MaxY).

parse_lines([], _, Grid) ->
    Grid;
parse_lines([Line|Lines], Y, Grid) ->
    G0 = parse_line(Line, 0, Y, Grid),
    parse_lines(Lines, Y + 1, G0).
    
parse_line([], _, _, Grid) ->
    Grid;
parse_line([$#|Rest], X, Y, Grid) ->
    G0 = maps:put({X, Y}, $#, Grid),
    parse_line(Rest, X + 1, Y, G0);
parse_line([$||Rest], X, Y, Grid) ->
    G0 = maps:put({X, Y}, $|, Grid),
    parse_line(Rest, X + 1, Y, G0);
parse_line([$.|Rest], X, Y, Grid) ->
    G0 = maps:put({X, Y}, $., Grid),
    parse_line(Rest, X + 1, Y, G0);
parse_line([_|Rest], X, Y, Grid) ->
    parse_line(Rest, X + 1, Y, Grid).

grid_to_string(Grid, MaxX, MaxY) ->
    [[ maps:get({X, Y}, Grid, '?') || 
         X <- lists:seq(0, MaxX - 1) ] ++ "\n" || 
        Y <- lists:seq(0, MaxY - 1) ].

coords(MaxX, MaxY) ->
    [ {X, Y} || 
        X <- lists:seq(0, MaxX - 1),
        Y <- lists:seq(0, MaxY - 1) ].

iteration(OldGrid, MaxX, MaxY) -> 
    lists:foldl(fun(Pos, NewGrid) ->
                        next(Pos, OldGrid, NewGrid)
                end, #{}, coords(MaxX, MaxY)).

adjacent({X,Y}, Grid) ->
    lists:sort(
      [maps:get({Xa, Ya}, Grid) || 
          Xa <- [X - 1, X, X + 1],
          Ya <- [Y - 1, Y, Y + 1],
          {X, Y} /= {Xa, Ya},
          maps:is_key({Xa, Ya}, Grid)]).

count(C, Adj) ->
    length(
      lists:filter(fun(Elem) ->
                           Elem == C
                   end, Adj)).

next(Pos, OldGrid, NewGrid) ->
    Acre = maps:get(Pos, OldGrid, undefined),
    Adj = adjacent(Pos, OldGrid),

    NextAcre =
        case Acre of 
            %% An open acre will become filled with trees if three or
            %% more adjacent acres contained trees. Otherwise, nothing
            %% happens.
            $. ->
                Trees = count($|, Adj),
                if Trees >= 3 ->
                        $|; 
                   true ->
                        $.
                end;
            
            %% An acre filled with trees will become a lumberyard if
            %% three or more adjacent acres were
            %% lumberyards. Otherwise, nothing happens.
            $| ->
                Lumberyards = count($#, Adj),
                if Lumberyards >= 3 ->
                        $#;
                   true ->
                        $|
                end;

            %% An acre containing a lumberyard will remain a
            %% lumberyard if it was adjacent to at least one other
            %% lumberyard and at least one acre containing
            %% trees. Otherwise, it becomes open.
            $# ->
                Lumberyards = count($#, Adj),
                Trees = count($|, Adj),
                if (Lumberyards >= 1) and (Trees >= 1) ->
                        $#;
                   true ->
                        $.
                end
        end,

    %% io:format("~p (~p) -> ~p~n", [[Acre], Adj, [NextAcre]]),
    
    maps:put(Pos, NextAcre, NewGrid).
        
    
                        
            
                        
          
          
                    



