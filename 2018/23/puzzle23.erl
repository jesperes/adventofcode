%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 23 Dec 2018 by Jesper Eskilson <>

-module(puzzle23).
-export([main/0]).
-compile([export_all]).

main() ->
    {{part1, part1()},
     {part2, part2()}}.

part1() ->
    NanoBots = input("input.txt"),
    Strongest = lists:max(NanoBots),
    InRange = inrange_of(Strongest, NanoBots),
    length(InRange).

part2() ->
    NanoBots = input("testinput2.txt"),
    test(NanoBots),
    io:format("Bots = ~p~n", [NanoBots]),
    Box = find_bounding_box(NanoBots),
    %% {{X, Y, Z}, {W, H, D}} = Box,
    %% {Box, volume(Box)},
    search(Box, NanoBots).

test(Bots) ->
    5 = num_intersects({{12, 12, 12}, {1, 1, 1}}, Bots).

search({Pos, {1, 1, 1}} = Box, Bots) ->
    [{Pos, num_intersects(Box, Bots)}];
search(Box, NanoBots) ->
    io:format("Searching box ~p (in range of ~p bots)~n", [Box, num_intersects(Box, NanoBots)]),

    {B1, B2} = split_box(Box),

    Bots1 = num_intersects(B1, NanoBots),
    Bots2 = num_intersects(B2, NanoBots),

    %% io:format("B1 = ~p (in range of ~p bots)~n", [B1, Bots1]),
    %% io:format("B2 = ~p (in range of ~p bots)~n", [B2, Bots2]),
    
    if Bots1 > Bots2 ->
	    search(B1, NanoBots);
       Bots2 > Bots1 ->
	    search(B2, NanoBots);
       true ->
	    search(B1, NanoBots) ++
		search(B2, NanoBots)
    end.    
    
    
num_intersects(Box, NanoBots) ->
    lists:foldl(fun(Bot, N) ->
			Intersects = intersects(Box, Bot),
			if Intersects -> 1 + N;
			   true -> 0
			end
		end, 0, NanoBots).

manhattan_comp(MinX, W, X) ->
    MaxX = MinX + W - 1,
    if X > MaxX -> 
	    X - MaxX;
       (X =< MaxX) and (X >= MinX) -> 
	    0;
       X < MinX -> 
	    MinX - X
    end.

intersects(Box, Bot) ->
    {{Xb, Yb, Zb}, {W, H, D}} = Box,
    {R, X, Y, Z} = Bot,
    
    Dist = 
	manhattan_comp(Xb, W, X) + 
	manhattan_comp(Yb, H, Y) + 
	manhattan_comp(Zb, D, Z),
    
    Dist =< R.


%% Split the box in two along the longest axis.
split_box({{Xb, Yb, Zb}, {W, H, D}}) ->
    if (W >= H) and (W >= D) ->
	    %% W is largest
	    Half = max(W div 2, 1),
	    B1 = {{Xb, Yb, Zb}, {Half, H, D}},
	    B2 = {{Xb + Half, Yb, Zb}, {Half, H, D}},
	    {B1, B2};
       H >= D ->
	    %% H is largest
	    Half = max(H div 2, 1),
	    B1 = {{Xb, Yb, Zb}, {W, Half, D}},
	    B2 = {{Xb, Yb + Half, Zb}, {W, Half, D}},
	    {B1, B2};
       true ->
	    %% D is largest
	    Half = max(D div 2, 1),
	    B1 = {{Xb, Yb, Zb}, {W, H, Half}},
	    B2 = {{Xb, Yb, Zb + Half}, {W, H, Half}},
	    {B1, B2}
    end.
	    
%% Return a list of all the Nth elements of the tuples in T.
tuple_nth(N, TL) ->
    lists:map(fun(T) -> erlang:element(N, T) end, TL).

%% Find the bounding box of the given set of nanobots.
find_bounding_box(NanoBots) ->
    {{MinX, MaxX}, {MinY, MaxY}, {MinZ, MaxZ}} = 
	list_to_tuple(
	  lists:map(
	    fun(N) ->
		    L = tuple_nth(N, NanoBots),
		    {lists:min(L), lists:max(L)}
	    end, [2, 3, 4])),
    {{MinX, MinY, MinZ}, 
     {MaxX - MinX,
      MaxY - MinY,
      MaxZ - MinZ}}.

%% in({Min, Max}, X) ->
%%     (X >= Min) and (X =< Max).

%% inside_box({XRange, YRange, ZRange}, {_, X, Y, Z}) ->
%%     in(XRange, X) and
%% 	in(YRange, Y) and
%% 	in(ZRange, Z).

%% nanobots_in_box(Box, NanoBots) ->			  
%%     Inside = 
%% 	lists:filter(fun(Bot) ->
%% 			     inside_box(Box, Bot)
%% 		     end, NanoBots),
%%     length(Inside).

manhattan_dist({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    abs(X1 - X2) + abs(Y1 - Y2) + abs(Z1 - Z2).
    
%% Is Bot2 inrange of Bot1?
inrange_bot(Bot1, Bot2) ->
    {R1, X1, Y1, Z1} = Bot1,
    {_, X2, Y2, Z2} = Bot2,
    manhattan_dist({X1, Y1, Z1}, 
                   {X2, Y2, Z2}) =< R1.

%% Return the list of nanobots which are in range of the given bot
inrange_of(Bot, NanoBots) ->
    lists:filter(fun(X) ->
                         inrange_bot(Bot, X)
                 end, NanoBots).

%% Return the bots which are in range of the given point
bots_inrange_of_point({X, Y, Z}, NanoBots) ->
    lists:filter(fun({R, Xb, Yb, Zb}) ->
			 manhattan_dist({X, Y, Z}, {Xb, Yb, Zb}) =< R
                 end, NanoBots).
%%% Parser

input(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    Lines = string:tokens(binary_to_list(Binary), "\n\r"),
    lists:map(fun(Line) ->
                      ["pos", X, Y, Z, "r", R] = string:tokens(Line, "=<,> "),
                      {list_to_integer(R),
		       list_to_integer(X),
                       list_to_integer(Y),
                       list_to_integer(Z)}
              end, Lines).

