%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 23 Dec 2018 by Jesper Eskilson <>

-module(puzzle23b).
-export([main/0]).

main() ->
    part2().

%%
%% Monte-Carlo solution to part2 where we generate random points and
%% see which one is closest, then refocus around that point and throw
%% more points there. Will often get the right result, but not always.
%%

part2() ->
    NanoBots = input("input.txt"),
    RandomPoints = 10000,
    {Point, Radius} = find_center_point(NanoBots),
    
    {BestPoint, _} = 
	lists:foldl(fun(_, {_, 0} = Acc) ->
			    Acc;
		       (_, {P, R}) ->
			    NewP = get_random_best_point(P, R, NanoBots, RandomPoints),
			    {NewP, R div 2}
		    end, {Point, Radius}, lists:seq(1, 100)),
    
    io:format("Best point ~p in range of ~p bots~n", 
	      [BestPoint, num_bots_inrange_of_point(BestPoint, NanoBots)]),
    
    manhattan_dist(BestPoint, {0, 0, 0}).

get_random_best_point(Point, Radius, NanoBots, NumPoints) ->
    io:format("center point ~p, radius = ~p~n", [Point, Radius]),

    {_, Best} = 
	lists:foldl(fun(_, {N, _} = Acc) ->
			    RP = random_point(Point, Radius),
			    NB = num_bots_inrange_of_point(RP, NanoBots),
			    if NB > N ->
				    {NB, RP};
			       true ->
				    Acc
			    end
		    end, {0, undef}, lists:seq(1, NumPoints)),
    Best.

manhattan_dist({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    abs(X1 - X2) + abs(Y1 - Y2) + abs(Z1 - Z2).
    
%% Return the bots which are in range of the given point
bots_inrange_of_point({X, Y, Z}, NanoBots) ->
    lists:filter(fun({R, Xb, Yb, Zb}) ->
			 manhattan_dist({X, Y, Z}, {Xb, Yb, Zb}) =< R
                 end, NanoBots).

num_bots_inrange_of_point({X, Y, Z}, NanoBots) ->
    lists:foldl(fun({R, Xb, Yb, Zb}, N) ->
			InRange = manhattan_dist({X, Y, Z}, {Xb, Yb, Zb}) =< R,
			if InRange -> 1 + N;
			   true -> N
			end
		end, 0, NanoBots).

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

%% Return a list of all the Nth elements of the tuples in T.
tuple_nth(N, TL) ->
    lists:map(fun(T) -> erlang:element(N, T) end, TL).

%% Find the bounding box of the given set of nanobots.
find_bounding_box(NanoBots) ->
    list_to_tuple(
      lists:map(
	fun(N) ->
		L = tuple_nth(N, NanoBots),
		{lists:min(L), lists:max(L)}
	end, [2, 3, 4])).

find_center_point(NanoBots) ->
    {{MinX, MaxX}, {MinY, MaxY}, {MinZ, MaxZ}} = find_bounding_box(NanoBots),
    XRange = MaxX - MinX,
    YRange = MaxY - MinY,
    ZRange = MaxZ - MinZ,
    Point = {MinX + XRange div 2,
	     MinY + YRange div 2,
	     MinZ + ZRange div 2},
    Radius = lists:max([XRange, YRange, ZRange]),
    {Point, Radius}.

rand(R) ->
    floor((rand:uniform() - 0.5) * 2 * R).

random_point({X, Y, Z}, R) ->
    {X + rand(R),
     Y + rand(R),
     Z + rand(R)}.

	

