-module(aoc2018_day23).

-include_lib("eunit/include/eunit.hrl").

-behavior(aoc_puzzle).

-export([parse/1, solve1/1, solve2/1, info/0]).

-include("aoc_puzzle.hrl").

-spec info() -> aoc_puzzle().
info() ->
    #aoc_puzzle{module = ?MODULE,
                year = 2018,
                day = 23,
                name = "Experimental Emergency Teleportation",
                expected = {889, 160646364},
                has_input_file = true}.

-type input_type() :: [{integer(), integer(), integer(), integer()}].
-type result_type() :: integer().

-spec parse(Input :: binary()) -> input_type().
parse(Input) ->
    Lines = string:tokens(binary_to_list(Input), "\n\r"),
    lists:map(fun(Line) ->
                 ["pos", X, Y, Z, "r", R] = string:tokens(Line, "=<,> "),
                 {list_to_integer(R), list_to_integer(X), list_to_integer(Y), list_to_integer(Z)}
              end,
              Lines).

-spec solve1(Input :: input_type()) -> result_type().
solve1(NanoBots) ->
    Strongest = lists:max(NanoBots),
    InRange = inrange_of(Strongest, NanoBots),
    length(InRange).

-spec solve2(Input :: input_type()) -> result_type().
solve2(NanoBots) ->
    Box = find_bounding_box(NanoBots),
    {_, Sols} =
        search(_Boxes = gb_sets:singleton({num_intersects(Box, NanoBots), Box}),
               _Solutions = gb_sets:empty(),
               NanoBots),

    {_, Post} = gb_sets:largest(Sols),
    manhattan_dist(Post, {0, 0, 0}).

%% Search for the points with most number of nanobots in range.  Boxes
%% is an ordered set with those sub-boxes we have not yet examined.
%% Solutions is an ordered set with the current (best) solutions.
search(Boxes, Solutions, NanoBots) ->
    NumBoxes = gb_sets:size(Boxes),
    if NumBoxes == 0 ->
           %% No more boxes to examine.
           {Boxes, Solutions};
       true ->
           %% Take the best box seen so far (the one in range of most
           %% bots), and split in in half along the largest axis.
           {{_, Box}, Boxes1} = gb_sets:take_largest(Boxes),
           {B1, B2} = split_box(Box),

           %% Update box-queue and solutions
           {Boxes2, Solutions1} = handle_subbox(B1, Boxes1, Solutions, NanoBots),
           {Boxes3, Solutions2} = handle_subbox(B2, Boxes2, Solutions1, NanoBots),

           %% Recurse.
           search(Boxes3, Solutions2, NanoBots)
    end.

best_solution(Solutions) ->
    NumSols = gb_sets:size(Solutions),
    if NumSols == 0 ->
           0;
       true ->
           {Best, _} = gb_sets:largest(Solutions),
           Best
    end.

%% Return true if the given box contains exactly one point.
is_point({_, {1, 1, 1}}) ->
    true;
is_point(_) ->
    false.

box_to_point({Pos, _}) ->
    Pos.

handle_subbox(Box, Boxes, Solutions, NanoBots) ->
    NumBots = num_intersects(Box, NanoBots),
    BestSol = best_solution(Solutions),
    IsPoint = is_point(Box),

    if IsPoint and (NumBots >= BestSol) ->
           %% Single point, equal or better than the current best solution
           Sol = {NumBots, box_to_point(Box)},
           {Boxes,
            if NumBots > BestSol ->
                   %% If this point is best, discard all other solutions
                   gb_sets:singleton(Sol);
               true ->
                   gb_sets:add(Sol, Solutions)
            end};
       NumBots >= BestSol ->
           %% We haven't reached a single point, but this
           %% sub-box has the potential of containing better
           %% solutions, so add the box to the queue.
           {gb_sets:add({NumBots, Box}, Boxes), Solutions};
       true ->
           %% Not enough bots in range of this sub-box, just ignore it.
           {Boxes, Solutions}
    end.

%% Count the number of bots which are in range of any position within
%% the given box.
num_intersects(Box, Bots) ->
    lists:foldl(fun(Bot, N) ->
                   Intersects = intersects(Box, Bot),
                   if Intersects -> 1 + N;
                      true -> N
                   end
                end,
                0,
                Bots).

num_intersects_test() ->
    Bots =
        [{2, 10, 12, 12},
         {2, 12, 14, 12},
         {4, 16, 12, 12},
         {6, 14, 14, 14},
         {200, 50, 50, 50},
         {5, 10, 10, 10}],

    Box = {{12, 12, 12}, {1, 1, 1}},
    ?assert(intersects(Box, {2, 10, 12, 12})),
    ?assertEqual(5, num_intersects({{12, 12, 12}, {1, 1, 1}}, Bots)).

manhattan_comp(MinX, W, X) ->
    MaxX = MinX + W - 1,
    if X > MaxX ->
           X - MaxX;
       (X =< MaxX) and (X >= MinX) ->
           0;
       X < MinX ->
           MinX - X
    end.

manhattan_comp_test() ->
    ?assertEqual(0, manhattan_comp(0, 5, 2)),
    ?assertEqual(0, manhattan_comp(0, 5, 0)),
    ?assertEqual(1, manhattan_comp(0, 5, 5)),
    ?assertEqual(2, manhattan_comp(0, 5, -2)),
    ?assertEqual(2, manhattan_comp(0, 5, 6)).

%%
intersects(Box, Bot) ->
    {{Xb, Yb, Zb}, {W, H, D}} = Box,
    {R, X, Y, Z} = Bot,

    Dist = manhattan_comp(Xb, W, X) + manhattan_comp(Yb, H, Y) + manhattan_comp(Zb, D, Z),

    %% erlang:display({manhattan_dist, Box, Bot, Dist}),
    Dist =< R.

intersects_test() ->
    ?assertNot(intersects({{0, 0, 0}, {2, 2, 2}}, {0, 3, 3, 3})),
    ?assert(intersects({{0, 0, 0}, {2, 2, 2}}, {6, 3, 3, 3})),
    ?assert(intersects({{0, 0, 0}, {2, 2, 2}}, {9, -3, -3, -3})).

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
        list_to_tuple(lists:map(fun(N) ->
                                   L = tuple_nth(N, NanoBots),
                                   {lists:min(L), lists:max(L)}
                                end,
                                [2, 3, 4])),
    {{MinX, MinY, MinZ}, {MaxX - MinX, MaxY - MinY, MaxZ - MinZ}}.

%% in({Min, Max}, X) ->
%%     (X >= Min) and (X =< Max).

%% inside_box({XRange, YRange, ZRange}, {_, X, Y, Z}) ->
%%     in(XRange, X) and
%%         in(YRange, Y) and
%%         in(ZRange, Z).

%% nanobots_in_box(Box, NanoBots) ->
%%     Inside =
%%         lists:filter(fun(Bot) ->
%%                              inside_box(Box, Bot)
%%                      end, NanoBots),
%%     length(Inside).

manhattan_dist({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    abs(X1 - X2) + abs(Y1 - Y2) + abs(Z1 - Z2).

%% Is Bot2 inrange of Bot1?
inrange_bot(Bot1, Bot2) ->
    {R1, X1, Y1, Z1} = Bot1,
    {_, X2, Y2, Z2} = Bot2,
    manhattan_dist({X1, Y1, Z1}, {X2, Y2, Z2}) =< R1.

%% Return the list of nanobots which are in range of the given bot
inrange_of(Bot, NanoBots) ->
    lists:filter(fun(X) -> inrange_bot(Bot, X) end, NanoBots).

%% Return the bots which are in range of the given point
%% bots_inrange_of_point({X, Y, Z}, NanoBots) ->
%%   lists:filter(fun({R, Xb, Yb, Zb}) ->
%%                    manhattan_dist({X, Y, Z}, {Xb, Yb, Zb}) =< R
%%                end, NanoBots).

%%% Parser
