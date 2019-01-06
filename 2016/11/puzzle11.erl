%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2019, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created :  4 Jan 2019 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(puzzle11).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

-define(ELEVATOR, 'E').
-define(BOTTOM_FLOOR, 0).
-define(SECOND_FLOOR, 1).
-define(THIRD_FLOOR, 2).
-define(TOP_FLOOR, 3).

%%%
%%% Rules: 
%%% 
%%% * Elevator can carry at most two items
%%% * Elevator must have at least one RTG or microchip in it to move.
%%% * Elevator always moves one floor at a time, and is exposed to
%%%   all items on the floor it is on.
%%% * Microchips are shielded from other RTGs when and only when they
%%%   are connected to their generator.
%%%

%%%
%%% Performance notes
%%%
%%% First solution solved part 1 in ~15 seconds, but part 2 adds two
%%% microchips and this causes part 2 to take much too long time.
%%%
%%% eprof says:
%%%
%%% ...
%%% puzzle11:rtg/1                             7159210     4.54   1778000  [      0.25]
%%% puzzle11:'-check_items/1-fun-0-'/1         7800823     4.71   1843000  [      0.24]
%%% puzzle11:is_microchip/1                    9415855     4.95   1936000  [      0.21]
%%% puzzle11:'-check_items/1-fun-1-'/2         9415855     5.38   2107000  [      0.22]
%%% lists:delete/2                            13118728     8.29   3245000  [      0.25]
%%% gb_sets:insert_1/3                         9570438     8.33   3262000  [      0.34]
%%% gb_sets:is_member_1/2                     30183837    17.97   7035000  [      0.23]
%%% ---------------------------------------  ---------  -------  --------  [----------]
%%% Total:                                   180182569  100.00%  39139000  [      0.22]
%%%
%%% is_member is taking time presumably because each node is a map of
%%% {int, list} pairs, which needs to be pairwise compared all the
%%% time. Perhaps one optimization is to find a more efficient
%%% representation of node states.
%%%


%%% Alternative representation: store all the elements as a list of
%%% integers, where each position in the list corresponds to to an
%%% element (e.g. elevator, microchip, generator), and the value at
%%% that location the floor the element is on. Since we only ever have
%%% 4 floors, 2 bits is enough.

%%% Time part 1: 8.5 secs

testdata() ->
    <<"The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.\n",
      "The second floor contains a hydrogen generator.\n",
      "The third floor contains a lithium generator.\n",
      "The fourth floor contains nothing relevant.\n">>.

realdata() ->
    {ok, Binary} = file:read_file("input.txt"),
    Binary.

start() ->
    {
      {part1, number_of_moves(realdata(), part1)}
      %% {part2, number_of_moves(realdata(), part2)}
    }.

number_of_moves(Data, Part) ->
    {Time, Result} = timer:tc(fun() ->
				      number_of_moves0(Data, Part)
			      end),
    io:format("Time: ~w secs~n", [Time / 1000000.0]),
    Result.

number_of_moves0(Data, Part) ->
    {Names, Start} = parse(Data, Part),
    
    io:format("Initial state:~n", []),
    print(Names, Start),
    io:format("Searching...~n", []),
    case astar2:astar(Start, endstate(Names), 
		      fun cost/1,
		      fun(Bits) ->
			      moves(Names, Bits)
		      end,
		      fun distance/2) of
	search_exhausted ->
	    no_solution;
	Path ->
	    %% -1 is here because the path includes the start state, and we
	    %% want the number of moves, not the number of states.
	    length(Path) - 1
    end.

%%% Searching

distance(_, _) -> 1.

%% Cost-heuristic; lower value means closer to goal.  We use the
%% sum of the distances for each item from the fourth floor.
cost(Node) ->
    cost(Node, 0).
cost(<<>>, Sum) -> Sum;
cost(<<Floor:2, Rest/bitstring>>, Sum) ->
    cost(Rest, Sum + (?TOP_FLOOR - Floor)).

endstate(Names) ->
    TopFloor = ?TOP_FLOOR,
    << <<TopFloor:2>> || _ <- Names >>.

%% The elevator floor is always the two first bits
moves([?ELEVATOR|Names] = AllNames, <<EF:2, Rest/bitstring>> = Bits) ->
    %% io:format("~n==========================================~n", []),
    %% io:format("Getting adjacent moves for: ~w~n", [Bits]),
    %% print(AllNames, Bits),
    
    %% Items on the elevator floor
    EItems = items_on_floor(Names, EF, Rest),

    Combos = combinations(1, EItems) ++ combinations(2, EItems),

    %% io:format("Number of combinations: ~w~n", [length(Combos)]),

    Moves = 
	lists:foldl(
	  fun(FN, Acc) ->
		  %% Items on the destination floor
		  FItems = items_on_floor(Names, FN, Rest),
		  
		  lists:foldl(
		    fun(Combo, CAcc) ->
			    %% io:format("Checking move ~w to floor ~w~n", [Combo, FN]),
			    case {check_items(FItems, Combo, []), 
				  check_items(EItems, [], Combo)} of
				{true, true} ->
				    Move = apply_move(FN, [?ELEVATOR|Combo], AllNames, Bits),
				    %% io:format("Move ~w to floor ~w is OK: ~w~n", [Combo, FN+1, Move]),
				    [Move|CAcc];
				_ ->
				    %% io:format("Move ~w to floor ~w will DESTROY one or more chips.~n", [Combo, FN+1]),
				    CAcc
			    end
		    end, Acc, Combos)
	  end, [], adj_floors(EF)),
    
    %% io:format("Valid adjacent states: ~w~n", [[Moves]]),
    Moves.

%% Check that "Items" is compatible
check_items(Items, ItemsToAdd, ItemsToRemove) ->
    ItemsToCheck = (Items -- ItemsToRemove) ++ ItemsToAdd,
    %% ?debugFmt("check_items(~w, ~w, ~w) -> ~w", [Items, ItemsToAdd, ItemsToRemove, ItemsToCheck]),
    check_items(ItemsToCheck).

apply_move(_FN, _ItemsToMove, [], <<>>) -> <<>>;
apply_move(NewFN, ItemsToMove, [Item|Names], <<OldFN:2, Rest/bitstring>>) -> 
    Bits = apply_move(NewFN, ItemsToMove, Names, Rest),
    case lists:member(Item, ItemsToMove) of
	true ->
	    <<NewFN:2, Bits/bitstring>>;
	false ->
	    <<OldFN:2, Bits/bitstring>>
    end.

%% Check that all microchips are either shielded by their own RTG or
%% that it is not exposed to another RTG on the same floor.
check_items(Items) ->
    %% {Ms, Gs} = split_items(lists:delete(?ELEVATOR, Items)),
    Status = 
	lists:all(fun(Item) ->
			  case is_microchip(Item) of
			      true ->
				  M = Item,
				  RTG = rtg(M),
				  IsShielded = lists:member(RTG, Items),			      
				  HasOtherRTGs = 
				      lists:any(fun(X) ->
							is_rtg(X)
						end, lists:delete(RTG, Items)),
				  %%?debugFmt("~p: IsShielded(~p) = ~w", [Items, M, IsShielded]),
				  %%?debugFmt("~p: HasOtherRTGs(~p) = ~w", [Items, M, HasOtherRTGs]),
				  IsShielded or (not HasOtherRTGs);
			      false ->
				  true
			  end
		  end, Items),
    
    %% io:format("Check items ~w : ~w~n", [Items, Status]),
    Status.

%%% Parser

parse(Binary) ->
    parse(Binary, part1).
   
parse(Binary, Part) ->
    Lines = string:tokens(binary_to_list(Binary), "\n\r"),
    E = [{?ELEVATOR, ?BOTTOM_FLOOR}|lists:foldl(fun parse_line/2, [], Lines)],
    Elements = 
	lists:sort(
	  case Part of
	      part1 -> E;
	      part2 -> lists:sort(E ++ [{eg, 0}, {em, 0}, {dg, 0}, {dm, 0}])
	  end),
    
    Bits = floors_to_binary([X || {_, X} <- Elements]),
    Names = [X || {X, _} <- Elements],
    {Names, Bits}.

floornum("first") -> ?BOTTOM_FLOOR;
floornum("second") -> ?BOTTOM_FLOOR + 1;
floornum("third") -> ?BOTTOM_FLOOR + 2;
floornum("fourth") -> ?TOP_FLOOR.


parse_line(Line, List) ->
    ["The", Floor, "floor", "contains"|Items] = string:tokens(Line, " .,"),
    parse_items(Items, floornum(Floor), List).

parse_items([], _, List) -> 
    List;
parse_items(["and"|Rest], FN, List) -> 
    parse_items(Rest, FN, List);
parse_items(["a", [C|_], [T|_]|Rest], FN, List) ->
    parse_items(Rest, FN, [{list_to_atom([C,T]), FN}|List]);
parse_items(["nothing", "relevant"], _FN, List) ->
    List.

%%% Printer

print(Names, Bits) ->
    io:format("--~n~w, ~w~n", [Names, Bits]),
    lists:foreach(
      fun(FloorNum) ->
	      io:format("F~w ", [FloorNum + 1]),	      
	      print_floor(FloorNum, Names, Bits)
      end, lists:seq(3, 0, -1)).

print_floor(_, [], <<>>) ->
    io:format("~n", []);
print_floor(FN, [Item|Items], <<F:2, Rest/bitstring>>) when F == FN ->
    io:format("~-3s", [string:uppercase(atom_to_list(Item))]),
    print_floor(FN, Items, Rest);
print_floor(FN, [_|Items], <<_:2, Rest/bitstring>>) ->
    io:format(".  ", []),
    print_floor(FN, Items, Rest).

%%% Helpers

items_on_floor([], _, _) ->
    [];
items_on_floor([Item|Names], FN, <<Floor:2,Rest/bitstring>>) when FN == Floor ->
    [Item|items_on_floor(Names, FN, Rest)];
items_on_floor([_|Names], FN, <<_:2,Rest/bitstring>>) ->
    items_on_floor(Names, FN, Rest).

items_on_floor_test() ->
    {Names, Bits} = parse(testdata()),
    ['E'|_] = Names,
    <<EF:2, _Rest/bitstring>> = Bits,
    ?assertEqual([?ELEVATOR, hm, lm], items_on_floor(Names, EF, Bits)),
    ?assertEqual([hg], items_on_floor(Names, ?SECOND_FLOOR, Bits)),
    ?assertEqual([lg], items_on_floor(Names, ?THIRD_FLOOR, Bits)),
    ?assertEqual([], items_on_floor(Names, ?TOP_FLOOR, Bits)).

adj_floors(?BOTTOM_FLOOR) -> [?SECOND_FLOOR];
adj_floors(?SECOND_FLOOR) -> [?BOTTOM_FLOOR, ?THIRD_FLOOR];
adj_floors(?THIRD_FLOOR) -> [?SECOND_FLOOR, ?TOP_FLOOR];
adj_floors(?TOP_FLOOR) -> [?THIRD_FLOOR].


%% Return the RTG powering the given microchip.
rtg(sm) -> sg;
rtg(tm) -> tg;
rtg(pm) -> pg;
rtg(cm) -> cg;
rtg(rm) -> rg;
rtg(hm) -> hg;
rtg(lm) -> lg;
rtg(em) -> eg;
rtg(dm) -> dg.
	   
     
is_microchip(sm) -> true;
is_microchip(tm) -> true;
is_microchip(pm) -> true;
is_microchip(cm) -> true;
is_microchip(rm) -> true;
is_microchip(hm) -> true;
is_microchip(lm) -> true;
is_microchip(em) -> true;
is_microchip(dm) -> true;
is_microchip('E') -> false;
is_microchip(sg) -> false;
is_microchip(tg) -> false;
is_microchip(pg) -> false;
is_microchip(cg) -> false;
is_microchip(rg) -> false;
is_microchip(hg) -> false;
is_microchip(lg) -> false;
is_microchip(eg) -> false;
is_microchip(dg) -> false.

is_rtg(sg) -> true;
is_rtg(tg) -> true;
is_rtg(pg) -> true;
is_rtg(cg) -> true;
is_rtg(rg) -> true;
is_rtg(hg) -> true;
is_rtg(lg) -> true;
is_rtg(eg) -> true;
is_rtg(dg) -> true;
is_rtg(_) -> false.

%%% List is a sorted list of floors of the elements involved.
floors_to_binary(List) ->
    << <<X:2>> || X <- List >>.

binary_to_floors(Bin) ->
    [X || <<X:2>> <= Bin].

%% Returns all possible combinations of a given length.
%% https://github.com/joergen7/lib_combin/blob/master/src/lib_combin.erl
combinations(N, List) when N >= 0 ->
    combinations(N, List, []).

combinations(0, _, Acc) -> [Acc];
combinations(_, [], _) -> [];
combinations(M, [H|T], Acc) ->
    case T of
	[]    -> combinations(M-1, [], [H|Acc]);
	[_|_] -> combinations(M-1, T, [H|Acc]) ++ combinations(M, T, Acc)
    end.

combinations_test() ->
    ?assertEqual([[2,1],[3,1],[3,2]], combinations(2, [1, 2, 3])),
    ?assertEqual([[1],[2],[3]], combinations(1, [1, 2, 3])),
    ?assertEqual([[]], combinations(0, [1, 2, 3])).

%%% Tests

check_items_test_() ->
    [
     ?_assert(check_items([cm])),	  %% cm is unshielded, but no other rtg
     ?_assert(check_items([cm, cg])),	  %% cm is shielded by cg
     ?_assert(check_items([cm, cg, pg])), %% cm is shielded by cg
     ?_assertNot(check_items([cm, pg]))   %% cm is destroyed by pg
    ].

apply_move_test() ->
    Names =  [?ELEVATOR, a, b, c, d, e, f],
    Floors = [2,         1, 2, 2, 1, 3, 3],
    Moving = [?ELEVATOR,    b, c],
    ToFloor = 3,
    ExpRes = [3,         1, 3, 3, 1, 3, 3],

    Bits = floors_to_binary(Floors),
    Moved = apply_move(ToFloor, Moving, Names, Bits),
    ?debugFmt("~w -> (moving ~w to floor ~w) -> ~w",
	      [Floors, Moving, ToFloor, binary_to_floors(Moved)]),
    ?assertEqual(ExpRes, binary_to_floors(Moved)).

cost_test() ->
    {Names, Bits} = parse(testdata()),
    ?assertEqual(12, cost(Bits)),
    ?assertEqual(0, cost(endstate(Names))).

moves_test() ->
    {Names, Bits} = parse(testdata()),
    print(Names, Bits),
    [Move] = moves(Names, Bits),
    io:format("First move:~n", []),
    print(Names, Move).

ex1_test() ->
    Names = [?ELEVATOR,hg,hm,lg,lm],
    Bits = floors_to_binary([1, 1, 1, 2, 0]),
    print(Names, Bits),
    Move = apply_move(?THIRD_FLOOR, [?ELEVATOR, hm, hg], Names, Bits),
    print(Names, Move),
    ?assertEqual(floors_to_binary([2, 2, 2, 2, 0]), Move).

    
   

