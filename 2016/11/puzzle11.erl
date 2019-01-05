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
      %%,  {part2, number_of_moves(realdata(), part2)}
    }.

number_of_moves(InputData, Part) ->
    {Time, Result} = timer:tc(fun() ->
				      number_of_moves0(InputData, Part)
			      end),
    io:format("Time: ~w secs~n", [Time / 1000000.0]),
    Result.

number_of_moves0(InputData, Part) ->
    Floors = 
	case Part of
	    part1 ->
		parse(InputData);
	    part2 ->
		%% For part 2, we discover that there are additional
		%% RTG/microchips (Elerium & Dilithium).
		F = parse(InputData),
		ItemsFloor1 = maps:get(1, F),
		maps:put(1, ItemsFloor1 ++ [eg, em, dg, dm], F)
	end,

    Path = 
	astar2:astar(Floors, endstate(Floors), 
		     fun cost/1,
		     fun moves/1,
		     fun distance/2),

    %% -1 is here because the path includes the start state, and we
    %% want the number of moves, not the number of states.
    length(Path) - 1.
    

%%% Searching

distance(_, _) -> 1.

cost(Node) ->
    %% Cost-heuristic; lower value means closer to goal.  We use the
    %% sum of the distances for each item from the fourth floor.
    maps:fold(fun(Floor, Items, Acc) ->
		      Acc + (length(Items) * (4 - Floor))
	      end, 0, Node).

endstate(Floors) ->
    %% All items on the fourth floor, and the other floors empty.
    #{4 => lists:sort(maps:fold(fun(_K, V, Acc) ->
					V ++ Acc
				end, [], Floors)),
      3 => [], 
      2 => [], 
      1 => []}.

%% Return the possible moves represented as new states.
moves(Floors) ->
    %% We can only move things from the floor the elevator is on.  We
    %% can move the elevator plus 0-2 items.
    {ElevatorFloor, ItemsOnElevatorFloor} =
	maps:fold(fun(E, Items, AccIn) when is_number(E) ->
			  case lists:member(?ELEVATOR, Items) of
			      true -> {E, Items};
			      false -> AccIn
			  end
		  end, {undef, undef}, Floors),
    Xs = lists:delete(?ELEVATOR, ItemsOnElevatorFloor),
    
    %% Create a list of combinations of items to move, e.g.  [e], [sg,
    %% e], [pg, e], [pg, sg, e], etc.  All combinations must include
    %% the elevator.
    ItemCombos = lists:map(fun(Elem) -> 
				   lists:sort(Elem ++ [?ELEVATOR]) 
			   end,
			   cnr(1, Xs) ++ cnr(2, Xs)),
    
    %% The possible floors we can move to.
    FloorNums = 
	case ElevatorFloor of
	    1 -> [2];
	    2 -> [1,3];
	    3 -> [2,4];
	    4 -> [3]
	end,
    
    %% Create a list of {Floor, Items} tuples describing all possible
    %% combinations of moves. Many of these will be invalid.
    Moves = lists:map(
	      fun(F) ->
		      lists:map(
			fun(Combo) -> {F, Combo} end, ItemCombos)
	      end, FloorNums),
    
    %% Filter out all moves which will destroy microchips.
    ValidMoves = 
	lists:filter(
	  fun({DestFloor, ItemsToMove}) ->
		  I0 = maps:get(ElevatorFloor, Floors) -- ItemsToMove,
		  I1 = maps:get(DestFloor, Floors) ++ ItemsToMove,
		  check_items(I0) and check_items(I1)
	  end, lists:flatten(Moves)),
    
    States = 
	lists:map(fun(Move) ->
			  apply_move(Move, ElevatorFloor, Floors)
		  end, ValidMoves),
    
    %%io:format("Computed next moves for:~n", []),    print(Floors),
    %%io:get_line("Continue?"),
    States.

	
%% Check that all microchips are either shielded by their own RTG or
%% that it is not exposed to another RTG on the same floor.
check_items(Items) ->
    %% {Ms, Gs} = split_items(lists:delete(?ELEVATOR, Items)),
    lists:all(fun(Item) ->
		      case is_microchip(Item) of
			  true ->
			      M = Item,
			      IsShielded = lists:member(rtg(M), Items),			      
			      HasOtherRTGs = 
				  lists:any(fun(Item) ->
						    is_rtg(Item)
					    end, lists:delete(rtg(M), Items)),
			      %%?debugFmt("~p: IsShielded(~p) = ~w", [Items, M, IsShielded]),
			      %%?debugFmt("~p: HasOtherRTGs(~p) = ~w", [Items, M, HasOtherRTGs]),
			      IsShielded or (not HasOtherRTGs);
			  false ->
			      true
		      end
	      end, Items).

%% Return the RTG powering the given microchip.
rtg(sm) -> sg;
rtg(tm) -> tg;
rtg(pm) -> pg;
rtg(cm) -> cg;
rtg(rm) -> rg;
rtg(hm) -> hg;
rtg(lm) -> lg.

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
    
%% Split items into microchips and generators
split_items(Items) ->
    lists:foldl(fun(Item, {Ms, Gs}) ->
			case is_microchip(Item) of		
			    true -> {[Item|Ms], Gs};
			    false -> {Ms, [Item|Gs]}
			end
		end, {[], []}, Items).

%% Apply a move to a set of floors/items.
apply_move({DestFloor, Items}, OrigFloor, Floors) ->
    ItemsOnOrigFloor = maps:get(OrigFloor, Floors),
    ItemsOnDestFloor = maps:get(DestFloor, Floors),
    Floors#{OrigFloor => lists:sort(lists:subtract(ItemsOnOrigFloor, Items)),
	    DestFloor => lists:sort(ItemsOnDestFloor ++ Items)}.

%%% Parser

parse(Binary) ->
    Lines = string:tokens(binary_to_list(Binary), "\n\r"),
    Floors =
        lists:foldl(fun(Line, Floors) ->
			    {FloorNum, Items} = parse_line(Line),
			    Floors#{FloorNum => Items}
                    end, #{}, Lines),
    %% Put elevator on floor 1.
    maps:update_with(1, fun(V) -> [?ELEVATOR|V] end, Floors).

floornum("first") -> 1;
floornum("second") -> 2;
floornum("third") -> 3;
floornum("fourth") -> 4.

parse_line(Line) ->
    ["The", Floor, "floor", "contains"|Items] = string:tokens(Line, " .,"),
    {floornum(Floor), lists:sort(parse_items(Items, []))}.

parse_items([], List) -> List;
parse_items(["and"|Rest], List) -> parse_items(Rest, List);
parse_items(["a", [C|_], [T|_]|Rest], List) ->
    parse_items(Rest, [list_to_atom([C,T])|List]);
parse_items(["nothing", "relevant"], List) ->
    List.
 
%%% Printer

print(Floors) ->
    lists:map(fun({FloorNum, Floor}) ->
                      io:format("~w ~w~n", 
                                [FloorNum, Floor])
              end, lists:reverse(lists:sort(maps:to_list(Floors)))),
    ok.

%%% Helpers

%% Returns all possible combinations of a given length.
%% https://github.com/joergen7/lib_combin/blob/master/src/lib_combin.erl
cnr(N, SrcLst) when N >= 0 ->
  Cnr = fun
            Cnr(0, _, Acc)     -> [Acc];
            Cnr(_, [], _)      -> [];
            Cnr(M, [H|T], Acc) ->
                case T of
                    []    -> Cnr(M-1, [], [H|Acc]);
                    [_|_] -> Cnr(M-1, T, [H|Acc]) ++ Cnr(M, T, Acc)
                end
        end,
    
  Cnr(N, SrcLst, []).

%%% Tests

check_items_test_() ->
    [
     ?_assert(check_items([cm])),	  %% cm is unshielded, but no other rtg
     ?_assert(check_items([cm, cg])),	  %% cm is shielded by cg
     ?_assert(check_items([cm, cg, pg])), %% cm is shielded by cg
     ?_assertNot(check_items([cm, pg]))   %% cm is destroyed by pg
    ].

apply_move_test() ->
    ?assertEqual(
       #{1 => [d, e, f],
	 2 => [g, h, i, j, a, b, c]},
       apply_move({2, [a, b, c]}, 1, #{1 => [a, b, c, d, e, f],
				       2 => [g, h, i, j]})).
