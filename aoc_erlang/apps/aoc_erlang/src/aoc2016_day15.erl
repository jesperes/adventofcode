-module(aoc2016_day15).
-include_lib("eunit/include/eunit.hrl").


start({TargetLevel, Data}) ->
    Discs = parse(Data),
    find_earliest_release(Discs, TargetLevel).

simulate(_Discs, Level, _DT, TargetLevel) when Level == TargetLevel ->
    ok;
simulate(Discs, Level, DT, TargetLevel) ->
    case maps:get(Level, Discs, falling) of
	falling -> simulate(Discs, Level + 1, DT, TargetLevel);
	{NumPos, InitPos} ->
	    if ((InitPos + DT + Level) rem NumPos) == 0 ->
		    simulate(Discs, Level + 1, DT, TargetLevel);
	       true -> lost
	    end
    end.

find_earliest_release(Discs, TL) ->
    find_earliest_release(Discs, 0, TL).

find_earliest_release(Discs, T, TL) ->
    case simulate(Discs, 0, T, TL) of
	ok   -> T;
	lost -> find_earliest_release(Discs, T + 1, TL)
    end.


%%% Data

testdata() ->
    {3, <<"Disc #1 has 5 positions; at time=0, it is at position 4.\n",
	  "Disc #2 has 2 positions; at time=0, it is at position 1.\n">>}.

realdata() ->
    {7, <<"Disc #1 has 17 positions; at time=0, it is at position 15.\n",
	  "Disc #2 has 3 positions; at time=0, it is at position 2.\n",
	  "Disc #3 has 19 positions; at time=0, it is at position 4.\n",
	  "Disc #4 has 13 positions; at time=0, it is at position 2.\n",
	  "Disc #5 has 7 positions; at time=0, it is at position 2.\n",
	  "Disc #6 has 5 positions; at time=0, it is at position 0.\n">>}.

realdata_part2() ->
    {8, <<"Disc #1 has 17 positions; at time=0, it is at position 15.\n",
	  "Disc #2 has 3 positions; at time=0, it is at position 2.\n",
	  "Disc #3 has 19 positions; at time=0, it is at position 4.\n",
	  "Disc #4 has 13 positions; at time=0, it is at position 2.\n",
	  "Disc #5 has 7 positions; at time=0, it is at position 2.\n",
	  "Disc #6 has 5 positions; at time=0, it is at position 0.\n",
	  "Disc #7 has 11 positions; at time=0, it is at position 0.\n">>}.

%%% Parser

ltoi(N) -> list_to_integer(N).

parse(Binary) ->
    Lines = string:tokens(binary_to_list(Binary), "\n\r"),
    maps:from_list(lists:map(fun parse_line/1, Lines)).

parse_line(Line) ->
    ["Disc", Disc, "has", N, "positions", "at", "time", _Time, "it",
     "is", "at", "position", Pos] = string:tokens(Line, " ;,.#="),
    {ltoi(Disc), {ltoi(N), ltoi(Pos)}}.

%%% Tests

main_test_() ->
  [ {"Test input",
     ?_assertEqual(5, start(testdata()))}
  , {"Part 1",
     ?_assertEqual(400589, start(realdata()))}
  , {"Part 2",
     ?_assertEqual(3045959, start(realdata_part2()))}
  ].
