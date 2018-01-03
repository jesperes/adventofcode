-module(day10).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_INPUT,
       ["value 5 goes to bot 2",
	"bot 2 gives low to bot 1 and high to bot 0",
	"value 3 goes to bot 1",
	"bot 1 gives low to output 1 and high to bot 0",
	"bot 0 gives low to output 2 and high to output 0",
	"value 2 goes to bot 2"]).

parse_line(Line) ->
    [First|Rest] = string:lexemes(Line, " "),
    case First of
	"value" ->
	    [Value, "goes", "to", "bot", Bot] = Rest,
	    {goes_to, 
	     list_to_integer(Value),
	     list_to_integer(Bot)};
	"bot" ->
	    [Bot, "gives", "low", "to", LowType, LowDest, "and", "high", "to", HighType, HighDest] =
		Rest,
	    {bot_to_bot, 
	     list_to_integer(Bot), 
	     list_to_atom(LowType),
	     list_to_integer(LowDest), 
	     list_to_atom(HighType),
	     list_to_integer(HighDest)}
    end.

parse_input([], Acc) ->
    Acc;
parse_input([Line|Rest], Acc) ->
    parse_input(Rest, [parse_line(Line)|Acc]).

parse_input_test() ->
    ?assertEqual([{goes_to, 1, 2},
		  {bot_to_bot, 2, bot, 1, bot, 0}
		 ], 
		 parse_input([
			      "bot 2 gives low to bot 1 and high to bot 0",
			      "value 1 goes to bot 2"
			     ], [])).


%%% Giv each bot their initial values
insert_initial_bot_values([], Map) ->
    Map;
insert_initial_bot_values([{goes_to, Value, Bot}|Rest], Map) ->
    insert_initial_bot_values(
      Rest, maps:put({bot, Bot}, [Value|maps:get({bot, Bot}, Map, [])], Map));
insert_initial_bot_values([_|Rest], Map) ->
    insert_initial_bot_values(Rest, Map).

insert_initial_bot_values_test() ->
    List = parse_input(?TEST_INPUT, []),
    Map = insert_initial_bot_values(List, maps:new()),
    ?assertEqual([3], maps:get({bot, 1}, Map)),
    ?assertEqual([5, 2], maps:get({bot, 2}, Map)).
    

get_low_and_high(X, Y) when X =< Y ->
    {X, Y};
get_low_and_high(X, Y) ->
    {Y, X}.


send_to(Value, Type, Dest, Map) ->
    Key = {Type, Dest},
    maps:put(Key, [Value|maps:get(Key, Map, [])], Map).

%%% Run a pass over all bot instructions
run_bot_pass([], Map, _) ->    
    Map;
run_bot_pass([Instr = {bot_to_bot, Bot, LowType, LowDest, HighType, HighDest}|Rest], Map, Target = {TargetX, TargetY}) -> 
    case maps:get({bot, Bot}, Map, []) of
	[] ->
	    run_bot_pass(Rest, Map, Target);
	[_] ->
	    run_bot_pass(Rest, Map, Target);
	[X, Y] ->
	    ValueTuple = {LowValue, HighValue} = get_low_and_high(X, Y),
	    TargetTuple = {LowTarget, HighTarget} = get_low_and_high(TargetX, TargetY),
	    
	    %% ?debugFmt("Value tuple is ~w, target tuple is ~w", [ValueTuple, TargetTuple]),

	    if ValueTuple == TargetTuple ->
		    %% When we have found our target values, return
		    %% them in a tuple.
		    {target, Bot};
	       true ->
		    %% ?debugFmt("Bot ~w has two values, moving ~w -> ~w ~w and ~w -> ~w ~w", 
		    %% 	      [Bot, 
		    %% 	       LowValue, LowType, LowDest,
		    %% 	       HighValue, HighType, HighDest]),
		    
		    Map1 = send_to(LowValue, LowType, LowDest, Map),
		    Map2 = send_to(HighValue, HighType, HighDest, Map1),
		    Map3 = maps:remove({bot, Bot}, Map2),
		    run_bot_pass(Rest, Map3, Target)
	    end
    end;
run_bot_pass([_|Rest], Map, Target) -> 
    run_bot_pass(Rest, Map, Target).

run_bot_pass(List, Map) ->
    run_bot_pass(List, Map, {-1,-1}).

run_bot_pass_test() ->
    List = parse_input(?TEST_INPUT, []),
    Map = insert_initial_bot_values(List, maps:new()),
    NewMap = run_bot_pass(List, Map),
    NewMap1 = run_bot_pass(List, NewMap),
    NewMap2 = run_bot_pass(List, NewMap1),
    ?assertEqual([5], maps:get({output, 0}, NewMap2)),
    ?assertEqual([2], maps:get({output, 1}, NewMap2)),
    ?assertEqual([3], maps:get({output, 2}, NewMap2)).

find_target_values(_, _, _, 0) ->
    throw(max_passes_exceeded);
find_target_values(List, InitialMap, Target, MaxPasses) ->    
    ?debugMsg("Running pass..."),
    case run_bot_pass(List, InitialMap, Target) of
	{target, Bot} ->
	    ?debugFmt("Bot holding target values: ~w", [Bot]),
	    Bot;
	Map ->
	    find_target_values(List, Map, Target, MaxPasses - 1)
    end.
    
find_target_values_test() ->
    List = parse_input(?TEST_INPUT, []),
    Map = insert_initial_bot_values(List, maps:new()),
    ?assertEqual(2, find_target_values(List, Map, {5, 2}, 5)).

real_input_test() ->
    List = parse_input(utils:read_file_lines("input10.txt"), []),
    Map = insert_initial_bot_values(List, maps:new()),
    ?assertEqual(161, find_target_values(List, Map, {17, 61}, 10)).
