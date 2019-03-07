-module(puzzle19).
-export([start/0]).

-include("input.hrl").
-include_lib("eunit/include/eunit.hrl").

start() ->
    Part1Sol = length(get_all_replacements(?INPUT, ?INPUT_RULES)),
    {Part2Sol, _} = part2(?INPUT, ?INPUT_RULES),
    {Part1Sol, Part2Sol}.

get_all_replacements(Input, Rules) ->
    Repls = [apply_rule(Rule, Input) || Rule <- Rules],
    sets:to_list(sets:from_list(lists:flatten(Repls))).

apply_rule({F, T}, Input) ->
    [binary:replace(Input, F, T, [{scope, Pos}]) || Pos <- binary:matches(Input, F)].

get_all_reductions(Input, Rules) ->
    Repls = [apply_reduction(Rule, Input) || Rule <- Rules],
    sets:to_list(sets:from_list(lists:flatten(Repls))).
    
apply_reduction({F, T}, Input) ->
    [binary:replace(Input, T, F, [{scope, Pos}]) || Pos <- binary:matches(Input, T)].

part1(Input, Rules) ->
    length(get_all_replacements(Input, Rules)).

part2(Target, Rules) ->
    %% Search backwards, starting with the string we want to produce.
    Start = Target,
    End = <<"e">>,
    {ok, Path} = 
	astar:a_star(Start,
		     fun({neighbors, Current}) ->
			     get_all_reductions(Current, Rules);
			({dist, _Neighbor, _Current}) ->
			     1;
			({cost, Neighbor}) ->
			     byte_size(Neighbor);
                        ({is_goal, Current}) ->
                             Current =:= End
		     end),
    
    %% The number of reductions needed is the length of the path - 1
    %% since the path includes the start node.
    {length(Path) - 1, Path}.

part2_test() ->    
    Rules = [
	     {<<"e">>, <<"H">>},
	     {<<"e">>, <<"O">>},
	     {<<"H">>, <<"HO">>},
	     {<<"H">>, <<"OH">>},
	     {<<"O">>, <<"HH">>}
	    ],
    
    ?assertMatch({3, _}, part2(<<"HOH">>, Rules)),
    ?assertMatch({6, _}, part2(<<"HOHOHO">>, Rules)).

get_all_reductions_test() ->
    Rules = [{<<"A">>, <<"XX">>},
	     {<<"B">>, <<"YY">>},
	     {<<"C">>, <<"ZZ">>}],
    Input = <<"XXYYZZ">>,
    get_all_reductions(Input, Rules).

part1_test() ->
    ?assertEqual(4, part1(?TEST_INPUT, ?TEST_INPUT_RULES)),
    ?assertEqual(576, part1(?INPUT, ?INPUT_RULES)).

