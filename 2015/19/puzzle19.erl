%%% @author  <jespe@LAPTOP-P6HKA27J>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 27 Dec 2018 by  <jespe@LAPTOP-P6HKA27J>

-module(puzzle19).
-compile([export_all]).

-include("input.hrl").
-include_lib("eunit/include/eunit.hrl").

part1_test() ->
    ?assertEqual(4, start(?TEST_INPUT, ?TEST_INPUT_RULES)).

part2_test() ->
    ?assertEqual(576, start(?INPUT, ?INPUT_RULES)).

start(Input, Rules) ->
    length(get_all_replacements(Input, Rules)).

get_all_replacements(Input, Rules) ->
    Repls = [apply_rule(Rule, Input) || Rule <- Rules],
    sets:to_list(sets:from_list(lists:flatten(Repls))).

apply_rule({F, T}, Input) ->
    [binary:replace(Input, F, T, [{scope, Pos}]) || Pos <- binary:matches(Input, F)].

start2() ->
    %% Rules = ?TEST_INPUT_RULES_PART2,
    %% Input = ?TEST_PART2,
    Rules = ?INPUT_RULES,
    Start = ?INPUT,
    End = <<"e">>,
    dfs(Start, End, Rules).

%% Graph search algorithm.

%% dfs(Start, End, Rules) ->
%%     dfs(Start, End, Rules, sets:new()).

%% dfs(Start, End, Rules, Discovered) ->
    
%%     %% Add start node to set of discovered nodes
%%     D0 = sets:add_element(Start, Discovered),
    
%%     %% Compute all strings we can get by applying the rules
%%     %% to our start string.
%%     AllReplacements = 
%% 	get_all_replacements(Start, Rules),
    
%%     %% Remove all strings we have already seen
%%     UnvisitedReplacements =
%% 	lists:filter(fun(A) ->
%% 			     not sets:is_element(A, D0)
%% 		     end, AllReplacements),

%%     StartLen = byte_size(Start),

%%     %% Sort them to consider the most-reducing replacement 
%%     %% first.
%%     NodesToVisit = 
%% 	lists:reverse(
%% 	  lists:sort(fun(A, B) ->
%% 			     DiffA = StartLen - A,
%% 			     DiffB = StartLen - B,
%% 			     DiffA =< DiffB
%% 		     end, UnvisitedReplacements)),
    
%%     [dfs(Node, End, Rules, D0) ||
%% 	Node <- NodesToVisit].
    

dijkstra(Start, End, Rules) ->
    Frontier = [Start],
    Explored = sets:new(),
    dijkstra(End, Rules, Frontier, Explored).

dijkstra(End, Rules, [], Explored) ->
    fail;
dijkstra(End, Rules, [End|Frontier], Explored) ->
    {found, End};
dijkstra(End, Rules, [Next|Frontier], Explored) ->
    Rs = get_all_replacements(Next, Rules),
    

    
    

    

    

  
