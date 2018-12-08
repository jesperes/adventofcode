-module(puzzle8).
-export([start1/0, start2/0]).

start1() ->
    IntList = tokenize_input(testdata()),
    {Root, [], _} = parse_node(IntList, 0),
    {Root, metadata_sum(Root)}.

testdata() -> 
    "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2".

realdata() ->
    {ok, Binary} = file:read_file("input.txt"),
    string:trim(binary_to_list(Binary)).

tokenize_input(Str) ->
    lists:map(fun list_to_integer/1, string:tokens(Str, " ")).

%% Parse a single node. Returns a tuple {Node, Rest, NextId}.
parse_node([NumChildNodes, NumMetadataEntries|Rest], Id) ->
    {ChildNodes, Rem, NextId} = parse_child_nodes(NumChildNodes, [], Rest, Id + 1),
    {Metadata, Rem1} = lists:split(NumMetadataEntries, Rem),
    {{list_to_atom([$A + Id]), ChildNodes, Metadata}, Rem1, NextId}.

%% Parse N nodes from List
parse_child_nodes(0, Acc, List, Id) ->
    {lists:reverse(Acc), List, Id};
parse_child_nodes(N, Acc, List, Id) ->
    {ChildNode, Rest, NextId} = parse_node(List, Id),
    parse_child_nodes(N - 1, [ChildNode|Acc], Rest, NextId).

sum(List) ->
    lists:foldl(fun(X,Acc) -> X + Acc end, 0, List).

metadata_sum({_Id, ChildNodes, Metadata}) ->
    sum(Metadata) + 
	lists:foldl(fun(Node, Acc) ->
			    Acc + metadata_sum(Node)
		    end, 0, ChildNodes).

%% ------------------------------------------------------------
%% Part 2
%% ------------------------------------------------------------

start2() ->
    IntList = tokenize_input(realdata()),
    {Root, [], _} = parse_node(IntList, 0),
    metadata_sum2(Root).

metadata_sum2({_Id, [], Metadata}) ->
    sum(Metadata);
metadata_sum2({Id, ChildNodes, Metadata}) ->
    sum_child_nodes(Id, Metadata, ChildNodes).

sum_child_nodes(_, [], _ChildNodes) ->
    0;
sum_child_nodes(Id, [M|Metadata], ChildNodes) ->
    child_index_sum(Id, M, ChildNodes) + 
	sum_child_nodes(Id, Metadata, ChildNodes).

child_index_sum(_Id, M, ChildNodes) ->
    try metadata_sum2(lists:nth(M, ChildNodes))
    catch _:_ -> 0 %% invalid metadata entry
    end.


