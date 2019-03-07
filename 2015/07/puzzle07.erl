-module(puzzle07).
-export([start/0]).
-compile([export_all]).

start() ->
    {ok, Bin} = file:read_file("input.txt"),
    Lines = string:tokens(binary_to_list(Bin), "\n\r"),
    List = lists:map(fun(Line) ->
                             lists:map(fun c/1, string:tokens(Line, " "))
                     end, Lines),
    
    propagate_signals(List, #{}).

c(S) -> 
    try list_to_integer(S)
    catch error:badarg -> list_to_atom(S)
    end.

propagate_signals(List, Map) ->
    case maps:is_key(a, Map) of
        true -> maps:get(a, Map);
        false ->
            propagate_signals(List, run_pass(List, Map))
    end.

    
run_pass([], Map) -> Map;
run_pass([{WireA, 'AND', WireB, _, WireC}|Rest], Map) ->
    
