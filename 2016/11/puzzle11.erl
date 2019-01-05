%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2019, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created :  4 Jan 2019 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(puzzle11).
-compile([export_all]).

testdata() ->
    <<"The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.\n",
      "The second floor contains a hydrogen generator.\n",
      "The third floor contains a lithium generator.\n",
      "The fourth floor contains nothing relevant.\n">>.

realdata() ->
    {ok, Binary} = file:read_file("input.txt"),
    Binary.

start() ->
    parse(testdata()).

%%% Parser

parse(Binary) ->
    Lines = string:tokens(binary_to_list(Binary), "\n"),
    {_, Floors} = 
        lists:foldl(fun(Line, {FloorNum, Floors}) ->
                            {FloorNum - 1, 
                             orddict:store(FloorNum, 
                                           parse_line(Line), Floors)}
                    end, {length(Lines), orddict:new()}, Lines),
    %% Put elevator on floor 1.
    orddict:update(1, fun(V) -> sets:add_element(e, V) end, Floors).

parse_line(Line) ->
    ["The", Floor, "floor", "contains"|Items] = string:tokens(Line, " .,"),
    parse_items(Items, sets:new()).

parse_items([], Set) -> Set;
parse_items(["and"|Rest], Set) -> parse_items(Rest, Set);
parse_items(["a", [C|_], [T|_]|Rest], Set) ->
    parse_items(Rest, sets:add_element(list_to_atom([C,T]), Set));
parse_items(["nothing", "relevant"], Set) ->
    Set.
 
%%% Printer

print(Floors) ->
    lists:map(fun({FloorNum, Floor}) ->
                      io:format("~w ~w~n", 
                                [FloorNum, sets:to_list(Floor)])
              end, lists:reverse(Floors)).

