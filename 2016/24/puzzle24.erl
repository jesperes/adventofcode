%%% @author  <jespe@LAPTOP-P6HKA27J>
%%% @copyright (C) 2019, 
%%% @doc
%%%
%%% @end
%%% Created : 13 Jan 2019 by  <jespe@LAPTOP-P6HKA27J>

-module(puzzle24).
-compile([export_all]).

start() ->
    {ok, Binary} = file:read_file("testinput.txt"),
    Lines = string:tokens(binary_to_list(Binary), "\n\r"),
    {_, Map} = lists:foldl(fun parse_line/2, {0, #{}}, Lines).


parse_line(, AccIn) ->
    
