%%% @author  <jespe@LAPTOP-P6HKA27J>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Dec 2018 by  <jespe@LAPTOP-P6HKA27J>

-module(puzzle12).
-compile([export_all]).

input() ->
    {ok, Binary} = file:read_file("input.txt"),
    Binary.

start() ->
    Obj = jsone:decode(input()),
    {count(Obj), count_nored(Obj)}.

count(X) when is_number(X) ->
    X;
count(X) when is_binary(X) ->
    0;
count(X) when is_map(X) ->
    maps:fold(fun(_,V,Acc) -> count(V) + Acc end, 0, X);
count(X) when is_list(X) ->
    lists:foldl(fun(V,Acc) -> count(V) + Acc end, 0, X).


count_nored(X) when is_number(X) ->
    X;
count_nored(X) when is_binary(X) ->
    0;
count_nored(X) when is_map(X) ->
    {IsRed, Sum} = 
	maps:fold(fun(_,<<"red">>,{_, Acc}) -> 
			  {true, 0};
		     (_,V,{IsRed, Acc}) ->
			  {IsRed, Acc + count_nored(V)}
		  end, {false, 0}, X),
    case IsRed of
	true ->
	    0;
	false ->
	    Sum
    end;
count_nored(X) when is_list(X) ->
    lists:foldl(fun(V,Acc) -> count_nored(V) + Acc end, 0, X).



   

    



    
	  
