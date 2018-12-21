%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 20 Dec 2018 by Jesper Eskilson <>

-module(puzzle16).

-compile([export_all]).

mfcsam_output() ->
    #{
      children => 3,
      cats => 7,
      samoyeds => 2,
      pomeranians => 3,
      akitas => 0,
      vizslas => 0,
      goldfish => 5,
      trees => 3,
      cars => 2,
      perfumes => 1
     }.

matches(Map1, Map2) ->
    AllKeys = 
        sets:from_list(maps:keys(Map1) ++ 
                           maps:keys(Map2)).

start() ->
    {ok, Binary} = file:read_file("input.txt"),
    Map = 
        maps:from_list(
          lists:map(fun(Line) ->
                            [_, Num|Rest] = string:tokens(Line, ": ,"),
                            {list_to_integer(Num),
                             list_to_dict(Rest)}
                    end, string:tokens(binary_to_list(Binary), "\r\n"))),

    
    



list_to_dict([]) ->
    #{};
list_to_dict([Key, Value|Rest]) ->
    maps:put(list_to_atom(Key), 
             list_to_integer(Value), list_to_dict(Rest)).
              
    

