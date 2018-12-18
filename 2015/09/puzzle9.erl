-module(puzzle9).

-compile([export_all]).


testinput() ->
    [{london, dublin, 464},
     {london, belfast, 518},
     {dublin, belfast, 141}].

realinput() ->
    {ok, Binary} = file:read_file("input.txt"),
    Tokens = string:tokens(binary_to_list(Binary), "\n"),
    lists:map(fun(Line) ->
                      [From, "to", To, "=", Dist] = string:tokens(Line, " "),
                      {list_to_atom(From),
                       list_to_atom(To),
                       list_to_integer(Dist)}
              end, Tokens).

permute([]) -> [[]];
permute(L) -> [[X|Y] || X <- L, Y <- permute(L -- [X])].

city_pair(C1, C2) when C1 < C2 ->
    {C1, C2};
city_pair(C1, C2) ->
    {C2, C1}.

get_cities(Cities) ->
    get_cities(Cities, {sets:new(), maps:new()}).
get_cities([], Cities) ->
    Cities;
get_cities([{City1, City2, Dist}|Rest], {Set, Map}) ->
    S0 = sets:add_element(City1, Set),
    S1 = sets:add_element(City2, S0),
    M0 = maps:put(city_pair(City1, City2), Dist, Map),
    get_cities(Rest, {S1, M0}).

distance([], _Map) -> 
    0;
distance([_], _Map) -> 
    0;
distance([A,B|Rest], Map) -> 
    maps:get(city_pair(A, B), Map) + 
        distance([B|Rest], Map).

start() ->
    %% Input = testinput(),
    Input = realinput(),
    {Set, Map} = get_cities(Input),
    Cities = sets:to_list(Set),
    Distances = 
        [{CityList, distance(CityList, Map)} || CityList <- permute(Cities)],
    
    
    MinDist = 
        lists:foldl(fun({CityList, Dist}, Acc) when Dist < Acc ->
                            Dist;
                       (_, Acc) ->
                            Acc
                    end, 10000000, Distances),
    MaxDist = 
        lists:foldl(fun({CityList, Dist}, Acc) when Dist > Acc ->
                            Dist;
                       (_, Acc) ->
                            Acc
                    end, 0, Distances),
    %% {MinDist, Distances, Cities, Map}.
    {MinDist, MaxDist}.





