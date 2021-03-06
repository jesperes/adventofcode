-module(aoc2015_day09).

-include_lib("eunit/include/eunit.hrl").

testinput() ->
  [{london, dublin, 464},
   {london, belfast, 518},
   {dublin, belfast, 141}].

realinput() ->
  List = inputs:get_as_lines(2015, 9),
  lists:map(fun(Line) ->
                [From, "to", To, "=", Dist] = string:tokens(Line, " "),
                {list_to_atom(From),
                 list_to_atom(To),
                 list_to_integer(Dist)}
            end, List).

permute([]) -> [[]];
permute(L) -> [[X|Y] || X <- L, Y <- permute(L -- [X])].

city_pair(C1, C2) when C1 < C2 -> {C1, C2};
city_pair(C1, C2) -> {C2, C1}.

get_cities(Cities) ->
  get_cities(Cities, {sets:new(), maps:new()}).
get_cities([], Cities) ->
  Cities;
get_cities([{City1, City2, Dist}|Rest], {Set, Map}) ->
  S0 = sets:add_element(City1, Set),
  S1 = sets:add_element(City2, S0),
  M0 = maps:put(city_pair(City1, City2), Dist, Map),
  get_cities(Rest, {S1, M0}).

distance([], _Map) -> 0;
distance([_], _Map) -> 0;
distance([A,B|Rest], Map) ->
  maps:get(city_pair(A, B), Map) + distance([B|Rest], Map).

day09_test_() ->
  [ {"Part 1 & 2", fun() -> ?assertEqual({251,898}, main(realinput())) end}
  , {"Test inputs", fun() -> ?assertEqual({605,982}, main(testinput())) end}
  ].

main(Input) ->
  {Set, Map} = get_cities(Input),
  Cities = sets:to_list(Set),
  Distances =  [distance(CityList, Map) || CityList <- permute(Cities)],
  {lists:min(Distances), lists:max(Distances)}.
