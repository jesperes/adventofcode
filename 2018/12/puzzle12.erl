%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 12 Dec 2018 by Jesper Eskilson <>

-module(puzzle12).
-export([start1/0, start2/0]).

-include_lib("eunit/include/eunit.hrl").

start1() ->
    start0(20).

start2() ->
    { 
      start0(50),
      start0(500),
      start0(5000),
      start0(50000),
      start0(500 * 1000)
      %% 5 * 10^4 -> 2651362
      %% 5 * 10^5 -> 26501362
      %% 5 * 10^6 -> 265001362
      %% 5 * 10^7 -> 2650001362
      %% 5 * 10^8 -> 26500001362
      %% 5 * 10^9 -> 265000001362
      %% 5 * 10^10 -> 2650000001362
      %% 5 * 10^11 -> 26500000001362
      %% start0(5000000) 
    }.


start0(Limit) ->

    {_N, Pots, Rules} = parse_data(realdata()),

    FinalPots = apply_steps(Limit, Pots, Rules),
    
    %% io:format("Final pots: ~p~n", 
    %%           [pot_set_to_string(FinalPots)]),
    %% io:format("Final pots: ~p~n", 
    %%           [lists:sort(sets:to_list(FinalPots))]),

    sets:fold(fun(PotNr, Sum) ->
                      PotNr + Sum
              end, 0, FinalPots).
       
apply_steps(0, Pots, _Rules) ->
    Pots;
apply_steps(N, Pots, Rules) ->
    Pots0 = apply_step(Pots, Rules),
    apply_steps(N - 1, Pots0, Rules).

realdata() ->
    {ok, Binary} = file:read_file("input.txt"),
    binary_to_list(Binary).

testdata() ->
    "initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #".

parse_data(String) ->
    [First|Rest] = string:tokens(String, "\n"),
    ["initial", "state", InitState] = string:tokens(First, " :"),
    
    {N, Map} =
        lists:foldl(fun(Char, {N, Acc}) ->
                            case Char of 
                                $# ->
                                    {N+1, sets:add_element(N, Acc)};
                                _ ->
                                    {N+1, Acc}
                            end
                    end, {0, sets:new()}, InitState),
    
    Rules = 
        lists:map(fun(Line) ->
                          [From, To] = string:tokens(Line, " =>"),
                          {From, To}
                  end, Rest),
    {N, Map, Rules}.


bounds(Pots) ->
    MaxPot = 
        sets:fold(fun(Elem, Max) when Max == undef ->
                          Elem;
                     (Elem, Max) when Elem > Max ->
                          Elem;
                     (_,Max) ->
                          Max
                  end, undef, Pots),
    MinPot = 
        sets:fold(fun(Elem, Min) when Min == undef ->
                          Elem;
                     (Elem, Min) when Elem < Min ->
                          Elem;
                     (_,Min) ->
                          Min
                  end, undef, Pots),
    {MinPot-5, MaxPot+5}.

pot_set_to_string(Set) ->
    {Start, End} = bounds(Set),
    [ case sets:is_element(PotNr, Set) of
          true ->
              $#;
          _ ->
              $.
      end || PotNr <- lists:seq(Start, End)].

apply_step(Pots, Rules) ->
    {Start, End} = bounds(Pots),
    lists:foldl(
      fun(PotNr, PotsWithPlants) ->
              case has_plant(PotNr, Pots, Rules) of
                  true ->
                      sets:add_element(PotNr, PotsWithPlants);
                  _ ->
                      PotsWithPlants
              end
      end, sets:new(), lists:seq(Start, End)).

has_plant(PotNr, Pots, Rules) ->
    Nbrs = get_neighborhood(PotNr, Pots),
    get_matching_rule(Nbrs, Rules) == $#.

get_matching_rule(_Nbrs, []) ->
    $.;
get_matching_rule(Nbrs, [{Nbrs,[To]}|_Rest]) ->
    To;
get_matching_rule(Nbrs, [_|Rest]) ->
    get_matching_rule(Nbrs, Rest).

get_neighborhood(PotNr, Pots) ->
    [ case sets:is_element(N, Pots) of
          true ->
              $#;
          false ->
              $.
      end || N <- lists:seq(PotNr - 2, PotNr + 2)].

get_neighborhood_test() -> 
    S = sets:from_list([2,3]),
    ?assertEqual("...##", get_neighborhood(1, S)),
    ?assertEqual("..##.", get_neighborhood(2, S)).

    

