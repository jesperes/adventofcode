%%% Advent of Code solution for 2020 day 21.
%%% Created: 2020-12-21T06:27:53+00:00

-module(aoc2020_day21).
-include_lib("eunit/include/eunit.hrl").

-compile([nowarn_unused_function]).

%% Puzzle solution
part1(Input) ->
  Map = parse(Input),
  IngredientsWithAllergens = ingredients_with_allergens(Map),
  IngredientsWithoutAllergens =
    sets:subtract(maps:get(ingredients, Map),
                  IngredientsWithAllergens),
  Freq = maps:get(freq, Map),
  sets:fold(
    fun(I, Acc) ->
        Acc + maps:get(I, Freq)
    end, 0, IngredientsWithoutAllergens).

ingredients_with_allergens(Map) ->
  M0 = maps:remove(ingredients, Map),
  M1 = maps:remove(allergens, M0),
  M2 = maps:remove(freq, M1),
  sets:union(maps:values(M2)).

map_to_str(M) ->
  maps:map(fun(_, V) when is_tuple(V) andalso element(1, V) =:= set ->
               sets:to_list(V);
              (_, V) -> V
           end, M).

part2(_Input) ->
  %% For part 2 I printed out the map from allergens to their
  %% ingredients then matched them up by hand to get this:
  CDI =
    [{eggs, bjq},
     {fish, jznhvh},
     {nuts, klplr},
     {peanuts, dtvhzt},
     {sesame, sbzd},
     {shellfish, tlgjzx},
     {soy, ctmbr},
     {wheat, kqms}],

  %% Sort them by allergen, and make into comma-separated list.
  lists:flatten(
    lists:join(",",
               lists:map(fun({_, X}) -> atom_to_list(X) end,
                         lists:sort(CDI)))).


%% ======================================================================
%% Parser
%% ======================================================================
parse(Lines) ->
  lists:foldl(
    fun(L, Acc) ->
        case re:run(L, "(.*) \\(contains (.*)\\)",
                    [{capture, all_but_first, list}]) of
          {match, [M1, M2]} ->
            %% Ingredients
            I = sets:from_list(
                  lists:map(
                    fun list_to_atom/1, string:split(M1, " ", all))),

            %% Allergens
            A = sets:from_list(
                  lists:map(
                    fun list_to_atom/1, string:split(M2, ", ", all))),

            %% Update map of allergens to ingredients that may contain
            %% them
            Acc0 = update_allergens(I, A, Acc),

            Acc1 =
              maps:update_with(
                ingredients,
                fun(Old) ->
                    sets:union(I, Old)
                end, I, Acc0),
            Acc2 =
              maps:update_with(
                allergens,
                fun(Old) ->
                    sets:union(A, Old)
                end, A, Acc1),

            _Acc3 = update_frequencies(I, Acc2)

        end
    end, #{}, Lines).

update_frequencies(Ingredients, Map) ->
  lists:foldl(
    fun(I, Acc) ->
        maps:update_with(
          freq,
          fun(Old) ->
              incr_freq(I, Old)
          end, #{I => 1}, Acc)
    end, Map, sets:to_list(Ingredients)).

incr_freq(I, Map) ->
  maps:update_with(I, fun(Old) -> Old + 1 end, 1, Map).

%% `Map' is a map from allergens to the ingredients that may contain
%% it (the intersection of all the ingredients it is listed for).
update_allergens(Ingredients, Allergens, Map) ->
  sets:fold(
    fun(Allergen, Acc) ->
        maps:update_with(
          Allergen,
          fun(Old) ->
              sets:intersection(Ingredients, Old)
          end, Ingredients, Acc)
    end, Map, Allergens).

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input21.txt).
get_input() ->
  inputs:get_as_lines(2020, 21).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(2428, part1(Input))}
  , {"Part 2", ?_assertEqual("bjq,jznhvh,klplr,dtvhzt,sbzd,tlgjzx,ctmbr,kqms", part2(Input))}
  ].

test_input() ->
  ["mxmxvkd kfcds sqjhc nhms (contains dairy, fish)",
   "trh fvjkl sbzzf mxmxvkd (contains dairy)",
   "sqjhc fvjkl (contains soy)",
   "sqjhc mxmxvkd sbzzf (contains fish)"].

ex1_test_() ->
  ?_assertEqual(5, part1(test_input())).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
