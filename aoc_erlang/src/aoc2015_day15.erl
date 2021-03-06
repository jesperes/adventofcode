-module(aoc2015_day15).

-include_lib("eunit/include/eunit.hrl").

day15_test_() ->
  {timeout, 600,
   [ {"Part 1", fun() -> ?assertEqual(222870, part1()) end}
   , {"Part 2", fun() -> ?assertEqual(117936, part2()) end}
   ]}.

input(List) ->
  lists:foldl(fun(X, Map) ->
                  [Ingredient,
                   "capacity", Cap,
                   "durability", Dur,
                   "flavor", Flavor,
                   "texture", Texture,
                   "calories", Calories] = string:tokens(X, ":, "),
                  Map#{
                       list_to_atom(Ingredient) =>
                         #{
                           capacity => list_to_integer(Cap),
                           durability => list_to_integer(Dur),
                           flavor => list_to_integer(Flavor),
                           texture => list_to_integer(Texture),
                           calories => list_to_integer(Calories)
                          }
                      }
              end, #{}, List).


properties() ->
  [capacity, durability, flavor, texture].

ingredient_amount([X1, _, _, _], 'Sugar') -> X1;
ingredient_amount([_, X2, _, _], 'Sprinkles') -> X2;
ingredient_amount([_, _, X3, _], 'Candy') -> X3;
ingredient_amount([_, _, _, X4], 'Chocolate') -> X4;

ingredient_amount([X1, _], 'Butterscotch') -> X1;
ingredient_amount([_, X2], 'Cinnamon') -> X2.

score(Amounts, Ingredients) ->
  %% Amounts is a list [X1,X2,X3,X4] where each X corresponds to an
  %% ingredient and indicates how much of each ingredient to take.

  lists:foldl(
    fun(Prop, Acc) ->
        %% For each property (e.g. 'capacity'), multiply each
        %% ingredient's 'capacity' value with the amount of that
        %% ingredient

        PropVal =
          maps:fold(fun(Ingredient, IngredientMap, Acc0) ->
                        A = ingredient_amount(Amounts, Ingredient),
                        V = maps:get(Prop, IngredientMap),
                        Acc0 + (A * V)
                    end, 0, Ingredients),

        if PropVal < 0 ->
            0;
           true ->
            PropVal * Acc
        end
    end, 1, properties()).

part1() ->
  List = ["Sugar: capacity 3, durability 0, flavor 0, texture -3, calories 2",
          "Sprinkles: capacity -3, durability 3, flavor 0, texture 0, calories 9",
          "Candy: capacity -1, durability 0, flavor 4, texture 0, calories 1",
          "Chocolate: capacity 0, durability 0, flavor -2, texture 2, calories 8"],
  Ingredients = input(List),
  Combinations =
    [[X1, X2, X3, X4] ||
      X1 <- lists:seq(1, 100),
      X2 <- lists:seq(1, 100),
      X3 <- lists:seq(1, 100),
      X4 <- lists:seq(1, 100),
      X1 + X2 + X3 + X4 == 100],

  lists:max(
    lists:map(fun(Amounts) ->
                  score(Amounts, Ingredients)
              end, Combinations)).

score2(Amounts, Ingredients) ->
  %% Amounts is a list [X1,X2,X3,X4] where each X corresponds to an
  %% ingredient and indicates how much of each ingredient to take.

  Score =
    lists:foldl(
      fun(Prop, Acc) ->
          %% For each property (e.g. 'capacity'), multiply each
          %% ingredient's 'capacity' value with the amount of that
          %% ingredient

          PropVal =
            maps:fold(fun(Ingredient, IngredientMap, Acc0) ->
                          A = ingredient_amount(Amounts, Ingredient),
                          V = maps:get(Prop, IngredientMap),
                          Acc0 + (A * V)
                      end, 0, Ingredients),

          if PropVal < 0 ->
              0;
             true ->
              PropVal * Acc
          end
      end, 1, properties()),

  Calories =
    maps:fold(fun(Ingredient, IngredientMap, Acc0) ->
                  A = ingredient_amount(Amounts, Ingredient),
                  V = maps:get(calories, IngredientMap),
                  Acc0 + (A * V)
              end, 0, Ingredients),
  {Score, Calories}.


part2() ->
  List = ["Sugar: capacity 3, durability 0, flavor 0, texture -3, calories 2",
          "Sprinkles: capacity -3, durability 3, flavor 0, texture 0, calories 9",
          "Candy: capacity -1, durability 0, flavor 4, texture 0, calories 1",
          "Chocolate: capacity 0, durability 0, flavor -2, texture 2, calories 8"],

  Ingredients = input(List),
  Combinations =
    [[X1, X2, X3, X4] ||
      X1 <- lists:seq(1, 100),
      X2 <- lists:seq(1, 100),
      X3 <- lists:seq(1, 100),
      X4 <- lists:seq(1, 100),
      X1 + X2 + X3 + X4 == 100],

  lists:max(
    lists:filtermap(
      fun({Score, Cal}) when Cal == 500 ->
          {true, Score};
         (_) ->
          false
      end,
      lists:map(fun(Amounts) ->
                    score2(Amounts, Ingredients)
                end, Combinations))).
