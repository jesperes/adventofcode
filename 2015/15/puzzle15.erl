%%% @author  <jespe@LAPTOP-P6HKA27J>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 19 Dec 2018 by  <jespe@LAPTOP-P6HKA27J>

-module(puzzle15).
-compile([export_all]).

input() ->
    List = ["Sugar: capacity 3, durability 0, flavor 0, texture -3, calories 2",
	    "Sprinkles: capacity -3, durability 3, flavor 0, texture 0, calories 9",
	    "Candy: capacity -1, durability 0, flavor 4, texture 0, calories 1",
	    "Chocolate: capacity 0, durability 0, flavor -2, texture 2, calories 8"],
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

zero_if_negative(X) when X < 0 -> 0;
zero_if_negative(X) -> X.

start() ->
    Ingredients = input(),
    Combinations = 
	[#{'Sugar' => X1,
	   'Sprinkles' => X2, 
	   'Candy' => X3, 
	   'Chocolate' => X4} || 
	    X1 <- lists:seq(1, 100),
	    X2 <- lists:seq(1, 100),
	    X3 <- lists:seq(1, 100),
	    X4 <- lists:seq(1, 100),
	    X1 + X2 + X3 + X4 == 100],
    
    lists:map(fun(Measures) ->
		      Sugar = maps:get('Sugar', Measures),
		      Sprinkles = maps:get('Sprinkles', Measures),
		      Candy = maps:get('Candy', Measures),
		      Chocolate = maps:get('Chocolate', Measures),

		      SugarScore =
			  zero_if_negative(Sugar * maps:get('Sugar', Ingredients)),
		      SprinklesScore =
			  zero_if_negative(Sprinkles * maps:get('Sprinkles', Ingredients)),
		      CandyScore =
			  zero_if_negative(Candy * maps:get('Candy', Ingredients)),
		      ChocolateScore =
			  zero_if_negative(Chocolate * maps:get('Chocolate', Ingredients)),

		      Score =
			  SugarScore *
			  SprinklesScore *
			  CandyScore *
			  ChocolateScore,

		      
			  
	      end, Combinations).
		      
    

