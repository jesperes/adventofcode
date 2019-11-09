-module(aoc2015_day17).

-include_lib("eunit/include/eunit.hrl").

combinations([]) ->
  [];
combinations([H | T]) ->
  CT = combinations(T),
  [[H]] ++ [[H | L] || L <- CT] ++ CT.

main_test_() ->
  {"Part 1 & 2",
   fun() ->
       Binary = inputs:get_as_binary(2015, 17),
       Volume = 150,

       Buckets =
         lists:map(fun list_to_integer/1,
                   string:tokens(binary_to_list(Binary), " \n\r")),

       BucketCombos =
         lists:filter(fun(BucketList) ->
                          lists:sum(BucketList) == Volume
                      end, combinations(Buckets)),

       SortedOnLength =
         lists:sort(fun(A, B) ->
                        length(A) =< length(B)
                    end, BucketCombos),

       MinLen = length(lists:nth(1, SortedOnLength)),

       NumShortestCombos =
         length(lists:filter(fun(X) ->
                                 length(X) == MinLen
                             end, SortedOnLength)),

       ?assertEqual(1638, length(BucketCombos)),
       ?assertEqual(17, NumShortestCombos)
   end}.
