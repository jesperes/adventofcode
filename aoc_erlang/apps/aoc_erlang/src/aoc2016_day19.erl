-module(aoc2016_day19).
-include_lib("eunit/include/eunit.hrl").

presents(N) ->
  lists:foldl(fun(Elf, Acc) ->
                  gb_trees:insert(Elf, 1, Acc)
              end, gb_trees:empty(), lists:seq(1, N)).

start(NumElves) ->
  Presents = presents(NumElves),
  {Elf, _} = steal_presents(Presents, NumElves, gb_trees:iterator(Presents)),
  Elf.

start2(NumElves) ->
  Presents = presents(NumElves),
  {Elf, _} = steal_presents2(Presents, NumElves,
                             gb_trees:iterator(Presents),
                             across(Presents, NumElves)),
  Elf.

across(Presents, NumElves) ->
  Iter = gb_trees:iterator(Presents),
  lists:foldl(fun(_, Acc) ->
                  {_, _, It} = gb_trees:next(Acc),
                  It
              end, Iter, lists:seq(1, NumElves div 2)).

circular_next(Presents, Iter) ->
  case gb_trees:next(Iter) of
    none -> gb_trees:next(gb_trees:iterator(Presents));
    Res  -> Res
  end.

steal_presents(Presents, 1, _Iter) ->
  gb_trees:smallest(Presents);
steal_presents(Presents, NumElves, Iter) ->
  {CurrElf, CurrPres, I0} = circular_next(Presents, Iter),
  {NextElf, NextPres, _I1} = circular_next(Presents, I0),
  P0 = gb_trees:delete(NextElf, Presents),
  P1 = gb_trees:update(CurrElf, CurrPres + NextPres, P0),
  NewIter = gb_trees:iterator_from(NextElf, P1),
  steal_presents(P1, NumElves - 1, NewIter).

steal_presents2(Presents, 1, _Iter, _Across) ->
  gb_trees:smallest(Presents);
steal_presents2(Presents, NumElves, Iter, Across) ->
  {CurrElf, CurrPres, _I0} = circular_next(Presents, Iter),
  {AcrossElf, AcrossPres, Across0} =
    case NumElves rem 2 of
      0 ->
        {_, _, It} = circular_next(Presents, Across),
        circular_next(Presents, It);
      _ ->
        circular_next(Presents, Across)
    end,

  %% ?debugFmt("CurrElf = ~p, AcrossElf = ~p~n", [CurrElf, AcrossElf]),
  P0 = gb_trees:delete(AcrossElf, Presents),
  P1 = gb_trees:update(CurrElf, CurrPres + AcrossPres, P0),
  %% ?debugFmt("Ring: ~p~n", [gb_trees:to_list(P1)]),
  {_, _, NewIter} = gb_trees:next(gb_trees:iterator_from(CurrElf, P1)),
  steal_presents2(P1, NumElves - 1, NewIter, Across0).


main_test_() ->
  NumElves = 3014387,
  [ {"Tests",
     [ ?_assertEqual(3, start(5))
     , ?_assertEqual(2, start2(5))
     ]
    }
  , {"Part 1", timeout, 60, ?_assertMatch(1834471, start(NumElves))}
  , {"Part 2", timeout, 60,
     ?_assertMatch(1420064, start2(NumElves))}
  ].
