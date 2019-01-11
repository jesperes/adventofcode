%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2019, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2019 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(puzzle19).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

start(NumElves) ->
    Presents = lists:foldl(fun(Elf, Acc) -> 
                                   gb_trees:insert(Elf, 1, Acc)
                           end, gb_trees:empty(), lists:seq(1, NumElves)),
    steal_presents(Presents, NumElves, gb_trees:iterator(Presents)).

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

