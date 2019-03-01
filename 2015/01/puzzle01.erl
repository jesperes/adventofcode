%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2019, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created :  1 Mar 2019 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(puzzle01).

-export([start/0]).

start() ->
    {ok, Bin} = file:read_file("input.txt"),
    count_floors(Bin, 0, 0, undef).

count_floors(<<>>, Acc, _, Neg) -> {Acc, Neg};
count_floors(Bin, Acc, Pos, undef) when Acc < 0 ->
    count_floors(Bin, Acc, Pos, Pos);
count_floors(<<$(,Rest/binary>>, Acc, Pos, Neg) -> 
    count_floors(Rest, Acc + 1, Pos + 1, Neg);
count_floors(<<$),Rest/binary>>, Acc, Pos, Neg) -> 
    count_floors(Rest, Acc - 1, Pos + 1, Neg).

     
    
