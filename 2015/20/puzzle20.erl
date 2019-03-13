%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 30 Dec 2018 by Jesper Eskilson <>

-module(puzzle20).
-export([start/0]).


-compile([export_all]).

-define(CHUNKSIZE, 10000).
-define(LIMIT, 36000000).

start() ->
    {part1(), part2()}.

part1() ->
    deliver(1).

part2() ->
    ok.

%% Deliver presents to houses in the range [StartHouse, StartHouse +
%% ChunkSize).
deliver(StartHouse) ->
    %% If StartHouse is 1, ChunkSize == 1000, then we are going to
    %% deliver presents to houses 1..1000. Elf number 1000 is the last
    %% elf which is going to deliver presents.
    LastElf = LastHouse = StartHouse + ?CHUNKSIZE - 1,

    Presents = 
        foldn(fun(ElfNum, Acc) ->
                      %% Find the first house this elf should deliver
                      %% presents to
                      FirstHouse = StartHouse + (ElfNum - (StartHouse rem ElfNum)),
                      NumPresents = ElfNum * 10,
                      foldn(
                        fun(House, InnerAcc) ->
                                maps:update_with(House, 
                                                 fun(V) -> V + NumPresents end, 
                                                 NumPresents, 
                                                 InnerAcc)
                                %% map_increment(House, ElfNum * 10, InnerAcc)
                        end, Acc, FirstHouse, LastHouse, ElfNum)
                          
              end, #{}, 1, LastElf, 1),
    
    case maps:fold(
           fun(K, V, AccIn) when (V >= ?LIMIT) and (K < AccIn) -> K;
              (_, _, AccIn) -> AccIn
           end, undef, Presents) 
    of
        undef -> deliver(StartHouse + ?CHUNKSIZE);
        House -> House
    end.

foldn(Fun, Init, Start, End, Incr) ->
    foldn(Start, Fun, Init, Start, End, Incr).

foldn(N, _Fun, Acc, _Start, End, _Incr) when N > End ->
    Acc;
foldn(N, Fun, Acc, Start, End, Incr) ->
    NewAcc = Fun(N, Acc),
    foldn(N + Incr, Fun, NewAcc, Start, End, Incr).


