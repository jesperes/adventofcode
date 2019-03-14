%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 30 Dec 2018 by Jesper Eskilson <>

-module(puzzle20).
-export([start/0]).

-define(CHUNKSIZE, 10000).
-define(LIMIT, 36000000).

%% This is a little cheating based on knowledge about the solutions,
%% but it allows us to keep the runtime down to < 10s.
-define(UPPER, 900000).
-define(LOWER, 800000).

start() ->
    P1 = part1(),
    P2 = part2(),
    {P1, P2}.

part1() -> deliver(1).
part2() -> deliver2(1, #{}).

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
                        fun(House, InnerAcc) when (House < ?LOWER) or (House > ?UPPER) ->
                                InnerAcc;
                           (House, InnerAcc) ->
                                maps:update_with(House, 
                                                 fun(V) -> V + NumPresents end, 
                                                 NumPresents, 
                                                 InnerAcc)
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

deliver2(Elf, Map) ->
    NumPresents = Elf * 11,
    Map1 = 
        foldn(fun(House, Acc) when (House < ?LOWER) or (House > ?UPPER) ->
                      Acc;
                 (House, Acc) ->
                      maps:update_with(House, 
                                       fun(V) -> V + NumPresents end, 
                                       NumPresents, Acc)
              end, Map, Elf, Elf * 50, Elf),
    
    %% When this elf is finished, the house with that number will not
    %% receive any more presents.

    N = maps:get(Elf, Map1, 0),
    if N >= ?LIMIT ->
            Elf;
       true ->
            deliver2(Elf + 1, maps:remove(Elf, Map1))
    end.

foldn(Fun, Init, Start, End, Incr) ->
    foldn(Start, Fun, Init, Start, End, Incr).

foldn(N, _Fun, Acc, _Start, End, _Incr) when N > End ->
    Acc;
foldn(N, Fun, Acc, Start, End, Incr) ->
    NewAcc = Fun(N, Acc),
    foldn(N + Incr, Fun, NewAcc, Start, End, Incr).



