-module(aoc2015_day24).

-include_lib("eunit/include/eunit.hrl").

%% To avoid combinatorial explosions when looking for possible
%% combination of packages for the first group, we assume that group A
%% will never be larger than 6. This turns out to be good enough.
-define(MAX_GROUP_A_SIZE, 6).

packages() -> [1, 2, 3, 7, 11, 13, 17, 19, 23, 31, 37, 41, 43, 47,
               53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107,
               109, 113].

main_test_() ->
  [ {"Part 1", fun() ->
                   ?assertEqual(11846773891, start_common(3))
               end}
  , {"Part 2", fun() ->
                   ?assertEqual(80393059, start_common(4))
               end}
  ].

start_common(Groups) ->
  Packages = packages(),
  Sum = lists:sum(Packages) div Groups,
  find_first_group_qe(Packages, Sum).

%% Compute the "quantum entanglement" of a group, which is simply the
%% product of all integers in the group.
qe(L) ->
  lists:foldl(fun(V, AccIn) -> V * AccIn end, 1, L).

%% Sort group A candidates on length (smallest first), then
%% on Quantum Entanglement.
sort_fun(A, B) when length(A) /= length(B) ->
  length(A) =< length(B);
sort_fun(A, B) ->
  qe(A) =< qe(B).

find_first_group_qe(Packages, Sum) ->
  GroupAs = find_a(Packages, Sum),
  [Best|_] = lists:sort(fun sort_fun/2, GroupAs),
  %% I assume here that the remaining elements can be divided into 2
  %% (3) groups with the correct sum. This assumption turned out to be
  %% correct.
  qe(Best).

%% Find best choices for first group (A)
find_a(Data, Sum) ->
  lists:foldl(
    fun(N, As) ->
        Combos = cnr(N, Data),
        As ++ lists:filter(fun(X) -> lists:sum(X) == Sum end, Combos)
    end, [], lists:seq(1, ?MAX_GROUP_A_SIZE)).

%% Returns all possible combinations of a given length.
%% https://github.com/joergen7/lib_combin/blob/master/src/lib_combin.erl
cnr(N, SrcLst) when N >= 0 ->
  Cnr = fun
          Cnr(0, _, Acc)     -> [Acc];
          Cnr(_, [], _)      -> [];
          Cnr(M, [H|T], Acc) ->
            case T of
              []    -> Cnr(M-1, [], [H|Acc]);
              [_|_] -> Cnr(M-1, T, [H|Acc]) ++ Cnr(M, T, Acc)
            end
        end,

  Cnr(N, SrcLst, []).
