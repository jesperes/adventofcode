-module(puzzle3).
-export([main/0]).
-compile([export_all]).

main() ->
    Areas = get_areas(),
    {{part1, start1(Areas)},
     {part2, start2(Areas)}}.

start1(Areas) ->
    ClaimedAreas = count_claims(Areas, #{}),
    count_overlaps(ClaimedAreas).

start2(Areas) ->
    [{Id, _, _, _, _}|_]
        = lists:dropwhile(fun(A) ->
                                  overlaps(A, Areas)
                          end, Areas),
    Id.

%%% Part 1

%% Count the number of square inches which overlap by more than 2.
count_overlaps(Map) ->
    count_overlaps0(maps:iterator(Map), 0).

count_overlaps0(It0, N) ->
    case maps:next(It0) of
        {_, V, It1} when V >= 2 ->
            count_overlaps0(It1, N + 1);
        {_, _, It1} ->
            count_overlaps0(It1, N);
        _ ->
            N
    end.


%% Returns a map from Pos -> NumberOfClaims, one position for each
%% square inch.
count_claims([], Map) ->
    Map;
count_claims([{Id, L, T, W, H}|Areas], Map) ->
    NewMap = claim_area(Id, L, T, W, H, Map),
    count_claims(Areas, NewMap).

claim_area(_, L, T, W, H, Map) ->
    Coords =
        [{X,Y} || X <- lists:seq(L, L + W - 1),
                  Y <- lists:seq(T, T + H - 1)],
    lists:foldl(fun(K, AccIn) ->
                        maps:update_with(K,
                                         fun(V) ->
                                                 V + 1
                                         end, 1, AccIn)
                end, Map, Coords).


%%% Part 2

%% Does area A overlap any area in Areas?
overlaps(A, Areas) ->
    lists:any(fun(A1) ->
                      overlaps0(A, A1)
              end, Areas).

overlaps0(A, A) ->
    false;
overlaps0(A1, A2) ->
    {_, L1, T1, W1, H1} = A1,
    {_, L2, T2, W2, H2} = A2,

    %% Each of these are true if the two areas do not overlap (either
    %% side-by-side or over-and-under, or both). If they are all
    %% false, then the two areas overlap.
    not ((L1 + W1 =< L2) or
         (L2 + W2 =< L1) or
         (T1 + H1 =< T2) or
         (T2 + H2 =< T1)).


%%% Parser

get_areas() ->
    Input = read_file_lines("input.txt"),
    lists:map(fun parse_line/1, Input).

read_file_lines(File) ->
    {ok, Binary} = file:read_file(File),
    string:tokens(binary_to_list(Binary), "\n").

parse_line(Line) ->
    list_to_tuple(
        lists:map(fun list_to_integer/1,
                  string:tokens(Line, "#@ ,:x"))).
