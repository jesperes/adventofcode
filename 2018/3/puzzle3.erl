-module(puzzle3).
-export([start1/0, start2/0]).

start1() ->
    %% TestData = ["#1 @ 1,3: 4x4",
    %%             "#2 @ 3,1: 4x4",
    %%             "#3 @ 5,5: 2x2"],
    
    Input = read_file_lines("input.txt"),
    Areas = lists:map(fun parse_line/1, Input),
    ClaimedAreas = count_claims(Areas, #{}),
    display_fabric(8, 8, ClaimedAreas),
    
    maps:fold(fun(_, Claims, AccIn) ->
                      if length(Claims) > 1 ->
                              1 + AccIn;
                         true ->
                              AccIn
                      end
              end, 0, ClaimedAreas).

read_file_lines(File) ->
    {ok, Binary} = file:read_file(File),
    string:tokens(binary_to_list(Binary), "\n").

parse_line(Line) ->
    [Id, Left, Top, W, H] = 
        lists:map(fun list_to_integer/1, string:tokens(Line, "#@ ,:x")),
    {Id, Left, Top, W, H}.

count_claims([], Map) ->
    Map;
count_claims([{Id, L, T, W, H}|Areas], Map) ->
    NewMap = claim_area(Id, L, T, W, H, Map),
    count_claims(Areas, NewMap).

claim_area(Id, L, T, W, H, Map) ->

    %% The coordinates for each square inch in the claimed area.
    Coords = 
        [{X,Y} || X <- lists:seq(L, L + W - 1),
                  Y <- lists:seq(T, T + H - 1)],
    
    %% Map is a map of {X,Y} coordinates to lists of ids making claims
    %% on that particular square inch.
    lists:foldl(fun(K, AccIn) ->
                        maps:update_with(K, 
                                         fun(V) ->
                                                 [Id|V]
                                         end, [Id], AccIn)
                end, Map, Coords).

display_fabric(W, H, Map) ->
    io:format("~p~n", 
              [[[maps:get({X,Y},Map,[])
                 || X <- lists:seq(0, W - 1)]
                || Y <- lists:seq(0, H - 1)]]).

start2() ->
    %% TestData = ["#1 @ 1,3: 4x4",
    %%             "#2 @ 3,1: 4x4",
    %%             "#3 @ 5,5: 2x2"],
    
    Input = read_file_lines("input.txt"),
    Areas = lists:map(fun parse_line/1, Input),
    ClaimedAreas = count_claims(Areas, #{}),

    AllIds = lists:map(fun({Id, _, _, _, _}) ->
                               Id
                       end, Areas),
    
    %% Find the id which does not share any square in with any other
    %% id.
    [N] = lists:filter(fun(Id) ->
                               not overlaps(Id, ClaimedAreas)
                       end, AllIds),
    N.

overlaps(Id, ClaimedAreas) ->
    %% List where each member is a list containing the ids for each
    %% square inch.
    SquareInches = maps:values(ClaimedAreas),
    
    %% Find the cells which are shared, i.e. has at least 2 ids in
    %% them.
    Shared = lists:filter(fun(SqInch) ->
                                  length(SqInch) >= 2
                          end, SquareInches),
        
    %% If the specified id is a member of any of the shared lists, it
    %% overlaps with another id.
    lists:any(fun(SqInch) ->
                      lists:member(Id, SqInch)
              end, Shared).
