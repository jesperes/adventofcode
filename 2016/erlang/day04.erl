-module(day04).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

letter_freq([], Map) ->
    Map;
letter_freq([A|As], Map) ->
    Map2 = case maps:find(A, Map) of
	       {ok, Value} ->
		   maps:update(A, Value + 1, Map);
	       _ ->
		   maps:put(A, 1, Map)
	   end,
    letter_freq(As, Map2).

letter_freq(String) ->
    letter_freq(String, maps:new()).

letter_freq_test() ->
    Map = letter_freq("abcab"),
    2 = maps:get($a, Map),
    2 = maps:get($b, Map),
    1 = maps:get($c, Map).


named_re_match_to_map(Str, ReStr) ->
    {ok, RE} = re:compile(ReStr),
    {namelist, Names} = re:inspect(RE, namelist),
    case re:run(Str, RE, [{capture, all_names}]) of
	{match, Captures} ->
	    Matches = lists:zip(Names, Captures),
	    lists:foldl(fun({Name,{From,To}}, Acc) ->
				maps:put(Name, lists:sublist(Str, From+1, To), Acc)
			end, maps:new(), Matches);
	X ->
	    X
    end.

split_room(Str) ->
    Map = named_re_match_to_map(Str, "(?<name>[a-z\\-]+)-(?<sectorid>[0-9]+)\\[(?<checksum>[a-z]+)\\]"),
    %% ?debugFmt("~s", [maps:get(<<"sectorid">>, Map)]),
    {maps:get(<<"name">>, Map),
     list_to_integer(maps:get(<<"sectorid">>, Map)),
     maps:get(<<"checksum">>, Map)}.



is_real_room(Room) ->
    {Name, SectorId, Checksum} = split_room(Room),

    %% Compute the letter frequency
    FreqMap = letter_freq(Name),
    
    %% Convert map to list of pairs
    FreqPairs = maps:to_list(FreqMap),

    %% Do not include '-'
    List = lists:filter(fun({A, _}) -> A /= $- end, FreqPairs),
    
    %% Sort the list of pairs according to the rules stated in the
    %% problem.
    Sorted = lists:sort(fun({A, Fa}, {B, Fa}) ->
				%% When frequency is same, sort in
				%% alphanumerical ascending order
				A =< B;
			   ({_, Fa}, {_, Fb}) ->
				%% Otherwise sort on frequency descending
				%% (i.e. highest first)
				Fb =< Fa
			end, List),

    %% Take the first five
    {FirstFive, _} = lists:split(5, Sorted),

    ExpectedChecksum = lists:map(fun({Char, _}) -> Char end, FirstFive),
    
    if ExpectedChecksum == Checksum ->
	    {real, Name, SectorId, Checksum};
       true ->
	    {decoy, Name, SectorId, Checksum}
    end.

is_real_room_test() ->
    {real, _, _, _} = is_real_room("a-bb-ccc-dddd-zz-xx-123[dcbxz]"),
    {decoy, _, _, _} = is_real_room("a-bb-ccc-dddd-zz-xx-123[abc]").

find_sum_of_real_rooms([]) ->
    0;
find_sum_of_real_rooms([Room|Rooms]) ->
    case is_real_room(Room) of
	{real, _, SectorId, _} ->
	    SectorId;
	_ ->
	    0
    end + find_sum_of_real_rooms(Rooms).

find_sum_of_real_rooms_test() ->
    Rooms = ["aaaaa-bbb-z-y-x-123[abxyz]",
	     "a-b-c-d-e-f-g-h-987[abcde]",
	     "not-a-real-room-404[oarel]",
	     "totally-real-room-200[decoy]"],
    1514 = find_sum_of_real_rooms(Rooms).

rotate([], _) ->
    [];
rotate([C|Cs], SectorId) ->
    Repl = case C of
	       $- -> 
		   $\s;
	       _ ->
		   ((C - $a + SectorId) rem 26) + $a
	   end,
    [Repl|rotate(Cs, SectorId)].

rotate_test() ->
    "very encrypted name" == rotate("qzmt-zixmtkozy-ivhz-", 343).

decrypt_name({_, Name, SectorId, _}) ->
    {SectorId, rotate(Name, SectorId)}.

decrypt_rooms([]) ->
    [];
decrypt_rooms([Room|Rooms]) ->
    Room0 = is_real_room(Room),
    [decrypt_name(Room0) | decrypt_rooms(Rooms)].

day04_test() ->	    
    Rooms = utils:read_file_lines("input04.txt"),
    Sum = find_sum_of_real_rooms(Rooms),
    ?debugFmt("Sector id sum of real rooms: ~w", [Sum]),
    DecryptedRooms = decrypt_rooms(Rooms),

    %% Find rooms holding "North Pole objects"
    Search = lists:filter(fun({_Id, X}) ->
				  string:str(X, "north") /= 0
			  end, DecryptedRooms),    
    lists:map(fun({Id, Name}) ->
		      ?debugFmt("~s (sector id ~w)", [Name, Id])
	      end, Search).
    

