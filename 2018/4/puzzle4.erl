-module(puzzle4).
-export([start1/0, start2/0]).

start1() ->
    %% LogEntries is a list of lists on the form [Year, Month, Day,
    %% Hour, Minute|What] where What is either
    %% 
    %% ["falls", "asleep"]
    %% ["Guard", "#XXX", "begins", "shift"]
    %% ["wakes", "up"]
    %% 
    LogEntries = scan_input(lists:map(fun parse_date_str/1, input())),

    %% Construct a list of ranges for each sleeping period
    SleepRanges = count_sleep_minutes(LogEntries),

    %% Given this list of sleep ranges, find the sleepiest guard.
    SleepyGuard = find_sleepiest_guard(SleepRanges),

    %% Given the list of log entries and the sleepiest guard, find the
    %% minute where the guard sleeps the most, summing over all days
    %% the guard sleeps.
    SleepiestMinute = find_sleepiest_minute(SleepyGuard, LogEntries),

    [$#|GuardNumStr] = SleepyGuard,
    GuardNum = list_to_integer(GuardNumStr),
    GuardNum * SleepiestMinute.
    
parse_date_str(Str) ->
    string:tokens(Str, "[-] :").

input() ->
    {ok, Binary} = file:read_file("input.txt"),
    lists:sort(string:tokens(binary_to_list(Binary), "\n")).

testdata() ->
    lists:sort(
      ["[1518-11-01 00:00] Guard #10 begins shift",
       "[1518-11-01 00:05] falls asleep",
       "[1518-11-01 00:25] wakes up",
       "[1518-11-01 00:30] falls asleep",
       "[1518-11-01 00:55] wakes up",
       "[1518-11-01 23:58] Guard #99 begins shift",
       "[1518-11-02 00:40] falls asleep",
       "[1518-11-02 00:50] wakes up",
       "[1518-11-03 00:05] Guard #10 begins shift",
       "[1518-11-03 00:24] falls asleep",
       "[1518-11-03 00:29] wakes up",
       "[1518-11-04 00:02] Guard #99 begins shift",
       "[1518-11-04 00:36] falls asleep",
       "[1518-11-04 00:46] wakes up",
       "[1518-11-05 00:03] Guard #99 begins shift",
       "[1518-11-05 00:45] falls asleep",
       "[1518-11-05 00:55] wakes up]"]).

int(N) ->
    list_to_integer(N).

scan_input(LogEntries) -> 
    %% tuple is { GuardNum, FallAsleepTime }
    scan_input(LogEntries, {noguard, notime}).

scan_input([], _) ->
    [];
scan_input([[_, _, _, _, _, "Guard", GuardNum, "begins", "shift"]|Rest], 
           {_, _}) ->
    scan_input(Rest, {GuardNum, onduty});
scan_input([[_, _, _, _, M, "falls", "asleep"]|Rest], 
           {GuardNum, _}) ->
    scan_input(Rest, {GuardNum, M});
scan_input([[Y, M, D, _H, EndMin, "wakes", "up"]|Rest], 
           {GuardNum, StartMin}) ->
    [{int(Y), int(M), int(D), GuardNum, int(StartMin), int(EndMin)}|
     scan_input(Rest, {GuardNum, onduty})].

%% Count the number of minutes slept by each guard
count_sleep_minutes(List) ->
    count_sleep_minutes(List, #{}).
count_sleep_minutes([], Map) ->
    Map;
count_sleep_minutes([{_Y, _M, _D, Guard, Start, End}|Rest], Map) ->
    Duration = End - Start,
    NewMap = maps:update_with(Guard, fun(V) ->
                                             V + Duration
                                     end, Duration, Map),
    count_sleep_minutes(Rest, NewMap).

%% Find the guard which has slept the most number of minutes
%% (i.e. find the key with the largest value in the given map)
find_sleepiest_guard(Map) ->
    {Guard, _} = 
        maps:fold(fun(K, V, {_, CurrMax}) when V >= CurrMax ->
                          {K, V};
                     (_K, _V, AccIn) ->
                          AccIn
                  end, {undefined, 0}, Map),
    Guard.

find_sleepiest_minute(Guard, Intervals) ->
    GuardList = lists:filter(fun({_, _, _, Guard0, _, _}) ->
                                 Guard0 =:= Guard
                             end, Intervals),
    
    %% GuardList contains dates and intervals for when the given guard
    %% was sleeping.
    %% 
    %% [{1518,11,1,"#10",5,25},
    %%  {1518,11,1,"#10",30,55},
    %%  {1518,11,3,"#10",24,29}]
    
    NumSleepsAtMinuteList = 
        lists:map(fun(Minute) ->
                          {Minute, number_of_sleeping_days(GuardList, Minute)}
                  end, [Min || Min <- lists:seq(0, 59)]),
    
    find_max_sleeping_days(NumSleepsAtMinuteList, 0, notfound).

%% Returns the number of days a guard has been sleeping at the given
%% minute.
number_of_sleeping_days([], _) ->
    0;
number_of_sleeping_days([{_Y, _M, _D, _Guard, Start, End}|Rest], Minute) ->    
    if (Minute >= Start) and (Minute < End) ->
            1 + number_of_sleeping_days(Rest, Minute);
       true ->
            number_of_sleeping_days(Rest, Minute)
    end.
   
find_max_sleeping_days([], _Max, Minute) ->
    Minute;
find_max_sleeping_days([{Minute, NumSleepsAtMinute}|Rest], Max, _) 
  when NumSleepsAtMinute > Max ->
    find_max_sleeping_days(Rest, NumSleepsAtMinute, Minute);
find_max_sleeping_days([_|Rest], Max, Minute) ->
    find_max_sleeping_days(Rest, Max, Minute).


start2() ->
    %% Here we use a different strategy to find the sleepiest guard.
    %% Over all the guards, which guard is most frequently asleep on
    %% the same minute.

    %% LogEntries = scan_input(lists:map(fun parse_date_str/1, testdata())),
    LogEntries = scan_input(lists:map(fun parse_date_str/1, input())),

    %% This map maps {Guard,Minute} tuples to the total number of
    %% minutes that guard sleeps any given minute.
    Map = 
        lists:foldl(fun(Min, MapAcc) ->
                            guards_asleep_at(LogEntries, Min, MapAcc)
                    end, #{}, lists:seq(0, 59)),
    
    %% Find the larges value in the map
    {GuardId, SleepiestMinute, NumSleeps} = 
        maps:fold(fun({Guard, Minute} = _Key, NumSleeps = _Value, 
                      {_CurrGuard, _CurrMinute, Max}) when NumSleeps > Max ->
                          {Guard, Minute, NumSleeps}; %% we have a new max, at 'Minute'
                     (_, _, AccIn) ->
                          AccIn
                  end, {undefined, undefined, 0}, Map),
    
    [$#|GuardNum] = atom_to_list(GuardId),
    {GuardId, SleepiestMinute, NumSleeps, list_to_integer(GuardNum) * SleepiestMinute}.
    
                          

%% Update a map with which guards are asleep at which minute
guards_asleep_at(SleepRanges, Minute, Map) ->
    lists:foldl(fun({_Y, _M, _D, Guard, Start, End}, MapAcc) ->
                        if (Minute >= Start) and (Minute < End) ->
                                maps:update_with({list_to_atom(Guard), 
                                                  Minute},
                                                 fun(V) ->
                                                         V + 1
                                                 end, 1, MapAcc);
                           true ->
                                MapAcc
                        end
                end, Map, SleepRanges).
