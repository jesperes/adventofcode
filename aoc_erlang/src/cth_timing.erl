%% CT hook for producing data suitable for plotting with gnuplot
-module(cth_timing).

-export([ init/2
        , pre_init_per_suite/3
        , pre_init_per_testcase/4
        , post_end_per_testcase/5
        , post_end_per_suite/4
        , terminate/1
        ]).

ts() ->
   erlang:system_time(microsecond).

init(_Id, _Opts) ->
  {ok, #{}}.

pre_init_per_suite(Suite, Config, State) ->
  {Config, maps:put({start_suite, Suite}, ts(), State)}.

pre_init_per_testcase(Suite, TC, Config, State) ->
  {Config, maps:put({start_tc, Suite, TC}, ts(), State)}.

post_end_per_testcase(Suite, TC, _Config, Return, State) ->
  Elapsed = ts() - maps:get({start_tc, Suite, TC}, State),
  {Return, maps:put({elapsed_tc, Suite, TC}, Elapsed, State)}.

post_end_per_suite(Suite, _Config, Return, State) ->
  Elapsed = ts() - maps:get({start_suite, Suite}, State),
  {Return, maps:put({elapsed_suite, Suite}, Elapsed, State)}.

terminate(State) ->
  try
    ct:print("~s", [timings(State)])
  catch X:Y:St ->
      ct:print("~p", [{X, Y, St}])
  end.

year(Suite) ->
  {match, [Year]} = re:run(atom_to_list(Suite), "aoc(\\d+)_.*",
                           [{capture, all_but_first, list}]),
  list_to_integer(Year).

day(TC) ->
  {match, [Day]} = re:run(atom_to_list(TC), "day_(\\d+)",
                          [{capture, all_but_first, list}]),
  list_to_integer(Day).

timings(State) ->
  Timings =
    lists:sort(
      maps:fold(fun({elapsed_tc, Suite, TC}, Elapsed, Acc) ->
                    [{year(Suite), day(TC), Elapsed}|Acc];
                   (_, _, Acc) -> Acc
                end, [], State)),

  MaxTime = lists:max(lists:map(fun({_, _, X}) -> X end, Timings)),

  io_lib:format("~-6s ~-5s ~-12s ~s~n", ["Year", "Day", "Time", "Relative time"]) ++
    lists:map(
      fun({Suite, TC, ElapsedUsecs}) ->
          ElapsedSecs = ElapsedUsecs / 1000000.0,
          Col1 = io_lib:format("~p", [Suite]),
          Col2 = io_lib:format("~p", [TC]),
          Col3 = io_lib:format("~.3f secs", [ElapsedSecs]),
          Col4 = progressbar(ElapsedUsecs, MaxTime),
          io_lib:format("~-6s ~-5s ~-12s ~s~n",
                       [Col1, Col2, Col3, Col4])
      end, Timings).

progressbar(S, Max) ->
  Width = 40,
  Left = trunc((S / Max) * Width),
  Right = Width - Left,

  "[" ++
    io_lib:format("~s~s", [lists:duplicate(Left, $=),
                           lists:duplicate(Right, $ )]) ++
    "]".
