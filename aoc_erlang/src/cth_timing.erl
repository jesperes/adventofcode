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
    ct:print("~s", [slowest(State, 10)])
  catch X:Y:St ->
      ct:print("~p", [{X, Y, St}])
  end.

slowest(State, N) ->
  ElapsedTC =
    maps:fold(fun({elapsed_tc, Suite, TC}, Elapsed, Acc) ->
                  maps:put({Suite, TC}, Elapsed, Acc);
                 (_, _, Acc) -> Acc
              end, #{}, State),

  MaxTime = lists:max(maps:values(ElapsedTC)),



  List = lists:sort(
           lists:map(fun({TC, Elapsed}) ->
                         {-Elapsed, TC}
                     end, maps:to_list(ElapsedTC))),
  {First10, _} = lists:split(min(N, length(List)), List),
  io_lib:format("~w slowest tests~n", [N]) ++
    lists:map(
      fun({X, {Suite, TC}}) ->
          ElapsedUsecs = -X,
          ElapsedSecs = ElapsedUsecs / 1000000.0,
          Col1 = io_lib:format("~p", [Suite]),
          Col2 = io_lib:format("~p", [TC]),
          Col3 = io_lib:format("~p secs", [ElapsedSecs]),
          Col4 = progressbar(ElapsedUsecs, MaxTime),
          io_lib:format("~-18s ~-8s ~12s ~s~n", [Col1, Col2, Col3, Col4])
      end, First10).

progressbar(S, Max) ->
  Width = 40,
  Left = trunc((S / Max) * Width),
  Right = Width - Left,

  "[" ++
    io_lib:format("~s~s", [lists:duplicate(Left, $=),
                           lists:duplicate(Right, $ )]) ++
    "]".
