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

  List = lists:sort(
           lists:map(fun({TC, Elapsed}) ->
                         {-Elapsed, TC}
                     end, maps:to_list(ElapsedTC))),
  {First10, _} = lists:split(min(N, length(List)), List),
  io_lib:format("~w slowest tests~n", [N]) ++
    lists:map(
      fun({X, Y}) ->
          io_lib:format("~p: ~p secs~n",
                        [Y, -X/1000000])
      end, First10).
