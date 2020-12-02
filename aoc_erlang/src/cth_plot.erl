%% CT hook for producing data suitable for plotting with gnuplot
-module(cth_plot).

-export([ init/2
        , pre_init_per_suite/3
        , pre_init_per_testcase/4
        , post_end_per_testcase/5
        , post_end_per_suite/4
        ]).

ts() ->
   erlang:system_time(microsecond).

init(_Id, _Opts) ->
  {ok, #{}}.

pre_init_per_suite(Suite, Config, State) ->
  DataFile = filename:join(
               proplists:get_value(data_dir, Config),
               io_lib:format("~p.dat", [Suite])),
  file:delete(DataFile),
  filelib:ensure_dir(DataFile),
  {Config, maps:merge(State, #{plotfile => DataFile,
                               count => 0})}.

pre_init_per_testcase(_Suite, _TC, Config, State) ->
  {Config, maps:put(tc_start, ts(), State)}.

post_end_per_testcase(_Suite, TC, _Config, Return, State) ->
  Elapsed = ts() - maps:get(tc_start, State),
  DataFile = maps:get(plotfile, State),
  {ok, IoDevice} = file:open(DataFile, [append]),
  ok = file:write(IoDevice,
                  io_lib:format("\"~s\" ~p~n", [format_tc(TC), Elapsed / 1000000])),
  file:close(IoDevice),
  {Return, maps:update_with(count, fun(Old) -> Old + 1 end, State)}.

post_end_per_suite(_Suite, _Config, Return, State) ->
  {Return, maps:remove(plotfile, State)}.

%% internal

format_tc(A) ->
  string:replace(atom_to_list(A), "_", " ").
