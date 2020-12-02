-module(aoc_nifs).

-export([ digest_to_hexstring/1
        , string_int_md5/2
        , init/0
        ]).

-on_load(init/0).

%% Keep LSP happy; it cannot find the NIF_DIR define which is in rebar.config.
-ifndef(NIF_DIR).
-define(NIF_DIR, ".").
-endif.

%% Load NIF
init() ->
  File = filename:join(?NIF_DIR, "aoc_nifs.so"),
  ok = erlang:load_nif(filename:rootname(File), 0).

-spec digest_to_hexstring(Binary :: binary()) -> binary().
digest_to_hexstring(_Binary) ->
  erlang:nif_error(not_loaded).


-spec string_int_md5(Binary :: binary(),
                     Num :: integer()) ->
                        binary().
string_int_md5(_Binary, _Num) ->
  erlang:nif_error(not_loaded).
