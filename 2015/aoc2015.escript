#!/usr/bin/env escript
%%! +P 10000000
-include_lib("eunit/include/eunit.hrl").

puzzles() ->
    [ {"01/puzzle01.erl", {puzzle01, start, []}, {232, 1783}}
    , {"02/puzzle02.erl", {puzzle02, start, []}, {1586300, 3737498}}
    , {"03/puzzle03.erl", {puzzle03, start, []}, {2572, 2631}}
    , {"04/puzzle04.erl", {puzzle04, start, []}, {282749, 9962624}}
    , {"05/puzzle05.erl", {puzzle05, start, []}, {238, 69}}
    , {"06/puzzle06.erl", {puzzle06, start, []}, {543903, 14687245}}
    , {"06/puzzle06b.erl", {puzzle06b, start, []}, {543903, 14687245}}
    , {"08/puzzle08.erl", {puzzle08, start, []}, {1371, 2117}}
    , {"09/puzzle9.erl",  {puzzle9,  start, []}, {251, 898}}
    , {"10/puzzle10.erl", {puzzle10, start, []}, {492982,6989950}}
    , {"11/puzzle11.erl", {puzzle11, start, []}, {"cqjxxyzz", "cqkaabcc"}}
    ].
    
compiler_opts() ->
    [verbose,
     report_warnings,
     report_errors].

main([]) ->
    Root = filename:absname(filename:dirname(escript:script_name())),
    Ebin = filename:join(Root, "ebin"),
    filelib:ensure_dir(filename:join(Ebin, "x")),
    code:add_patha(Ebin),

    lists:foreach(
      fun({Src, {M, F, A}, Expected}) ->
              AbsSrc = filename:join(Root, filename:dirname(Src)),
              file:set_cwd(AbsSrc),
              {ok, M} = 
                  compile:file(
                    filename:join(Root, Src),
                    compiler_opts() ++ [{outdir, Ebin}]),
              {T0, _} = 
                  timer:tc(
                    fun() ->
                            ?assertEqual(Expected, erlang:apply(M, F, A))
                    end),
              
              io:format("~-10w: ~5w msecs~n", [M, floor(T0 / 1000.0)])
      end, puzzles()).
