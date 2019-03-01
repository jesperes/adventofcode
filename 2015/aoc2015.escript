#!/usr/bin/env escript

-include_lib("eunit/include/eunit.hrl").

puzzles() ->
    [ {"01/puzzle01.erl", {puzzle01, start, []}, {232, 1783}}
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
              
              io:format("~-10w: ~w msecs~n", [M, T0 / 1000.0])
      end, puzzles()).
