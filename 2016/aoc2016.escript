#!/usr/bin/env escript
%%! +P 10000000
-include_lib("eunit/include/eunit.hrl").

puzzles() ->
    [ {"01/puzzle01.erl", {puzzle01, start, []}, {278, 161}}
    , {"02/puzzle02.erl", {puzzle02, start, []}, {"56983", "8B8B1"}}
    , {"03/puzzle03.erl", {puzzle03, start, []}, {917, 1649}}
    , {"04/puzzle04.erl", {puzzle04, start, []}, {185371, 984}}
      %% , {"11/puzzle11.erl", {puzzle11, start, []}, {"cqjxxyzz", "cqkaabcc"}}
    , {"12/puzzle12.erl", {puzzle12, start, []}, {318003,9227657}}
    , {"13/puzzle13.erl", {puzzle13, start, []}, {86,127}}
    ].

compiler_opts() ->
    [
      report_warnings
    , report_errors
    , nowarn_deprecated_function
    ].

run_puzzle(Root, Ebin, {Src, {M, F, A}, Expected}) ->
    AbsSrc = filename:join(Root, filename:dirname(Src)),
    file:set_cwd(AbsSrc),

    lists:foreach(fun(Erlfile) ->
                          {ok, _} =
                              compile:file(
                                filename:join(AbsSrc, Erlfile),
                                compiler_opts() ++ [{outdir, Ebin}])
                  end, filelib:wildcard("*.erl")),
    {T0, _} =
        timer:tc(
          fun() ->
                  ?assertEqual(Expected, erlang:apply(M, F, A))
          end),

    io:format("~-10w: ~12w secs~n", [M, T0 / (1000000.0)]).

main([]) ->
    Root = filename:absname(filename:dirname(escript:script_name())),
    Ebin = filename:join(Root, "ebin"),
    filelib:ensure_dir(filename:join(Ebin, "x")),
    code:add_patha(Ebin),

    {TotalTime, _} =
        timer:tc(
          fun() ->
                  lists:foreach(fun(Erlfile) ->
                                        {ok, _} =
                                            compile:file(
                                              Erlfile,
                                              compiler_opts() ++ [{outdir, Ebin}])
                                end, filelib:wildcard(
                                       filename:join(Root, "../utils/erlang/*.erl"))),

                  lists:foreach(fun(P) ->
                                        run_puzzle(Root, Ebin, P)
                                end, puzzles())
          end),


    io:format("Total time ~w seconds (~w msecs/puzzle)~n",
              [floor(TotalTime / 1000000),
               floor((TotalTime / 1000) / length(puzzles()))]).
