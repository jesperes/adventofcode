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
    , {"07/puzzle07.erl", {puzzle07, start, []}, {956, 40149}}
    , {"08/puzzle08.erl", {puzzle08, start, []}, {1371, 2117}}
    , {"09/puzzle9.erl",  {puzzle9,  start, []}, {251, 898}}
    , {"10/puzzle10.erl", {puzzle10, start, []}, {492982,6989950}}
    , {"11/puzzle11.erl", {puzzle11, start, []}, {"cqjxxyzz", "cqkaabcc"}}
    , {"12/puzzle12.erl", {puzzle12, start, []}, {119433, 68466}}
    , {"13/puzzle13.erl", {puzzle13, start, []}, {709, 668}}
    , {"14/puzzle14.erl", {puzzle14, start, []}, {2655, 1059}}
    , {"15/puzzle15.erl", {puzzle15, start, []}, {222870, 117936}}
    , {"16/puzzle16.erl", {puzzle16, start, []}, {213, 323}}
    , {"17/puzzle17.erl", {puzzle17, start, []}, {1638, 17}}
    , {"18/puzzle18.erl", {puzzle18, start, []}, {768, 781}}
    , {"19/puzzle19.erl", {puzzle19, start, []}, {576, 207}}
    , {"20/puzzle20.erl", {puzzle20, start, []}, {831600, 884520}}
    , {"21/puzzle21.erl", {puzzle21, start, []}, {91, 158}}
    , {"22/puzzle22.erl", {puzzle22, start, []}, {900, 1216}}
    , {"23/puzzle23.erl", {puzzle23, start, []}, {255, 334}}
    , {"24/puzzle24.erl", {puzzle24, start, []}, {11846773891, 80393059}}
    , {"25/puzzle25.erl", {puzzle25, start, []}, 8997277}
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

    io:format("~-10w: ~6w msecs~n", [M, floor(T0 / 1000.0)]).

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
