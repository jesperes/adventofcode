-module(puzzle06b).
-export([start/0]).

%%
%% Silly version of puzzle 6 where we spawn an Erlang process for each
%% light, then send messages for each toggle/on/off instruction. This
%% is actually faster than the naive solution in puzzle06.erl.
%%
%% Note that you need to increase Erlang's process limit to run this,
%% e.g.  "erl +P 10000000".
%%

start() ->
    {ok, Bin} = file:read_file("input.txt"),
    Lines = string:tokens(binary_to_list(Bin), "\n"),
    Instrs = 
        lists:map(
          fun(Line) ->
                  case string:tokens(Line, " ,") of
                      ["toggle", X0, Y0, "through", X1, Y1] ->
                          {toggle, {toi(X0), toi(Y0)}, {toi(X1), toi(Y1)}};
                      ["turn", "on", X0, Y0, "through", X1, Y1] ->
                          {turn_on, {toi(X0), toi(Y0)}, {toi(X1), toi(Y1)}};
                      ["turn", "off", X0, Y0, "through", X1, Y1] ->
                          {turn_off, {toi(X0), toi(Y0)}, {toi(X1), toi(Y1)}}
                  end
          end, Lines),
    
    Parent = self(),

    %% Spawn a process for each light
    Pids = 
        lists:foldl(
          fun(Pos, Map) ->
                  maps:put(Pos,
                           spawn(
                             fun() -> 
                                     light(#{parent => Parent, 
                                             pos => Pos,
                                             state => false,
                                             brightness => 0
                                            }) 
                             end),
                           Map)
                    end, 
                    #{},
                    [{X, Y} || X <- lists:seq(0, 999),
                               Y <- lists:seq(0, 999)]),

    %% Traverse instructions and send on/off/toggle messages to the
    %% lights
    lists:foreach(
      fun({What, {X0, Y0}, {X1, Y1}}) ->
              lists:foreach(
                fun(Pos) ->
                        maps:get(Pos, Pids) ! What
                end, [{X, Y} || X <- lists:seq(X0, X1),
                                Y <- lists:seq(Y0, Y1)])
      end, Instrs),

    %% Send 'done' message and wait for results.
    lists:foreach(
      fun(Pos) ->
              maps:get(Pos, Pids) ! done
      end, maps:keys(Pids)),

    Result = wait_for_result(Pids, #{}),

    maps:fold(fun(_, {true, Brightness}, {S, B}) ->
                      {S + 1, Brightness + B};
                 (_, {false, Brightness}, {S, B}) ->
                      {S, Brightness + B}
              end, {0, 0}, Result).
                             
%% The light process
light(#{state := State, 
        brightness := Brightness, 
        pos := Pos,
        parent := Parent} = Map) ->    
    receive
        toggle ->
            light(Map#{state => not State,
                       brightness => Brightness + 2});
        turn_on ->
            light(Map#{state => true,
                       brightness => Brightness + 1});
        turn_off ->
            light(Map#{state => false,
                       brightness => max(Brightness - 1, 0)});
        done ->
            Parent ! {self(), Pos, State, Brightness}
    end.
        
wait_for_result(Pids, Acc) ->
    case maps:size(Pids) of
        0 -> Acc;
        _ -> 
            receive 
                {_Pid, Pos, State, Brightness} ->
                    Acc0 = maps:put(Pos, {State, Brightness}, Acc),
                    Pids0 = maps:remove(Pos, Pids),
                    wait_for_result(Pids0, Acc0)
            end
    end.

toi(N) -> list_to_integer(N).

     



