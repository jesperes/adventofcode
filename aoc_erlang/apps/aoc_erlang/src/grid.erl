%% Utilities to handle grids represented as a map of {X,Y} -> Data.
-module(grid).
-export([ to_str/2
        , to_str/3
        ]).

%% Converts Grid to a list suitable for printing using "~s" format.
%% Automatically detects coordinate ranges, and can handle negative
%% coordinate values. Y grows downwards.
%%
%% @param Grid  a map containing {X, Y} keys.
%% @param Fun   a function mapping values in Grid to characters.
%%              Coordinates which do not exist in the map are shown
%%              as 32 (space).
-spec to_str(Grid :: map(),
             Fun :: fun((Char :: integer()) -> integer())) ->
                string().
to_str(Grid, Fun) ->
  to_str(Grid, Fun, []).

to_str(Grid, Fun, Overlays) ->
  Coords = lists:filter(fun({X, Y}) when is_integer(X) andalso is_integer(Y) ->
                            true;
                           (_) -> false
                        end, maps:keys(Grid)),

  XCoords = lists:map(fun key_to_x/1, Coords),
  YCoords = lists:map(fun key_to_y/1, Coords),

  if length(XCoords) > 0 andalso length(YCoords) > 0 ->
      MinX = lists:min(XCoords),
      MinY = lists:min(YCoords),
      MaxX = lists:max(XCoords),
      MaxY = lists:max(YCoords),
      %% io:format("Grid range {X:~p-~p, Y:~p-~p}~n",
      %%           [MinX, MaxX, MinY, MaxY]),
      [[
        begin
          case lists:filtermap(fun({K, V}) ->
                                   case maps:get(K, Grid, false) =:= {X, Y} of
                                     true -> {true, V};
                                     false -> false
                                   end
                               end, Overlays) of
            [O|_] -> O;
            [] ->
              case maps:get({X, Y}, Grid, undef) of
                undef -> 32;
                C -> Fun(C)
              end
          end
        end ||
         X <- lists:seq(MinX, MaxX)] ++ "\n" ||
        Y <- lists:seq(MinY, MaxY)];
     true ->
      "<grid is empty>"
  end.

key_to_x({X, _}) -> X.
key_to_y({_, Y}) -> Y.
