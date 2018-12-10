-module(puzzle10).
-export([start1/0]).

start1() ->
    Points = input(),
    do_steps(Points, 0).

input() ->
    {ok, Binary} = file:read_file("input.txt"),
    Str = binary_to_list(Binary),
    Points =
        lists:map(fun(Line) ->
                          ["position", X, Y, "velocity", Dx, Dy] = 
                              string:tokens(Line, "=<, >"),
                          {{to_i(X),to_i(Y)},{to_i(Dx),to_i(Dy)}}
                  end, string:tokens(Str, "\n")),

    lists:foldl(
      fun({Pos,Vel}, Map) ->
              add_point_at(Pos, Vel, Map)
      end, maps:new(), Points).

to_i(Str) ->
    list_to_integer(Str).

do_steps(Points, Sec) ->
    P1 = step_points(Points),
    W1 = get_width(Points),
    W2 = get_width(P1),
    %% When the width of the bounding box starts to expand, we are
    %% done. This assumes that all points converge to form the
    %% message, and the message appear when the points are closest
    %% together.
    IsExpanding = abs(W2) > abs(W1),
    if IsExpanding ->
            io:format("Found message after ~w seconds.~n", [Sec]),
            print_display(Points);
       true ->
            do_steps(P1, Sec + 1)
    end.

get_width(P) ->
    {MinX,MaxX,_,_} = get_bounds(P),
    MaxX - MinX.

add_point_at(Pos, Vel, Map) ->
    maps:update_with(
      Pos,
      fun(V) ->
              [Vel|V]
      end, [Vel], Map).

getmax(X,undef) ->
    X;
getmax(X,Max) when X > Max ->
    X;
getmax(_,Max) ->
    Max.

getmin(X,undef) ->
    X;
getmin(X,Min) when X < Min ->
    X;
getmin(_,Min) ->
    Min.

get_bounds(Points) ->
    maps:fold(fun({X,Y},_,{MaxX,MinX,MaxY,MinY}) ->
                      {getmax(X,MaxX),
                       getmin(X,MinX),
                       getmax(Y,MaxY),
                       getmin(Y,MinY)}
              end, {undef, undef, undef, undef}, Points).

display(Points) ->
    {MaxX,MinX,MaxY,MinY} = get_bounds(Points),
    [[ get_point_at(Points, {X, Y}) || X <- lists:seq(MinX,MaxX)] 
     || Y <- lists:seq(MinY,MaxY)].

print_display(Points) ->
    D = display(Points),
    lists:foreach(
      fun(Line) ->
              lists:foreach(
                fun(Row) ->
                        io:format("~s", [lists:join("", Row)])
                end, Line),
              io:nl()
      end, D).
    
get_point_at(Points, Pos) ->
    case maps:is_key(Pos, Points) of
        true ->
            "#";
        _ ->
            "."
    end.
            
step_points(Points) ->
    maps:fold(
      fun({X,Y}, Velocities, Map) ->
              lists:foldl(fun({Dx,Dy} = Vel, M) ->
                                  NewPos = {X + Dx, Y + Dy},
                                  add_point_at(NewPos, Vel, M)
                          end, Map, Velocities)
      end, maps:new(), Points).
