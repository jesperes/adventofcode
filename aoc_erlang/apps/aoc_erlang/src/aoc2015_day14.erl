-module(aoc2015_day14).

-include_lib("eunit/include/eunit.hrl").

main_test() ->
  Lines = inputs:get_as_lines(2015, 14),
  Input =
    lists:map(
      fun(Line) ->
          [Reindeer,
           "can", "fly", Speed, "km/s", "for", Second, "seconds", "but",
           "then", "must", "rest", "for", Rest, "seconds"] =
            string:tokens(Line, " ,."),
          #{reindeer => list_to_atom(string:lowercase(Reindeer)),
            speed => list_to_integer(Speed),
            fly_time => list_to_integer(Second),
            rest_time => list_to_integer(Rest),
            current_state => rest,
            remaining_time => 0,
            distance => 0}
      end, Lines),

    Seconds = 2503,

  States =
    lists:foldl(fun(_N, Acc) ->
                    A = iterate(Acc),
                    award_points(A)
                end, Input, lists:seq(1, Seconds)),

  ?assertEqual(2655, max_distance(States)),
  ?assertEqual(1059, max_points(States)).

max_distance(States) ->
  lists:foldl(fun(#{distance := Dist}, Max) when Dist > Max ->
                  Dist;
                 (_, Max) ->
                  Max
              end, 0, States).

max_points(States) ->
  lists:foldl(fun(#{points := Pts}, Max) when Pts > Max ->
                  Pts;
                 (_, Max) ->
                  Max
              end, 0, States).

award_points(States) ->
  MaxDist = max_distance(States),
  lists:map(fun(Map) ->
                case maps:get(distance, Map, 0) of
                  MaxDist ->
                    OldPoints = maps:get(points, Map, 0),
                    maps:put(points, OldPoints + 1, Map);
                  _ ->
                    Map
                end
            end, States).

iterate([]) ->
  [];
iterate([Map|Rest]) ->
  Map0 = progress_one_reindeer(Map),
  [Map0|iterate(Rest)].


progress_one_reindeer(#{current_state := rest,
                        speed := Speed,
                        fly_time := FlyTime,
                        distance := Dist,
                        remaining_time := 0} = Map) ->
  Map#{current_state := fly,
       distance := Dist + Speed,
       remaining_time := FlyTime - 1};
progress_one_reindeer(#{current_state := fly,
                        rest_time := RestTime,
                        remaining_time := 0} = Map) ->
  Map#{current_state := rest,
       remaining_time := RestTime - 1};
progress_one_reindeer(#{remaining_time := N,
                        current_state := rest} = Map) ->
  Map#{remaining_time := N - 1};
progress_one_reindeer(#{remaining_time := N,
                        speed := Speed,
                        distance := Dist,
                        current_state := fly} = Map) ->
  Map#{remaining_time := N - 1,
       distance := Dist + Speed}.
