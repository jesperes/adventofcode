%%% Advent of Code solution for 2019 day 12.
%%% Created: 2019-12-12T05:22:58+00:00

-module(aoc2019_day12).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Bodies, Steps) ->
  FinalBodies = nbodies(Bodies, Steps),
  energy(FinalBodies).

nbodies(Bodies, 0) -> Bodies;
nbodies(Bodies, N) ->
  nbodies(do_one_step(Bodies), N - 1).

do_one_step(Bodies) ->
  [velocity(B) || B <- apply_gravity(Bodies)].

apply_gravity(Bodies) ->
  [lists:foldl(fun(A, A) -> A;
                  (Other, Acc) -> gravity(Acc, Other)
               end, B, Bodies) || B <- Bodies].

gravity({{X, Y, Z} = Pos, {Dx, Dy, Dz}}, {{Xo, Yo, Zo}, _}) ->
  {Pos, {Dx + sign(Xo - X),
         Dy + sign(Yo - Y),
         Dz + sign(Zo - Z)}}.

velocity({{X, Y, Z}, {Dx, Dy, Dz} = D}) ->
  {{X + Dx, Y + Dy, Z + Dz}, D}.

sign(X) when X < 0 -> -1;
sign(X) when X == 0 -> 0;
sign(_) -> 1.

energy(Bodies) ->
  lists:foldl(fun({{X, Y, Z}, {Dx, Dy, Dz}}, Acc) ->
                  PE = abs(X) + abs(Y) + abs(Z),
                  KE = abs(Dx) + abs(Dy) + abs(Dz),
                  Acc + PE * KE
              end, 0, Bodies).

%% Part 2
part2(Bodies) ->
  States = [sets:new(),
            sets:new(),
            sets:new()],

  run2(Bodies, States, 0).


run2(Bodies, States, N) ->
  States0 = check_cycles(Bodies, States, N),

  case lists:all(fun is_integer/1, States0) of
    true -> lists:foldl(fun lcm/2, 1, States0);
    false ->
      Bodies0 = do_one_step(Bodies),
      run2(Bodies0, States0, N + 1)
  end.

%% Since the coordinates are independent, we can detect cycles in each
%% coordinate individually, then use LCM to compute the period of the
%% entire system.
check_cycles(Bodies, [Sx, Sy, Sz], N) ->
  Xcoords = {lists:map(fun({{X, _, _}, _}) -> X end, Bodies),
             lists:map(fun({_, {Dx, _, _}}) -> Dx end, Bodies)},
  Sx0 = update_cycle(Xcoords, Sx, N),

  Ycoords = {lists:map(fun({{_, Y, _}, _}) -> Y end, Bodies),
             lists:map(fun({_, {_, Dy, _}}) -> Dy end, Bodies)},
  Sy0 = update_cycle(Ycoords, Sy, N),

  Zcoords = {lists:map(fun({{_, _, Z}, _}) -> Z end, Bodies),
             lists:map(fun({_, {_, _, Dz}}) -> Dz end, Bodies)},
  Sz0 = update_cycle(Zcoords, Sz, N),

  [Sx0, Sy0, Sz0].

update_cycle(Coords, Set, N) ->
  case sets:is_set(Set) of
    false -> Set;
    true ->
      case sets:is_element(Coords, Set) of
        true -> N;
        false ->
          sets:add_element(Coords, Set)
      end
  end.

gcd(A,B) when A == 0; B == 0 -> 0;
gcd(A,B) when A == B -> A;
gcd(A,B) when A > B -> gcd(A-B, B);
gcd(A,B) -> gcd(A, B-A).

lcm(A,B) -> (A*B) div gcd(A, B).

%% Puzzle input
get_input() ->
  [ {{10, 15, 7}, {0, 0, 0}}
  , {{15, 10, 0}, {0, 0, 0}}
  , {{20, 12, 3}, {0, 0, 0}}
  , {{0,  -3, 13}, {0, 0, 0}}
  ].

%% Tests
main_test_() ->
  Input = get_input(),
  [ {"Part 1", ?_assertEqual(8362, part1(Input, 1000))}
  , {"Part 2", timeout, 60, ?_assertEqual(0, part2(Input))}
  ].

ex1_test_() ->
  Input =
    [ {{-1, 0, 2}, {0, 0, 0}}
    , {{2, -10, -7}, {0, 0, 0}}
    , {{4, -8, 8}, {0, 0, 0}}
    , {{3, 5, -1}, {0, 0, 0}}
    ],

  [ ?_assertEqual(179, part1(Input, 10))
  , ?_assertEqual(2772, part2(Input))
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
