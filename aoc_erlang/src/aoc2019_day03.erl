%%% Advent of Code solution for 2019 day 03.
%%% Created: 2019-12-03T15:32:24+00:00

-module(aoc2019_day03).
-include_lib("eunit/include/eunit.hrl").

%% Solves both part 1 and 2.
part1([W1, W2]) ->
  InitState = { {0, 0} %% Pos
              , #{}    %% Grid ({X, Y} => {FirstWire, CombinedSignalDelay, IsIntersection})
              , 0      %% SignalDelay
              },

  %% The grid is represented as a map where the keys are positions,
  %% and the values are three-tuples: {FirstWire, SignalDelay,
  %% IsIntersection}. Once we have traced the paths of both wires, we
  %% filter out all the values which are intersections.

  Grid0 = trace_wire(W1, 1, InitState),
  Grid = trace_wire(W2, 2, {{0, 0}, Grid0, 0}),

  Xs = lists:filter(fun is_intersection/1, maps:to_list(Grid)),

  Part1 = map_smallest(fun({{X, Y}, _}) ->
                           abs(X) + abs(Y)
                       end, Xs),
  Part2 = map_smallest(fun({_, {_, SignalDelay, _}}) ->
                           SignalDelay
                       end, Xs),
  {Part1, Part2}.

is_intersection({_, {_, _, true}}) -> true;
is_intersection(_) -> false.

%% Map Fun over List and take the smallest element.
map_smallest(Fun, List) ->
  lists:min(lists:map(Fun, List)).

trace_wire([], _WireNum, {_, Grid, _}) -> Grid;
trace_wire([{Dir, Dist}|Wire], WireNum, State) ->
  Delta = delta(Dir),
  %% Trace a single wire segment
  S0 = trace_wire_segment(Delta, Dist, WireNum, State),
  trace_wire(Wire, WireNum, S0).

trace_wire_segment(_, 0, _, State) ->
  State;
trace_wire_segment({Dx, Dy} = Delta,
                   Dist,
                   WireNum,
                   {{X, Y} = _Pos, Grid, SignalDelay} = _State) ->

  NewPos = {X + Dx, Y + Dy},
  NewSignalDelay = SignalDelay + 1,

  NewGrid =
    case maps:is_key(NewPos, Grid) of
      true ->
        case maps:get(NewPos, Grid) of
          {WireNum, _, _} ->
            %% Move to next cell, updating position and signal delay
            Grid;

          {FirstWire, FirstSignalDelay, _} ->
            %% Update signal delay, and mark this cell as an intersection
            CombinedSignalDelay = FirstSignalDelay + NewSignalDelay,
            maps:put(NewPos, {FirstWire, CombinedSignalDelay, true}, Grid)
        end;
      false ->
        %% Empty cell
        maps:put(NewPos, {WireNum, NewSignalDelay, false}, Grid)
    end,

  NewState = {NewPos, NewGrid, NewSignalDelay},
  trace_wire_segment(Delta, Dist - 1, WireNum, NewState).

delta('U') -> {0, -1};
delta('D') -> {0, 1};
delta('L') -> {-1, 0};
delta('R') -> {1, 0}.

%% Input reader (place downloaded input file in
%% priv/inputs/2019/input03.txt).
get_input() ->
  get_input(inputs:get_as_lines(2019, 03)).

get_input(Lines) ->
  lists:map(
    fun(L) ->
        lists:map(fun([C|Int]) ->
                      {list_to_atom([C]), list_to_integer(Int)}
                  end,
                  string:tokens(L, ","))
    end, Lines).


%% Tests
main_test_() ->
  Input = get_input(),
  {"Part 1 & 2",
   ?_assertEqual({627, 13190}, part1(Input))}.

ex_test_() ->
  [ ?_assertEqual({6, 30},
                  part1(get_input(["R8,U5,L5,D3",
                                   "U7,R6,D4,L4"])))
  , ?_assertEqual({159, 610},
                  part1(get_input(["R75,D30,R83,U83,L12,D49,R71,U7,L72",
                                   "U62,R66,U55,R34,D71,R55,D58,R83"])))
  , ?_assertEqual({135, 410},
                  part1(get_input(["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
                                   "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"])))
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
