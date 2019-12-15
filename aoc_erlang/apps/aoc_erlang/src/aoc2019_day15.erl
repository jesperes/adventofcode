%%% Advent of Code solution for 2019 day 15.
%%% Created: 2019-12-15T10:32:09+00:00

-module(aoc2019_day15).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Prog) ->
  Pid = start_intcode(Prog),
  try
    explore(Pid, #{pos => {0, 0}, dist => 0})
  catch throw:{target, _, Steps} ->
      Steps
  end.

to_str(C) -> C.

start_intcode(Prog) ->
  Parent = self(),
  spawn(fun() ->
            intcode:execute(
              Prog,
              fun(State) ->
                  {State, receive Input -> Input end}
              end,
              fun(Output, State) ->
                  State ! Output, State
              end,
              Parent)
        end).

step_repair_robot(Dir, IntCodePid) ->
  IntCodePid ! Dir,
  receive Output -> Output end.

explore(Pid, Grid0) ->
  lists:foldl(
    fun(Dir, #{pos := Pos, dist := Dist} = Grid) ->
        NewPos = move(Dir, Pos),
        NewDist = Dist + 1,
        case maps:is_key(NewPos, Grid) of
          true -> Grid;
          false ->
            case step_repair_robot(Dir, Pid) of
              0 -> maps:put(NewPos, $#, Grid); %% Wall
              1 ->
                %% New pos is space. Robot was moved.
                NewGrid =
                  explore(Pid, maps:merge(Grid, #{NewPos => $.,
                                                  dist => NewDist,
                                                  pos => NewPos})),

                %% We must move the robot back after exploring; it has
                %% its own state in the intcode machine and does not
                %% automatically rewind with our stack.
                ?assertEqual(1, step_repair_robot(opposite(Dir), Pid)),
                maps:merge(NewGrid, #{pos => Pos, dist => Dist});
              2 ->
                %% Found target
                throw({target, NewPos, NewDist})
            end
        end
    end, Grid0, [1, 2, 3, 4]).

move(1, {X, Y}) -> {X, Y - 1}; %% north
move(2, {X, Y}) -> {X, Y + 1}; %% south
move(3, {X, Y}) -> {X - 1, Y}; %% west
move(4, {X, Y}) -> {X + 1, Y}. %% east

opposite(1) -> 2;
opposite(2) -> 1;
opposite(3) -> 4;
opposite(4) -> 3.

part2(_Input) ->
  %% ?debugMsg("Not implemented."),
  0.

get_input() ->
  intcode:parse(inputs:get_as_binary(2019, 15)).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", timeout, 60, ?_assertEqual(214, part1(Input))}
  , {"Part 2", ?_assertEqual(0, part2(Input))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
