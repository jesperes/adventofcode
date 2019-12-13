%%% Advent of Code solution for 2019 day 13.
%%% Created: 2019-12-13T06:37:49+00:00

-module(aoc2019_day13).
-include_lib("eunit/include/eunit.hrl").

-define(EMPTY, 0).
-define(WALL, 1).
-define(BLOCK, 2).
-define(PADDLE, 3).
-define(BALL, 4).

%% Define ENABLE_TTY_OUTPUT to be able to call play() to see the game
%% being played on the tty.

%% -define(ENABLE_TTY_OUTPUT, 1).

-ifdef(ENABLE_TTY_OUTPUT).

-export([play/0]).

%% Solve the puzzle and print the grid on stdout at the game is being
%% played.
play() ->
  part2(get_input()).

-endif.

%% Puzzle solution
part1(Prog) ->
  {_, Output} = intcode:execute(Prog),
  count_blocks(lists:reverse(Output), #{}).

count_blocks([], Map) -> maps:size(Map);
count_blocks([X, Y, 2|Rest], Map) ->
  count_blocks(Rest, maps:put({X, Y}, 1, Map));
count_blocks([_, _, _|Rest], Map) ->
  count_blocks(Rest, Map).

%% ---- [ Part 2 ] ----

-record(state, { score = 0
               , output = {}
               , grid = #{}
               , paddle_x = 0 %% x coord of paddle
               , ball_x = 0   %% x coord of ball
               }).

part2(Prog) ->
  Prog0 = maps:merge(Prog, #{0 => 2}),
  InitState = #state{},

  {_, #state{score = Score}} =
    intcode:execute(Prog0, fun joystick/1, fun display/2, InitState),

  Score.


joystick(#state{ball_x = Bx, paddle_x = Px} = State) ->
  {State, case Bx - Px of
            0 -> 0;
            D when D < 0 -> -1;
            _ -> 1
          end}.

%% Receive X coordinate
display(X, #state{output = {}} = State) ->
  State#state{output = {X}};

%% Receive Y coordinate
display(Y, #state{output = {X}} = State) ->
  State#state{output = {X, Y}};

%% Receive score
display(Score, #state{output = {-1, 0}} = State) ->
  State#state{output = {}, score = Score};

%% Receive tile
display(Tile, #state{output = {X, Y}, grid = Grid} = State) ->
  State0 = State#state{grid = maps:put({X, Y}, Tile, Grid),
                       output = {}},
  print_grid(State0),
  case Tile of
    ?BALL -> State0#state{ball_x = X};
    ?PADDLE -> State0#state{paddle_x = X};
    _ -> State0
  end.

-ifdef(ENABLE_TTY_OUTPUT).

print_grid(State) ->
  io:format("~n~s~n~n", [grid_to_str(State)]).

grid_to_str(#state{grid = Map, score = Score}) ->
  {MinX, MaxX} = {-1, lists:max(lists:map(fun({X, _}) -> X end, maps:keys(Map)))},
  {MinY, MaxY} = {0, lists:max(lists:map(fun({_, Y}) -> Y end, maps:keys(Map)))},
  io_lib:format("Score: ~w~n~n", [Score]) ++
    [ [to_str(X, Y, Map) ||
        X <- lists:seq(MinX, MaxX) ] ++ "\n" ||
      Y <- lists:seq(MinY, MaxY) ].

to_str(X, Y, Map) ->
  to_str(maps:get({X, Y}, Map, 0)).

to_str(?EMPTY)  -> 32;
to_str(?WALL)   -> $#;
to_str(?BLOCK)  -> $*;
to_str(?PADDLE) -> $=;
to_str(?BALL)   -> $O.

-else.

print_grid(_) -> ok.

-endif.


%% Input reader (place downloaded input file in
%% priv/inputs/2019/input13.txt).
get_input() ->
  intcode:parse(inputs:get_as_string(2019, 13)).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(236, part1(Input))}
  , {"Part 2", ?_assertEqual(11040, part2(Input))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
