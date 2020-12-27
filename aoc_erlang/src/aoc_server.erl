%%%-------------------------------------------------------------------
%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2020, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 22 Dec 2020 by Jesper Eskilson <jesper.eskilson@klarna.com>
%%%-------------------------------------------------------------------
-module(aoc_server).

-behaviour(gen_server).

-include("aoc_puzzle.hrl").

%% API
-export([ start_link/2
        , solve/1
        , report_progress/3
        , report_result/3
        , get_puzzle_id/1
        ]).

%% Internal exports
-export([ solve_puzzle/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, { workers = []   :: [pid()]
               , input_file_dir :: string()
               , display_module :: module()
               , puzzles = #{}  :: #{aoc_puzzle_id() => aoc_puzzle()}
               }).

-type step() :: parse |
                part1 |
                part2 |
                part1_result |
                part2_result.

%%%===================================================================
%%% API
%%%===================================================================

start_link(InputFileDir, DisplayModule) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [InputFileDir, DisplayModule], []).

solve(Puzzles) ->
  gen_server:cast(?SERVER, {solve, Puzzles}).

-spec report_progress(Step :: step(),
                      Time :: integer(),
                      PuzzleId :: aoc_puzzle_id()) -> ok.
report_progress(Step, Time, PuzzleId) ->
  gen_server:cast(?SERVER, {report_progress, Step, Time, PuzzleId}).


-spec report_result(Part :: part1 | part2,
                    Result :: term(),
                    PuzzleId :: aoc_puzzle_id()) ->
        ok.
report_result(Part, Result, PuzzleId) ->
  gen_server:cast(?SERVER, {report_result, Part, Result, PuzzleId}).

-spec get_puzzle_id(Puzzle :: aoc_puzzle()) -> aoc_puzzle_id().
get_puzzle_id(#aoc_puzzle{year = Year, day = Day}) ->
  {Year, Day}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([InputFileDir, DisplayModule]) ->
  process_flag(trap_exit, true),
  {ok, #state{input_file_dir = InputFileDir,
              display_module = DisplayModule}}.

handle_call(wait, _From, State) ->
  {reply, ok, State}.

handle_cast({solve, Puzzles}, #state{input_file_dir = InputFileDir,
                                     display_module = DisplayModule} = State) ->
  DisplayModule:init(Puzzles),
  StateOut =
    lists:foldl(
      fun(#aoc_puzzle{year = Year, day = Day} = Puzzle, State0) ->
          Pid = spawn_link(?MODULE, solve_puzzle, [Puzzle, InputFileDir]),
          Workers = State0#state.workers,
          State0#state{workers = [Pid|Workers],
                       puzzles = maps:put({Year, Day},
                                          Puzzle,
                                          State0#state.puzzles)}
      end, State, Puzzles),
  {noreply, StateOut};
handle_cast({report_progress, Step, Time, PuzzleId},
             #state{puzzles = Puzzles} = State) ->
  Puzzle = maps:get(PuzzleId, State#state.puzzles),
  Puzzle0 =
    case Step of
      parse -> Puzzle#aoc_puzzle{parse = Time};
      part1 -> Puzzle#aoc_puzzle{part1 = Time};
      part2 -> Puzzle#aoc_puzzle{part2 = Time}
    end,

  (State#state.display_module):update_step_time(PuzzleId, Step, Time),

  State0 = State#state{puzzles = maps:update(PuzzleId, Puzzle0, Puzzles)},
  {noreply, State0};
handle_cast({report_result, Part, Result, PuzzleId},
            #state{puzzles = Puzzles} = State) ->
  Puzzle = maps:get(PuzzleId, State#state.puzzles),
  Puzzle0 =
    case Part of
      part1 -> Puzzle#aoc_puzzle{part1_result = Result};
      part2 -> Puzzle#aoc_puzzle{part2_result = Result}
    end,

  (State#state.display_module):update_part_result(PuzzleId, Part, Result),
  State0 = State#state{puzzles = maps:update(PuzzleId, Puzzle0, Puzzles)},
  {noreply, State0}.

handle_info({'EXIT', Pid, _Reason}, #state{puzzles = Puzzles} = State) ->
  Workers = State#state.workers,
  true = lists:member(Pid, Workers),
  StateOut = State#state{workers = lists:delete(Pid, Workers)},

  %% Terminate when all workers are done
  case StateOut#state.workers of
    [] ->
      (State#state.display_module):summarize(Puzzles),
      {stop, normal, ok};
    _ ->
      {noreply, StateOut}
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

format_status(_Opt, Status) ->
  Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_input(#aoc_puzzle{year = Year, day = Day, has_input_file = HasInput}, InputFileDir) ->
  Filename =
    lists:flatten(
      io_lib:format("~s/inputs/~w/input~2..0w.txt",
                    [InputFileDir, Year, Day])),

  case HasInput of
    true -> {ok, Binary} = file:read_file(Filename), Binary;
    false -> <<>>
  end.

run_step_with_progress(#aoc_puzzle{module = M} = Puzzle, Step, F, A) ->
  {T, V} = timer:tc(M, F, A),
  report_progress(Step, T, get_puzzle_id(Puzzle)),
  V.

solve_puzzle(Puzzle, InputFileDir) ->
  PuzzleId = get_puzzle_id(Puzzle),
  Binary = get_input(Puzzle, InputFileDir),
  ParsedInput = run_step_with_progress(Puzzle, parse, parse, [Binary]),

  try
    Part1 = run_step_with_progress(Puzzle, part1, solve1, [ParsedInput]),
    report_result(part1, Part1, PuzzleId),
    Part2 = run_step_with_progress(Puzzle, part2, solve2, [ParsedInput]),
    report_result(part2, Part2, PuzzleId)
  catch T:E:St ->
      io:format("Failed to run puzzle ~p: ~p~n~p~n", [PuzzleId, {T, E}, St]),
      io:format("Parsed input: ~p~n", [ParsedInput])
  end.
