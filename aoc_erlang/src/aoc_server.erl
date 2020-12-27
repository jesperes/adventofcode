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
-export([ start_link/0
        , solve/1
        , report_progress/3
        , get_puzzle_id/1
        ]).

%% Internal exports
-export([ solve_puzzle/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, { workers = []
               , puzzles = #{}
               }).

-type step() :: parse |
                part1 |
                part2.

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

solve(Puzzles) ->
  gen_server:cast(?SERVER, {solve, Puzzles}).

-spec report_progress(Step :: step(),
                      Time :: integer(),
                      PuzzleId :: aoc_puzzle_id()) -> ok.
report_progress(Step, Time, PuzzleId) ->
  gen_server:cast(?SERVER, {report_progress, Step, Time, PuzzleId}).

-spec get_puzzle_id(Puzzle :: aoc_puzzle()) -> aoc_puzzle_id().
get_puzzle_id(#aoc_puzzle{year = Year, day = Day}) ->
  {Year, Day}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{}}.

handle_call(wait, _From, State) ->
  {reply, ok, State}.

handle_cast({solve, Puzzles}, State) ->
  StateOut =
    lists:foldl(
      fun(#aoc_puzzle{year = Year, day = Day} = Puzzle, State0) ->
          Pid = spawn_link(?MODULE, solve_puzzle, [Puzzle]),
          io:format("Spawned worker process: ~p~n", [Pid]),
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

  io:format("~p ~p: ~p~n", [PuzzleId, Step, Time]),

  State0 = State#state{puzzles = maps:update(PuzzleId, Puzzle0, Puzzles)},
  {noreply, State0}.

handle_info({'EXIT', Pid, _Reason}, State) ->
  Workers = State#state.workers,
  true = lists:member(Pid, Workers),
  StateOut = State#state{workers = lists:delete(Pid, Workers)},

  %% Terminate when all workers are done
  case StateOut#state.workers of
    [] ->
      io:format("All workers are done, terminating.~n", []),
      {stop, normal, ok};
    _ ->
      {noreply, StateOut}
  end.

terminate(_Reason, _State) ->
  io:format("~p terminating.~n", [?SERVER]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

format_status(_Opt, Status) ->
  Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_input(#aoc_puzzle{year = Year, day = Day}) ->
  Filename =
    lists:flatten(
      io_lib:format("~s/inputs/~w/input~2..0w.txt",
                    [code:priv_dir(aoc_erlang), Year, Day])),
  file:read_file(Filename).

run_step_with_progress(#aoc_puzzle{module = M} = Puzzle, Step, F, A) ->
  {T, V} = timer:tc(M, F, A),
  report_progress(Step, T, get_puzzle_id(Puzzle)),
  V.

solve_puzzle(Puzzle) ->
  M = Puzzle#aoc_puzzle.module,

  case get_input(Puzzle) of
    {ok, Binary} ->
      io:format("Parsing...~n", []),

      ParsedInput = run_step_with_progress(Puzzle, parse, parse, [Binary]),
      _Part1 = run_step_with_progress(Puzzle, part1, solve1, [ParsedInput]),
      _Part2 = run_step_with_progress(Puzzle, part2, solve2, [ParsedInput]);
    {error, enoent} ->
      io:format("No input file found for ~p~n", [M]),
      ok = M:solve(<<>>)
  end.
