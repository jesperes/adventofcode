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
        ]).

%% Internal exports
-export([ solve_puzzle/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, { workers = []
               }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

solve(Puzzles) ->
  gen_server:cast(?SERVER, {solve, Puzzles}).

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
      fun(Puzzle, State0) ->
          Pid = spawn_link(?MODULE, solve_puzzle, [Puzzle]),
          io:format("Spawned worker process: ~p~n", [Pid]),
          Workers = State0#state.workers,
          State0#state{workers = [Pid|Workers]}
      end, State, Puzzles),
  {noreply, StateOut}.

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

solve_puzzle(Puzzle) ->
  M = Puzzle#aoc_puzzle.module,

  case get_input(Puzzle) of
    {ok, Binary} ->
      io:format("Parsing...~n", []),
      ParsedInput = M:parse(Binary),
      io:format("Solving...~n", []),
      ok = M:solve(ParsedInput);
    {error, enoent} ->
      io:format("No input file found for ~p~n", [M]),
      ok = M:solve(<<>>)
  end.
