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

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, { puzzles = [] :: [aoc_puzzle()]
               }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

solve(Puzzles) ->
  gen_server:call(?SERVER, {solve, Puzzles}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{}}.

handle_call({solve, Puzzles}, _From, State) ->
  io:format("Solving puzzles: ~p~n", [Puzzles]),
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

format_status(_Opt, Status) ->
  Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
