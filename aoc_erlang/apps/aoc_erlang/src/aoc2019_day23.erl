%%% Advent of Code solution for 2019 day 23.
%%% Created: 2019-12-23T06:13:41+00:00

-module(aoc2019_day23).
-include_lib("eunit/include/eunit.hrl").

-record(nic, { addr :: integer()    % Network address
             , booting :: boolean() % True when booting, false otherwise
             , parent :: pid()      % Parent pid
             , outseq :: integer()  % Next output is: 0 = addr, 1 = x, 2 = y
             , outaddr :: integer() % Address for next output package
             , outx :: integer()    % Buffered outgoing X value
             , iny :: integer()     % Buffered incoming Y value
             , idle :: integer()
             }).

-record(nat, { packet :: {integer(), integer()} % Last packet seen by NAT
             , idle :: term()                   % Set of idle pids
             , parent :: pid()                  % Parent pid
             , solutions :: term()
             , first :: integer()
             }).

part1(Prog) ->
  N = 50,
  Addrs = lists:seq(0, N-1),
  spawn_nics(Prog, Addrs),
  spawn_nat(),
  wait_for_nic_boot(Addrs),
  start_nics(Addrs),
  Res =
    receive
      Result -> Result
    end,
  kill_all([255|Addrs]),
  Res.

kill_all(L) ->
  lists:foreach(fun(N) ->
                    case nic_pid(N) of
                      undefined -> ok;
                      Pid -> exit(Pid, normal)
                    end
                end, L).

nic(Prog, Addr, Parent) ->
  register(nic_name(Addr), self()),
  {_ProgOut, NicState} =
    intcode:execute(Prog,
                    fun nic_input/1,
                    fun nic_output/2,
                    #nic{ addr = Addr
                        , booting = true
                        , parent = Parent
                        , outseq = 0
                        }),
  ?debugFmt("NIC terminated, state = ~p", [NicState]).

spawn_nat() ->
  Parent = self(),
  Pid = spawn(fun() ->
                  nat(Parent)
              end),
  receive {nat_start, Pid} -> ok end,
  Pid.

nat(Parent) ->
  register(nic_name(255), self()),
  Parent ! {nat_start, self()},
  ?debugFmt("Starting NAT...", []),
  nat_loop(#nat{parent = Parent,
                solutions = #{},
                first = undef,
                idle = sets:new()}),
  ?debugFmt("NAT terminated.", []).


check_mql() ->
  case process_info(self(), message_queue_len) of
    {message_queue_len, L} when L > 100 ->
      erlang:display({mql, L});
    _ ->
      ok
  end.

is_network_idle() ->
  %% erlang:display(checking_network_idle),
  lists:foldl(fun(_, false) ->
                  false;
                 (N, true) ->
                  Pid = nic_pid(N),
                  Pid ! {is_idle, self()},
                  receive
                    {idle, Pid, IsIdle} -> IsIdle
                  end
              end, true, lists:seq(0, 49)).


nat_loop(#nat{ parent = Parent
             , solutions = Solutions
             , idle = Idle
             , first = First
             , packet = Packet} = State) ->

  %% erlang:display({nat_loop, maps:keys(Solutions)}),
  check_mql(),

  %% Too high: (You guessed 22393.)
  %% Too high: (You guessed 19327.)

  receive
    P ->
      %% erlang:display({nat_receive, P}),
      nat_loop(State#nat{packet = P})
  after 10 ->
      case is_tuple(Packet) andalso is_network_idle() of
        false ->
          nat_loop(State);
        true ->
          %% erlang:display(network_idle),
          {_, Y} = Packet,

          %% Restart computers
          nic_pid(0) ! Packet,

          %% Store part 1 solution
          State0 = case First of
                     undef -> State#nat{first = Y};
                     _ -> State
                   end,

          case maps:is_key(Y, Solutions) of
            true ->
              %% We are done, send results back
              Parent ! {First, Y};
            _ ->
              nat_loop(State0#nat{ solutions = maps:put(Y, true, Solutions)
                                 , packet = undef
                                 })
          end
      end
  end.

  %% State0 =
  %%   case sets:size(Idle) of
  %%     Size when (Size == 50) and is_tuple(Packet) ->
  %%       erlang:display({network_idle, Packet}),

  %%       nic_pid(0) ! Packet,
  %%       {_, Y} = Packet,

  %%       case sets:is_element(Y, Solutions) of
  %%         true ->
  %%           erlang:display({part2, Y}),
  %%           Parent ! {First, Y},
  %%           State;
  %%         false ->
  %%           State#nat{ idle = sets:new()
  %%                    , packet = undef
  %%                    , solutions = sets:add_element(Y, Solutions)
  %%                    }
  %%       end;
  %%     _ ->
  %%       State
  %%   end,

  %% receive
  %%   {_, Y0} = Other ->
  %%     nat_loop(
  %%       case First of
  %%         undef ->
  %%           erlang:display({part1, Y0}),
  %%           State0#nat{first = Y0, packet = Other};
  %%         _ ->
  %%           State0#nat{packet = Other}
  %%       end);

  %%   Unknown ->
  %%     ?debugFmt("Unknown message: ~p", [Unknown])
  %% end.

spawn_nics(Prog, Addrs) ->
  Parent = self(),
  lists:foldl(
    fun(N0, Acc) ->
        maps:put(N0, spawn(fun() -> nic(Prog, N0, Parent) end), Acc)
    end, #{}, Addrs).

wait_for_nic_boot(Addrs) ->
  lists:foreach(
    fun(N0) -> receive {booted, N0} -> ok end end, Addrs).

start_nics(Addrs) ->
  lists:foreach(fun(N0) -> nic_pid(N0) ! all_booted end, Addrs).

nic_pid(Addr) ->
  whereis(nic_name(Addr)).

nic_name(Addr) ->
  list_to_atom(lists:flatten(io_lib:format("nic-~p", [Addr]))).

%% "booting" means NIC is waiting for its network address.
nic_input(#nic{ booting = true
              , addr = Addr
              , parent = Parent} = State) ->
  %% Signal parent that we have started.
  Parent ! {booted, Addr},
  %% Wait until all NICs have booted
  receive all_booted -> ok end,
  {State#nic{booting = false}, Addr};

%% If we have a buffered Y value, return it.
nic_input(#nic{iny = Y} = State) when is_integer(Y) ->
  {State#nic{iny = undef}, Y};
%% Poll input queue
nic_input(State) ->
  check_mql(),
  %% This is the only place where the intcode VM actually
  %% waits.
  receive
    {is_idle, Pid} ->
      Pid ! {idle, self(), State#nic.idle},
      nic_input(State);
    {X, Y} ->
      %% Buffer the Y value so that we can return it in the next input
      %% instruction.
      {State#nic{iny = Y, idle = false}, X}
  after 0 ->
      %% Tell NAT that we are idle
      {State#nic{idle = true}, -1}
  end.

%% If all computers have empty incoming packet queues and are
%% continuously trying to receive packets without sending packets, the
%% network is considered idle.

%% New destination address
nic_output(Output, #nic{outseq = N} = State) when N rem 3 == 0 ->
  State#nic{outaddr = Output, outseq = N + 1};

%% Buffer the X value
nic_output(X, #nic{outseq = N} = State) when N rem 3 == 1 ->
  State#nic{outseq = N + 1, outx = X};

%% Send package
nic_output(Y, #nic{outseq = N,
                   outx = X,
                   outaddr = OutAddr} = State) when N rem 3 == 2 ->
  nic_pid(OutAddr) ! {X, Y},
  State#nic{outseq = N + 1, outx = undef, outaddr = undef, idle = false}.

%% Input reader (place downloaded input file in
%% priv/inputs/2019/input23.txt).
get_input() ->
  intcode:parse(inputs:get_as_binary(2019, 23)).

%% Tests
main_test_() ->
  Input = get_input(),
  {"Part 1 & 2", timeout, 3600, ?_assertEqual(24268, part1(Input))}.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
