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
             }).

part1(Prog) ->
  N = 50,
  Parent = self(),
  Addrs = lists:seq(0, N-1),

  ?debugFmt("Starting NICs", []),

  NicPids =
    lists:foldl(
      fun(N0, Acc) ->
          maps:put(N0, spawn(fun() -> nic(Prog, N0, Parent) end), Acc)
      end, #{}, Addrs),

  ?debugFmt("Waiting for NICs to boot...", []),

  lists:foreach(
    fun(N0) ->
        receive {booted, N0} -> ok end
    end, Addrs),

  ?debugFmt("All NICs are up.", []),

  lists:foreach(fun(N0) -> nic_pid(N0) ! all_booted end, Addrs),

  ?debugFmt("Waiting for result...", []),
  receive
    Result -> Result
  end.

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
  receive
    {X, Y} ->
      %% Buffer the Y value so that we can return it in the next input
      %% instruction.
      {State#nic{iny = Y}, X}
  after 0 ->
      {State, -1}
  end.

%% End condition part 1, send output value back to parent.
nic_output(Output, #nic{addr = Addr,
                        outseq = N,
                        outaddr = 255,
                        parent = Parent} = State) when N rem 3 == 2 ->
  ?debugFmt("NIC ~p sending ~p to 255", [Addr, Output]),
  Parent ! Output,
  State;

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
  State#nic{outseq = N + 1, outx = undef, outaddr = undef}.

part2(_Input) ->
  %% ?debugMsg("Not implemented."),
  0.

%% Input reader (place downloaded input file in
%% priv/inputs/2019/input23.txt).
get_input() ->
  intcode:parse(inputs:get_as_binary(2019, 23)).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(0, part1(Input))}
  , {"Part 2", ?_assertEqual(0, part2(Input))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
