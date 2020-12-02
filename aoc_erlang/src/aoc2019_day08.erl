%%% Advent of Code solution for 2019 day 08.
%%% Created: 2019-12-08T06:56:28+00:00

-module(aoc2019_day08).
-include_lib("eunit/include/eunit.hrl").

-define(W, 25).
-define(H, 6).
-define(LSIZE, 150). %% Number of pixels per layer

-define(BLACK, $0).
-define(WHITE, $1).
-define(TRANSPARENT, $2).

layers() -> lists:seq(0, 99).

%% === [ Part 1 ] ===

part1(Binary) ->
  {_, Ones, Twos} =
    lists:min(
      lists:map(
        fun(Layer) ->
            %% Count 0/1/2 in this layer
            Fun = fun(N, {N0, N1, N2} = Acc) ->
                      case binary:at(Binary, N) of
                        $0 -> {N0 + 1, N1, N2};
                        $1 -> {N0, N1 + 1, N2};
                        $2 -> {N0, N1, N2 + 1};
                        _ -> Acc
                      end
                  end,
            lists:foldl(Fun, {0, 0, 0},
                        lists:seq(Layer * ?LSIZE,
                                  (Layer + 1) * ?LSIZE - 1))
        end, layers())),
  Ones * Twos.

%% === [ Part 2 ] ===

part2(Binary) ->
  lists:flatten(
    [[pixel_at({X, Y}, Binary) ||
       X <- lists:seq(0, ?W - 1)] ++ "\n" ||
      Y <- lists:seq(0, ?H - 1)]).

pixel_at({X, Y}, Binary) ->
  compose(lists:map(
            fun(Layer) ->
                binary:at(Binary, Layer * ?LSIZE + Y * ?W + X)
            end, layers())).

compose([?WHITE|_]) -> $#;
compose([?BLACK|_]) -> $\ ;
compose([?TRANSPARENT|Rest]) -> compose(Rest).

get_input() ->
  inputs:get_as_binary(2019, 08).

%% === [ Tests ] ===

main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(1703, part1(Input))}
  , {"Part 2",
     ?_assertEqual(
        "#  #  ##   ##  #### #### \n"
        "#  # #  # #  # #    #    \n"
        "#### #    #    ###  ###  \n"
        "#  # #    # ## #    #    \n"
        "#  # #  # #  # #    #    \n"
        "#  #  ##   ### #    #### \n",
        part2(Input))}
  ].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
