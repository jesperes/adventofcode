%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 13 Dec 2018 by Jesper Eskilson <>

-module(puzzle13).

-export([start1/0]).

start1() ->
    {Tracks,Carts} = parse(testdata()),
    lists:foreach(fun(S) ->
                          io:format("~s~n", [S])
                  end, track_to_str(Tracks, Carts, 15, 8)).

testdata() ->
    "/->-\\        
|   |  /----\\
| /-+--+-\\  |
| | |  | v  |
\\-+-/  \\-+--/
  \\------/   ".

parse(Str) ->
    %% Parse all track segments into a map of {X,Y} -> Char.
    {_, Tracks, Carts} = 
        lists:foldl(
          fun(Line, {Y, TM, CM}) ->
                  {_, TM1, CM1} =
                      lists:foldl(
                        fun(Char, {X,TM2,CM2}) ->
                                {TM3, CM3} = put_track_char({X,Y}, Char, TM2, CM2),
                                {X + 1, TM3, CM3}
                        end,
                        {0, CM, TM},
                        Line),
                  {Y + 1, TM1, CM1}
          end,
          {0, maps:new(), maps:new()},
          string:tokens(Str, "\n")),
    {Tracks,Carts}.

is_cart($<) ->
    true;
is_cart($>) ->
    true;
is_cart($^) ->
    true;
is_cart($v) ->
    true;
is_cart(_) ->
    false.

default_track_under_cart($<) ->
    $-;
default_track_under_cart($>) ->
    $-;
default_track_under_cart($^) ->
    $|;
default_track_under_cart($v) ->
    $|.

put_track_char(K, C, TM, CM) ->
    case is_cart(C) of
        true ->
            TM0 = maps:put(K, default_track_under_cart(C), TM),
            CM0 = maps:put(K, C, CM),
            {TM0, CM0};
        false ->
            TM0 = maps:put(K, C, TM),
            {TM0, CM}
    end.

char_at(Pos, TM, CM) ->
    maps:get(Pos, CM, 
             maps:get(Pos, TM, $?)).

track_to_str(TM, CM, W, H) ->
    [[ char_at({X,Y}, TM, CM) || X <- lists:seq(0, W)] 
     || Y <- lists:seq(0, H)].

