%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 13 Dec 2018 by Jesper Eskilson <>

-module(puzzle13).

-export([start1/0]).

%% 23,71 is wrong answer for part 2
%% 23,72 is wrong answer for part 2

start1() ->
    %% {Tracks,Carts} = parse(testdata2()),
    {Tracks,Carts} = parse(realdata()),
    %% print_track(Tracks, Carts),
    catch lists:foldl(fun(_, CartsIn) ->
                              Carts0 = do_step(CartsIn, Tracks),
                              %% print_track(Tracks, Carts0),
                              case maps:size(CartsIn) of
                                  1 ->
                                      [LastCartPos] = maps:keys(Carts0),
                                      throw({last_cart_left, LastCartPos});
                                  _ ->
                                      Carts0
                              end
                      end, Carts, lists:seq(1, 10000)).

realdata() ->
    {ok, Binary} = file:read_file("input.txt"),
    binary_to_list(Binary).

testdata() ->
    "/->-\\        
|   |  /----\\
| /-+--+-\\  |
| | |  | v  |
\\-+-/  \\-+--/
  \\------/   ".

testdata2() ->
    "/>-<\\  
|   |  
| /<+-\\
| | | v
\\>+</ |
  |   ^
  \\<->/".

print_track(Tracks, Carts) ->
    lists:foreach(fun(S) ->
                          io:format("~s~n", [S])
                  end, track_to_str(Tracks, Carts, 15, 8)).

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
                        {0, TM, CM},
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

direction($^) -> 0;
direction($>) -> 1;
direction($v) -> 2;
direction($<) -> 3.

dir_to_char(0) -> $^;
dir_to_char(1) -> $>;
dir_to_char(2) -> $v;
dir_to_char(3) -> $<.

put_track_char(K, C, TM, CM) ->
    case is_cart(C) of
        true ->
            TM0 = maps:put(K, default_track_under_cart(C), TM),
            %% Cart values are represented as {Dir,Next} where Dir is
            %% the current direction 0..3 clockwise, and Next is one
            %% of {-1, 0, 1} to apply to direction at next
            %% intersection.
            CM0 = maps:put(K, {direction(C),-1}, CM),
            {TM0, CM0};
        false ->
            case C of
                32 ->
                    {TM, CM};
                _ ->
                    TM0 = maps:put(K, C, TM),
                    {TM0, CM}
            end
    end.



char_at(Pos, TM, CM) ->
    case maps:get(Pos, CM, empty) of
        {Dir,_NextDir} ->
            dir_to_char(Dir);
        empty ->
            maps:get(Pos, TM, 32)
    end.

track_to_str(TM, CM, W, H) ->
    [[ char_at({X,Y}, TM, CM) || X <- lists:seq(0, W)] 
     || Y <- lists:seq(0, H)].

do_step(Carts, Tracks) ->
    maps:fold(fun(Pos, Cart, Carts0) ->
                      move_cart(Pos, Cart, Tracks, Carts0)
              end, #{}, Carts).


move({X,Y}, 0) ->
    {X,Y-1};
move({X,Y}, 1) ->
    {X+1,Y};
move({X,Y}, 2) ->
    {X,Y+1};
move({X,Y}, 3) ->
    {X-1,Y}.

next_dir(-1) -> 0;
next_dir(0) -> 1;
next_dir(1) -> -1.

turn(Dir, Turn) ->
    (Dir + Turn + 4) rem 4.

is_at_intersection(Pos, Tracks) ->
    case maps:get(Pos, Tracks, false) of
        $+ ->
            true;
        _ ->
            false
    end.

follow_track(0, $/) -> %% north turning right
    1;
follow_track(0, $|) -> %% north going straight
    0;
follow_track(0, $\\) -> %% north turning left
    3;
follow_track(1, $/) -> %% east turning left
    0;
follow_track(1, $-) -> %% east going straight
    1;
follow_track(1, $\\) -> %% east turning right
    2;
follow_track(2, $/) -> %% south turning right
    3;
follow_track(2, $|) -> %% south going straight
    2;
follow_track(2, $\\) -> %% south turning left
    1;
follow_track(3, $/) -> %% west turning left
    2;
follow_track(3, $-) -> %% west going straight
    3;
follow_track(3, $\\) -> %% west turning right
    0.

move_cart(Pos, Cart, Tracks, NewCarts) ->
    {Dir, Turn} = Cart,
    
    NewCart = {NewDir, _} =
        case is_at_intersection(Pos, Tracks) of
            true ->
                {turn(Dir, Turn), next_dir(Turn)};
            _ ->
                Track = maps:get(Pos, Tracks),
                {follow_track(Dir, Track), Turn}
        end,
    
    NewPos = move(Pos, NewDir),
    
    case maps:is_key(NewPos, NewCarts) of
        true ->
            %% In part 1, we just throw here.
            %% throw({collision, NewPos});
            
            %% For part 2, we obliterate the colliding carts and
            %% continue until the is only one cart left.
            erlang:display({obliterating, NewPos}),
            maps:remove(NewPos, NewCarts);
        _ ->
            maps:put(NewPos, NewCart, NewCarts)
    end.


    
