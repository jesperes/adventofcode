%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 13 Dec 2018 by Jesper Eskilson <>

-module(puzzle13).
%% -include_lib("eunit/include/eunit.hrl").

-export([start1/0, test/0]).

%% 23,71 is wrong answer for part 2
%% 23,72 is wrong answer for part 2
%% 104,51 is wrong answer for part 2
%% 53,75 is wrong answer for part 2
%% 146,90
%% 73,88
%% 64,50

-define(VARIANT, {part2,real}).

start1() ->
    {Tracks, Carts} = 
        case ?VARIANT of
            {part1, test} ->
                parse(testdata());
            {part1, real} -> 
                parse(realdata());
            {part2, test} ->
                parse(testdata2());
            {part2, real} ->
                parse(realdata())
        end,
    
    print_track(Tracks, Carts),
    catch lists:foldl(fun(_, CartsIn) ->
                              Carts0 = do_step(CartsIn, Tracks),
                              print_track(Tracks, Carts0),
                              case maps:size(Carts0) of
                                  1 ->
                                      [LastCartPos] = maps:keys(Carts0),
                                      throw({last_cart_left, LastCartPos});
                                  0 ->
                                      throw(no_carts_left);
                                  _ ->
                                      Carts0
                              end
                      end, Carts, lists:seq(1, 1000000)).

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
    "\
/<--\\  
|   v  
^ />+-\\
| | | v
\\>+</ |
  |   ^
  \\<->/
".


print_track(Tracks, Carts) ->
    case ?VARIANT of 
        {_, test} ->
            lists:foreach(fun(S) ->
                                  %% erlang:display(S)
                                  io:format("~s~n", [S])
                          end, track_to_str(Tracks, Carts, 12, 7));
        _ ->
            ok
    end.
                

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
    SortedCarts = 
        lists:sort(fun({{X1,Y1},_},{{X2,Y2},_}) ->
                           if Y1 < Y2 ->
                                   true;
                              Y1 == Y2 ->
                                   X1 =< X2;
                              true ->
                                   false
                           end
                   end, maps:to_list(Carts)),
    
    lists:foldl(
      fun({Pos, Cart}, Carts0) ->
              move_cart(Pos, Cart, Tracks, Carts0, Carts)
      end, #{}, SortedCarts).


is_facing(0 = _Old, 2 = _New) ->
    true;
is_facing(1 = _Old, 3 = _New) ->
    true;
is_facing(2 = _Old, 0 = _New) ->
    true;
is_facing(3 = _Old, 1 = _New) ->
    true;
is_facing(_, _) ->
    false.
  
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

move_cart(Pos, Cart, Tracks, NewCarts, OldCarts) ->
    {Dir, Turn} = Cart,
    
    %% erlang:display({moving, Pos, Cart}),

    NewPos = move(Pos, Dir),
    
    NewCart = {NewDir, _} =
        case is_at_intersection(NewPos, Tracks) of
            true ->
                %% Turn in the specified direction, and compute what
                %% to do in the next turn
                {turn(Dir, Turn), next_dir(Turn)};
            _ ->
                %% Otherwise, check if the track turns at the new
                %% position and turn accordingly to follow
                Track = maps:get(NewPos, Tracks),
                {follow_track(Dir, Track), Turn}
        end,
    
    %% Check for collisions.

    %% Case 1: There is an existing (not yet moved) cart in the
    %% position we are about to move this cart into. If they are
    %% facing our way, we have a collision. If they are facing the
    %% other way, they will move out of the way and everything is
    %% fine.
    OldCartAtNewPos = maps:get(NewPos, OldCarts, none),
    Coll1 = 
        case OldCartAtNewPos of
            none ->
                false;
            {D1, _} ->
                is_facing(D1, Dir)
        end,

    %% Case 2: If we already have placed a cart in this position, 
    %% we have a collision.
    Coll2 = maps:is_key(NewPos, NewCarts),

    IsCollision = Coll1 or Coll2,

    case IsCollision of
        true ->
            case ?VARIANT of
                {part1, _} ->
                    throw({collision, NewPos});
                {part2, _} ->
                    erlang:display({removing, NewPos}),
                    maps:remove(NewPos, NewCarts)
            end;
        _ ->
            maps:put(NewPos, NewCart, NewCarts)
    end.


s(L) ->
    lists:flatten(lists:join("\n", L)).

ensure_collision(Str) ->
    {Tracks, Carts} = 
        parse(Str),
    print_track(Tracks, Carts),

    C0 = do_step(Carts, Tracks),
    print_track(Tracks, C0),
    
    C1 = do_step(C0, Tracks),
    print_track(Tracks, C1).


test() ->
    %% ensure_collision(
    %%   s(["/<--\\",
    %%      "|   |",
    %%      "^   |",
    %%      "\\---/"])),
    
    %% ensure_collision(
    %%   s(["/-<-\\",
    %%      "^   |",
    %%      "|   |",
    %%      "\\---/"])),

    %% ensure_collision(
    %%   s(["/->-\\",
    %%      "|   ^",
    %%      "|   |",
    %%      "\\---/"])),

    %% ensure_collision(
    %%   s(["/---\\",
    %%      "|   |",
    %%      "|   v",
    %%      "|  /+<-\\",
    %%      "|  |^  |",
    %%     "\\--+/  |",
    %%      "   |   |",
    %%     "   \\---/"])),
    
    ensure_collision(
      s(["/----\\",
         "|    |",
         "|    v",
         "|  /-+<-\\",
         "|  | ^  |",
        "\\--+-/  |",
         "   |    |",
        "   \\----/"])),
    ok.

    
