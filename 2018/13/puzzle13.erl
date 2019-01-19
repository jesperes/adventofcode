%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 13 Dec 2018 by Jesper Eskilson <>

-module(puzzle13).
-export([main/0]).
-compile([export_all]).

trace(Fmt, Args) ->
    Io = get(tracefile),
    io:format(Io, Fmt, Args).

main() ->
    {ok, Io} = file:open("trace-erl.log", [write]),
    put(tracefile, Io),
    
    Input = parse("input.txt"),
    {Carts, _} = Input,
    io:format("Number of carts: ~w~n", [gb_trees:size(Carts)]),
    {{part1, start1(Input)},
     {part2, start2(Input)}}.

start1(Input) ->
    do_steps(Input, part1, 1).

start2(Input) ->
    do_steps(Input, part2, 1).

do_steps(Input, Part, N) ->
    {Carts, Tracks} = Input,
    case do_step(Carts, Tracks, Part, N) of
        {crash, {Y, X}} ->
            {X, Y};
        Carts0 ->
            case gb_trees:size(Carts0) of
                0 ->
                    no_carts_left;
                1 ->
                    {{Y, X}, _} = gb_trees:smallest(Carts0),
                    {X, Y};
                _ ->
                    do_steps({Carts0, Tracks}, Part, N + 1)
            end
    end.

cart_to_str({Y, X}, {Dir, Turn}) ->
    io_lib:format("{y=~w,x=~w,dir=~c,turn=~w}",
                  [Y, X, dir_to_char(Dir), Turn]).

%% Move all carts one tick, return the set of new cart positions.
do_step(Carts, Tracks, Part, N) ->
    trace("begin round ~w~n", [N]),
    MovedCarts = do_step0(Carts, Tracks, gb_trees:empty(), Part),
    trace("end round ~w~n", [N]),    
    MovedCarts.

do_step0(Carts, Tracks, MovedCarts, Part) ->
    case gb_trees:is_empty(Carts) of
        true ->
            MovedCarts;
        false ->
            {Pos, Cart, RemainingCarts} = gb_trees:take_smallest(Carts),
            case move_cart(Pos, Cart, RemainingCarts, MovedCarts, Tracks) of 
                {crash, Type, CrashPos, CrashedInto} ->
                    case {Type, Part} of
                        {_, part1} ->
                            %% For part 1, we simply return the first crash
                            %% position.
                            {crash, CrashPos};
                        
                        %% For part 2, we remove all crashed carts
                        %% and continue until there is only one
                        %% cart left
                        
                        {new, part2} ->
                            %% Crash with not-yet-moved cart; remove
                            %% from the "remaining" list.
                            {Yc, Xc} = CrashPos,
                            trace("  collision at ~w,~w~n",
                                  [Xc, Yc]),
                            trace("    obliterated: ~s~n", [cart_to_str(CrashPos, Cart)]),
                            trace("    obliterated: ~s~n", [cart_to_str(CrashPos, CrashedInto)]),
                            do_step0(gb_trees:delete(CrashPos, RemainingCarts), 
                                    Tracks, MovedCarts, Part);
                        
                        {moved, part2} ->
                            %% Crash with moved cart; remove it from
                            %% the "moved" list.
                            {Yc, Xc} = CrashPos,
                            trace("  collision at ~w,~w~n",
                                  [Xc, Yc]),
                            trace("    obliterated: ~s~n", [cart_to_str(CrashPos, Cart)]),
                            trace("    obliterated: ~s~n", [cart_to_str(CrashPos, CrashedInto)]),
                            do_step0(RemainingCarts, Tracks, 
                                    gb_trees:delete(CrashPos, MovedCarts), Part)
                    end;
                {move, NewPos, MovedCart} ->


                    trace("  moved cart ~s -> ~s~n",
                          [cart_to_str(Pos, Cart),
                           cart_to_str(NewPos, MovedCart)]),

                    do_step0(RemainingCarts, Tracks,
                            gb_trees:insert(NewPos, MovedCart, MovedCarts), 
                            Part)
            end
    end.

move_cart(Pos, Cart, Remaining, Moved, Tracks) ->
    %% Remaining is the set of carts not yet moved in this round.
    %% Moved is the set of carts already moved in this round.

    {Dir, Turn} = Cart,
    
    NewPos = move(Pos, Dir),

    %% Either turn at an intersection, or follow the tracks.
    NewCart =
        case is_at_intersection(NewPos, Tracks) of
            true ->
                {turn(Dir, Turn), next_dir(Turn)};
            _ ->
                Track = gb_trees:get(NewPos, Tracks),
                {follow_track(Dir, Track), Turn}
        end,

    collides(Cart, NewPos, NewCart, Moved, Remaining). 

%% Check for collisions. Returns {crash, NewPos} if there was a crash
%% at the new cart position, or {ok, NewPos, NewCart} if the move to
%% the new position was ok.
collides(Cart, NewPos, NewCart, Moved, Remaining) ->

    %% Case 1: There is an existing (not yet moved) cart in the
    %% position we are about to move this cart into. If they are
    %% facing our way, we have a collision. If they are facing the
    %% other way, they will move out of the way and everything is
    %% fine.
    case gb_trees:lookup(NewPos, Remaining) of
        {value, Cart1} ->
            {crash, new, NewPos, Cart1};
        none ->
            %% Case 2: If we already have placed a cart in this
            %% position, we have a collision.
            case gb_trees:lookup(NewPos, Moved) of
                {value, Cart1} ->
                    {crash, moved, NewPos, Cart1};
                none ->
                    {move, NewPos, NewCart}
            end
    end.

    

%%% Parser

%% Folds a fun over a binary representing a x,y-grid 
%% 
%% TODO move this to a general library function, it is useful for many
%% other puzzles as well. Also, implement without converting to list
%% first.
xy_fold(Fun, Init, Binary) ->
    Str = binary_to_list(Binary),
    [First|_] = string:split(Str, "\n"),
    Width = length(First),

    {_, Out} = 
        lists:foldl(fun(C, {N, AccIn}) ->
                            X = N rem (Width + 1),
                            Y = N div (Width + 1),
                            {N + 1, Fun(X, Y, C, AccIn)}
                    end, {0, Init}, Str),
    Out.

store_pos(X, Y, C, {Carts, Tracks}) ->
    case pos_type(C) of
        cart ->
            {gb_trees:insert({Y, X}, {direction(C), -1}, Carts),
             gb_trees:insert({Y, X}, default_track_under_cart(C), Tracks)};
        track ->
            {Carts, gb_trees:insert({Y, X}, C, Tracks)};
        space ->
            {Carts, Tracks}
    end.
            
parse(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    xy_fold(fun store_pos/4, 
            {gb_trees:empty(), gb_trees:empty()}, Binary).
  
%%% Pretty-printer 

print({Carts, Tracks}) ->
    TrackList = lists:filter(fun(X) -> is_tuple(X) end, gb_trees:keys(Tracks)),
    YCoords = lists:map(fun({Y, _}) -> Y end, TrackList),
    MinY = lists:min(YCoords),
    MaxY = lists:max(YCoords),
    XCoords = lists:map(fun({_, X}) -> X end, TrackList),
    MinX = lists:min(XCoords),
    MaxX = lists:max(XCoords),
    
    S = [[pos_to_str({Y, X}, Carts, Tracks) || X <- lists:seq(MinX, MaxX)] ++ "\n" 
         || Y <- lists:seq(MinY, MaxY)],
    
    io:format("~s~n", [S]).

pos_to_str(Pos, Carts, Tracks) ->
    case gb_trees:lookup(Pos, Carts) of
        {value, {Dir, _}} ->
            dir_to_char(Dir);
        none ->
            case gb_trees:lookup(Pos, Tracks) of
                {value, X} ->  X;
                none -> 32
            end
    end.

%%% Helpers 

pos_type($<) -> cart;
pos_type($>) -> cart;
pos_type($^) -> cart;
pos_type($v) -> cart;
pos_type(32) -> space;
pos_type($\n) -> space;
pos_type($+) -> track;
pos_type($|) -> track;
pos_type($\\) -> track;
pos_type($/) -> track;
pos_type($-) -> track.

default_track_under_cart($<) -> $-;
default_track_under_cart($>) -> $-;
default_track_under_cart($^) -> $|;
default_track_under_cart($v) -> $|.

direction($^) -> 0;
direction($>) -> 1;
direction($v) -> 2;
direction($<) -> 3.

follow_track(0, $/)  -> 1; %% north turning right
follow_track(0, $|)  -> 0; %% north going straight
follow_track(0, $\\) -> 3; %% north turning left
follow_track(1, $/)  -> 0; %% east turning left
follow_track(1, $-)  -> 1; %% east going straight
follow_track(1, $\\) -> 2; %% east turning right
follow_track(2, $/)  -> 3; %% south turning right
follow_track(2, $|)  -> 2; %% south going straight
follow_track(2, $\\) -> 1; %% south turning left
follow_track(3, $/)  -> 2; %% west turning left
follow_track(3, $-)  -> 3; %% west going straight
follow_track(3, $\\) -> 0. %% west turning right


is_facing(0 = _Old, 2 = _New) -> true;
is_facing(1 = _Old, 3 = _New) -> true;
is_facing(2 = _Old, 0 = _New) -> true;
is_facing(3 = _Old, 1 = _New) -> true;
is_facing(_, _) -> false.

move({Y,X}, 0) -> {Y-1,X};
move({Y,X}, 1) -> {Y,X+1};
move({Y,X}, 2) -> {Y+1,X};
move({Y,X}, 3) -> {Y,X-1}.

next_dir(-1) -> 0;
next_dir(0) -> 1;
next_dir(1) -> -1.

turn(Dir, Turn) ->
    (Dir + Turn + 4) rem 4.

dir_to_char(0) -> $^;
dir_to_char(1) -> $>;
dir_to_char(2) -> $v;
dir_to_char(3) -> $<.

is_at_intersection(Pos, Tracks) ->
    case gb_trees:lookup(Pos, Tracks) of
        {value, $+} -> true;
        _  -> false
    end.

