-module(puzzle5).
-export([main/0]).
-compile([export_all]).

main() ->
    Input = input_from_file("input.txt"),
    {{part1, start1(Input)},
     {part2, start2(Input)}}.

input_from_file(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    %% file:read_file/1 appends a newline to the file, so we need to
    %% remove it
    Size = byte_size(Binary),
    {B0, _} = split_binary(Binary, Size - 1),
    gb_sets:from_list(
      lists:zip(lists:seq(0, byte_size(B0) - 1),
                binary_to_list(B0))).

start1(Polymer) ->
    react(Polymer).

react(Polymer) ->
    P0 = react_once(gb_sets:iterator(Polymer), Polymer),
    OldSize = gb_sets:size(Polymer),
    NewSize = gb_sets:size(P0),
    if OldSize == NewSize ->
            %% No reactions were possible.
            NewSize;
       true ->
            react(P0)
    end.

react_once(It, Polymer) ->
    case gb_sets:next(It) of
        none ->
            Polymer;
        {{_, X} = A, It0} ->
            case gb_sets:next(It0) of
                none ->
                    Polymer;
                {{_, Y} = B, _} ->
                    if abs(X - Y) == 32 ->
                            %% React!
                            P0 = gb_sets:delete(A, Polymer),
                            P1 = gb_sets:delete(B, P0),
                            It2 = gb_sets:iterator_from(B, P1),
                            react_once(It2, P1);
                       true ->
                            react_once(It0, Polymer)
                    end
            end
    end.

start2(Polymer) ->
    lists:foldl(
      fun(C, Best) ->
              P0 = gb_sets:filter(
                     fun({_, X}) -> 
                             (X =/= C) and (X =/= C - 32)
                     end, Polymer),
              Size = react(P0),
              erlang:display({C, Size, Best}),
              min(Size, Best)
      end, 50000, lists:seq($a, $z)).
