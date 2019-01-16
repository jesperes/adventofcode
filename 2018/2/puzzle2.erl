-module(puzzle2).
-export([main/0]).

main() ->
    {{part1, start1()},
     {part2, start2()}}.

split_binary_into_lines(Binary) ->
    string:tokens(binary_to_list(Binary), "\n").

read_file_lines(File) ->
    {ok, Binary} = file:read_file(File),
    split_binary_into_lines(Binary).

%% Build a frequency table of the given string
freq_table(Str) ->
    freq_table(Str, maps:new()).
freq_table([], Table) ->
    Table;
freq_table([Char|Rest], Table) ->
    OldFreq = maps:get(Char, Table, 0),
    NewTable = maps:put(Char, OldFreq + 1, Table),
    freq_table(Rest, NewTable).

start1() ->
    Lines = read_file_lines("input.txt"),
    %% Lines = ["abcabc", "abcdef", "ababcc", "abbbac", "aaabbbccc"],
    
    %% This is a list of two-tuples {X, Y}, one for each element in
    %% the list, where X is true if and only if the string contains at
    %% least on letter which occurs exactly twice.  Y is true iff the
    %% string contains a letter which occurs exactly thrice.
    List = 
        lists:filtermap(
          fun(Str) ->
                  Table = freq_table(Str),
                  
                  {true,
                   {
                    maps:size(
                      maps:filter(fun(_Key, Value) ->
                                          Value =:= 2
                                  end, 
                                  Table)) > 0,
                    maps:size(
                      maps:filter(fun(_Key, Value) ->
                                          Value =:= 3
                                  end, 
                                  Table)) > 0}}
                      
          end, Lines),

    %% erlang:display(List),

    %% The number of pairs in the list where the first element is 
    %% true.
    XCount = length(
               lists:filtermap(
                 fun(X) ->
                         case X of
                             {true, _} ->
                                 {true, 1};
                             _ ->
                                 false
                         end
                 end, List)),

    YCount = length(
               lists:filtermap(
                 fun(X) ->
                         case X of
                             {_, true} ->
                                 {true, 1};
                             _ ->
                                 false
                         end
                 end, List)),
    
    XCount * YCount.


number_of_different_chars([], []) ->
    0;
number_of_different_chars([X|Xs], [X|Ys]) ->
    0 + number_of_different_chars(Xs, Ys);
number_of_different_chars([_|Xs], [_|Ys]) ->
    1 + number_of_different_chars(Xs, Ys).

remove_diff_char([], []) ->
    [];
remove_diff_char([X|Xs], [X|Ys]) ->
    [X|remove_diff_char(Xs, Ys)];
remove_diff_char([_|Xs], [_|Ys]) ->
    remove_diff_char(Xs, Ys).

start2() ->       
    Lines = read_file_lines("input.txt"),
    DiffList = [{X, Y, number_of_different_chars(X, Y)} || 
                   X <- Lines, Y <- Lines, X < Y],
    
    [Solution] = 
        lists:filtermap(
          fun({X, Y, N}) ->
                  if N =:= 1 ->
                          {true, remove_diff_char(X, Y)};
                     true ->
                          false
                  end
          end, DiffList),
    Solution.
