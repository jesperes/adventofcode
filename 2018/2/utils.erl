-module(utils).
-export([read_file_lines/1]).
-include_lib("eunit/include/eunit.hrl").

split_binary_into_lines(Binary) ->
    string:tokens(binary_to_list(Binary), "\n").

read_file_lines(File) ->
    {ok, Binary} = file:read_file(File),
    split_binary_into_lines(Binary).

split_binary_into_lines_test() ->
    ["foo", "bar", "frotz"] = split_binary_into_lines(<<"foo\nbar\nfrotz">>).



