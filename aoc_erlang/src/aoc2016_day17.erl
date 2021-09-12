-module(aoc2016_day17).

-include_lib("eunit/include/eunit.hrl").

start(Passcode) ->
    astar2:astar({0, 0, ""},
                 fun is_end/1,
                 fun cost/1,
                 fun(Node) -> neighbors(Node, Passcode) end,
                 fun dist/2).

start_part2(Passcode) ->
    dfs({0, 0, ""}, Passcode, 0).

dfs({3, 3, Path}, _, Acc) ->
    Len = length(Path),
    if Len > Acc ->
           Len;
       true ->
           Acc
    end;
dfs({_, _, _Path} = Node, Passcode, Acc) ->
    lists:foldl(fun(Nbr, AccIn) -> dfs(Nbr, Passcode, AccIn) end,
                Acc,
                neighbors(Node, Passcode)).

%%% Search callbacks
is_end({3, 3, _}) ->
    true;
is_end(_) ->
    false.

cost({X, Y, _}) ->
    abs(X - 3) + abs(Y - 3).

dist(_, _) ->
    1.

neighbors({X, Y, Path}, Passcode) ->
    [U, D, L, R | _] = md5(Passcode ++ Path),
    L1 = add_if_open(U, {X, Y - 1}, $U, Path, []),
    L2 = add_if_open(D, {X, Y + 1}, $D, Path, L1),
    L3 = add_if_open(L, {X - 1, Y}, $L, Path, L2),
    add_if_open(R, {X + 1, Y}, $R, Path, L3).

add_if_open(Char, {X, Y}, Dir, Path, List) ->
    if Char >= $b, X >= 0, Y >= 0, X =< 3, Y =< 3 ->
           [{X, Y, Path ++ [Dir]} | List];
       true ->
           List
    end.

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b", [N]) || <<N>> <= erlang:md5(S)]).

%%% Tests

neighbors_test() ->
    Passcode = "hijkl",
    ?assertEqual([{0, 1, "D"}], neighbors({0, 0, ""}, Passcode)),
    ?assertEqual([{1, 1, "DR"}, {0, 0, "DU"}], neighbors({0, 1, "D"}, Passcode)),
    ?assertEqual([], neighbors({1, 1, "DR"}, Passcode)),
    ?assertEqual([{1, 0, "DUR"}], neighbors({0, 0, "DU"}, Passcode)),
    ?assertEqual([], neighbors({1, 0, "DUR"}, Passcode)).

%% part1_ex_test() ->
%%   ?assertMatch({_, [{_, _, "DDRRRD"}|_]}, start("ihgpwlah")),
%%   ?assertMatch({_, [{_, _, "DDUDRLRRUDRD"}|_]}, start("kglvqrro")),
%%   ?assertMatch({_, [{_, _, "DRURDRUDDLLDLUURRDULRLDUUDDDRR"}|_]}, start("ulqzkmiv")).

%% part2_ex_test() ->
%%   ?assertEqual(370, start_part2("ihgpwlah")),
%%   ?assertEqual(492, start_part2("kglvqrro")),
%%   ?assertEqual(830, start_part2("ulqzkmiv")).

main_test_() ->
    [{"Part 1", ?_assertMatch({_, [{_, _, "DDRRUDLRRD"} | _]}, start("pslxynzg"))},
     {"Part 2", ?_assertEqual(488, start_part2("pslxynzg"))}].
