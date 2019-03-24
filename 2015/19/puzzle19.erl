-module(puzzle19).
-export([start/0]).

-include("input.hrl").
-include_lib("eunit/include/eunit.hrl").

start() ->
    {part1(), part2()}.

part1() ->
    length(get_all_replacements(?INPUT, ?INPUT_RULES)).

part2() ->
    %% This uses the trick described in
    %% https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4etju
    %% which describes how to compute the minimal number of steps
    %% needed without having to actually figure out which
    %% substitutions to make.
    lists:foldl(fun(C, Steps) ->
                        S0 = case is_lower(C) of
                                 false -> Steps + 1;
                                 true -> Steps
                             end,

                        case C of
                            $( -> S0 - 1;
                            $) -> S0 - 1;
                            $, -> S0 - 2;
                            _ -> S0
                        end

                end, -1, replace_paren(binary_to_list(?INPUT))).

replace_paren([]) ->
    [];
replace_paren([$A,$r|Rest]) ->
    [$(|replace_paren(Rest)];
replace_paren([$R,$n|Rest]) ->
    [$)|replace_paren(Rest)];
replace_paren([$Y|Rest]) ->
    [$,|replace_paren(Rest)];
replace_paren([X|Rest]) ->
    [X|replace_paren(Rest)].

is_lower(C) when (C >= $a) and (C =< $z) ->
    true;
is_lower(_) ->
    false.

get_all_replacements(Input, Rules) ->
    Repls = [apply_rule(Rule, Input) || Rule <- Rules],
    sets:to_list(sets:from_list(lists:flatten(Repls))).

apply_rule({F, T}, Input) ->
    [binary:replace(Input, F, T, [{scope, Pos}])
     || Pos <- binary:matches(Input, F)].
