%%% @author  <jespe@LAPTOP-P6HKA27J>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 24 Dec 2018 by  <jespe@LAPTOP-P6HKA27J>

-module(puzzle24).
-compile([export_all]).

-define(UNIT_RE, <<"(\\d+) units each with (\\d+) hit points (\\((.*)\\) )?with an attack that does (\\d+) (\\w+) damage at initiative (\\d+)">>).

start() ->
    input("input.txt").

input(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    [_, _, ImmuneSystem, _, Infection] =
        re:split(binary_to_list(Binary), "(Immune System|Infection):"),
    {ImmuneSystem, Infection},
    {{immune_system, parse_army(ImmuneSystem)}, 
     {infection, parse_army(Infection)}}.

parse_army(Binary) ->
    Groups = string:tokens(binary_to_list(Binary), "\n"),
    lists:foldl(fun(Line, List) ->
                        [parse_group(Line)|List]
                end, [], Groups).

to_i(N) -> list_to_integer(N).
to_a(N) -> list_to_atom(N).
     
parse_group(Line) ->
    case re:run(Line, ?UNIT_RE) of
        {match, Captures} ->
            #{units => to_i(capture_group(Line, Captures, 1)),
              hp => to_i(capture_group(Line, Captures, 2)),
              damage => to_i(capture_group(Line, Captures, 5)),
              damagetype => to_a(capture_group(Line, Captures, 6)),
              initiative => to_i(capture_group(Line, Captures, 7)),
              strengths => parse_strengths("immune", capture_group(Line, Captures, 4)),
              weaknesses => parse_strengths("weak", capture_group(Line, Captures, 4))
             }
    end.
            
        
parse_strengths(What, Str) ->
    X = parse_strength(What, 
                       lists:map(fun string:trim/1, string:tokens(Str, ";"))),
    erlang:display({What, X}),
    X.

parse_strength(_, []) ->
    [];
parse_strength(What, [Strength|Rest]) ->
    Tokens = string:tokens(Strength, " ,"),
    erlang:display({tokens, Tokens}),
    case Tokens of
        [What, "to"|Thing] ->
            lists:map(fun to_a/1, Thing);
        _ ->
            parse_strength(What, Rest)
    end.

capture_group(Line, Captures, N) ->
    Capture = lists:nth(N + 1, Captures),
    case Capture of
        {-1, _} ->
            "";
        {Start, Len} ->
            lists:sublist(Line, Start + 1, Len)
    end.
