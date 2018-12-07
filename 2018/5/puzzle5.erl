-module(puzzle5).
-export([start1/0, start2/0]).

start1() ->
    test(),
    Polymer = input(),
    ReactedPolymer = react(Polymer),
    length(ReactedPolymer).

%% testdata() ->
%%     "dabAcCaCBAcCcaDA".

input() ->
    {ok, Binary} = file:read_file("input.txt"),
    %% Trimming is important here, the input data contains a trailing
    %% newline.
    string:trim(binary_to_list(Binary)).

react(Polymer) ->
    case do_react(Polymer) of
        Polymer -> %% nothing happened, we're done
            Polymer;
        NewPolymer ->
            react(NewPolymer)
    end.

is_upper_case(Char) ->
    Char < $a.

to_lower_case(Char) ->
    case is_upper_case(Char) of
        true ->
            Char + 32;
        false ->
            Char
    end.

test() ->
    true = is_upper_case($A),
    false = is_upper_case($a),
    true = is_upper_case($Z),
    false = is_upper_case($z),
    $a = to_lower_case($A),
    $z = to_lower_case($Z),
    "dabCBAcaDA" = react("dabAcCaCBAcCcaDA").

do_react([]) ->
    [];
do_react([X]) ->
    [X];
do_react([X,X|Rest]) ->
    %% no reaction
    [X|do_react([X|Rest])];
do_react([X,Y|Rest]) ->
    case is_upper_case(X) =:= is_upper_case(Y) of
        true ->
            %% same case, no reaction
            [X|do_react([Y|Rest])];
        false->
            %% different case, will react if
            %% same letter
            case to_lower_case(X) =:= to_lower_case(Y) of
                true ->
                    %% Reaction removes the letters and continue with the next
                    do_react(Rest);
                false ->
                    %% No reaction
                    [X|do_react([Y|Rest])]
            end
    end.

%% Removes all occurrences of c/C from the polymer, reacts it and
%% returns a tuple {C, length(P)} where P is the polymer after
%% reaction.
remove_and_react(C, Polymer) ->
    %% io:format("Removing ~p and reacting...~n", [[C]]),
    ReducedPolymer = 
        lists:filter(fun(X) -> 
                             (X /= C) and (X /= (C + 32))
                     end, Polymer),
    ReactedPolymer = react(ReducedPolymer),
    {C, length(ReactedPolymer)}.
    
start2() ->
    %% Polymer = testdata(),
    Polymer = input(),
    List = [ remove_and_react(C, Polymer) || C <- lists:seq($A, $Z)],
    
    lists:foldl(fun({_C,Len}, Min) when Len < Min ->
                        Len;
                   (_, Min) ->
                        Min
                end, length(Polymer), List).

