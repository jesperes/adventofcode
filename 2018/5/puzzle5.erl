-module(puzzle5).
-export([start1/0, start2/0]).

start1() ->
    test(),
    Polymer = input(),
    ReactedPolymer = react(Polymer),
    length(ReactedPolymer).

testdata() ->
    "dabAcCaCBAcCcaDA".

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

start2() ->
    
