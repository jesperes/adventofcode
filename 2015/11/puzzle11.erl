%%% @author Jesper Eskilson <>
%%% @copyright (C) 2018, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created : 18 Dec 2018 by Jesper Eskilson <>

-module(puzzle11).

-compile([export_all]).

input() ->
    "cqjxjnds".

rev(S) ->
     lists:reverse(S).

increment(Pwd) ->
    Incr = rev(increment0(rev(Pwd))).

increment0([]) ->
    [];
increment0([$z|Xs]) ->
    [$a|increment0(Xs)];
increment0([C|Xs]) ->
    [C + 1|Xs].

has_straight([]) ->
    false;
has_straight([X,Y,Z|_]) when (Y == X + 1) and (Z == Y + 1) ->
    true;
has_straight([_|Rest]) ->
    has_straight(Rest).

no_confusing_letters(Pwd) ->
    not (lists:member($i, Pwd) or
	 lists:member($o, Pwd) or
	 lists:member($l, Pwd)).

two_pairs(Pwd) ->
    length([ X || X <- lists:seq($a, $z),
		  string:find(Pwd, [X, X]) /= nomatch ]) >= 2.


valid(Pwd) ->
    
    %% * Passwords must include one increasing straight of at least three
    %% letters, like abc, bcd, cde, and so on, up to xyz. They cannot skip
    %% letters; abd doesn't count.
    Rule1 = has_straight(Pwd),
	
    %% * Passwords may not contain the letters i, o, or l, as these
    %% letters can be mistaken for other characters and are therefore
    %% confusing.
    Rule2 = no_confusing_letters(Pwd),
    
    %% * Passwords must contain at least two different, non-overlapping
    %% pairs of letters, like aa, bb, or zz.
    Rule3 = two_pairs(Pwd),

    Rule1 and Rule2 and Rule3.
    
    %% if Valid ->
    %% 	    io:format("~s is valid~n", [Pwd]),
    %% 	    true;
    %%    true ->
    %% 	    io:format("~s is not valid~n", [Pwd]),
    %% 	    false
    %% end.
    

next_valid_password(Pwd) ->
    Next = increment(Pwd),
    case valid(Next) of
        true ->
            Next;
        false ->
            %% invalid password, take next
            next_valid_password(increment(Next))
    end.

start() ->
    NextValid = next_valid_password(input()),
    NextNextValid = next_valid_password(NextValid),
    {NextValid, NextNextValid}.
