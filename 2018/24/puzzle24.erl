%%% @author  <jespe@LAPTOP-P6HKA27J>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 24 Dec 2018 by  <jespe@LAPTOP-P6HKA27J>

-module(puzzle24).
-compile([export_all]).

start() ->
    input("input.txt").

input(Filename) ->
    {ok, Binary} = file:read_file(Filename),
    Lines = string:tokens(binary_to_list(Binary), "\r\n"),
    lists:map(fun(Line) ->
		      parse_line(Line)
	      end, Lines).

capture_groups(Line, Captures) ->
    lists:map(fun(Capture) ->
		      case Capture of
			  {-1, _} ->
			      "";
			  {Start, Len} ->
			      lists:sublist(Line, Start + 1, Len)
		      end
	      end, Captures).

parse_line(Line) ->
    case re:run(Line, "(\\d+) units each with (\\d+) hit points (\\(.*\\) )?with an attack that does (\\d+) (\\w+) damage at initiative (\\d+)") of
	{match, Captures} ->
	    [_, Units, HP, Weakness, Damage, DamageType, Initiative] =
		capture_groups(Line, Captures),
	    #{ units => list_to_integer(Units),
	       hp => list_to_integer(HP),
	       weakness => parse_weakness(Weakness),
	       damage => list_to_integer(Damage),
	       damagetype => DamageType,
	       initiative => list_to_integer(Initiative)};
	N ->
	    {nomatch, Line}
    end.

parse_weakness([]) ->
    [];
parse_weakness(Weakness) ->
    
