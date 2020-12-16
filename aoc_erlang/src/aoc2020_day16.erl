%%% Advent of Code solution for 2020 day 16.
%%% Created: 2020-12-16T06:17:01+00:00

-module(aoc2020_day16).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Input) ->
  #{fields := Fields,
    nearbytickets := Tickets} = parse(Input),

  lists:foldl(
    fun(Num, Acc) ->
        case is_valid_for_some_field(Num, Fields) of
          true -> Acc;
          false -> Num + Acc
        end
    end, 0, lists:flatten(Tickets)).

part2(Input) ->
  P = #{myticket := MyTicket} = parse(Input),

  show_valid_fields(P),

  DepartureFields = [3, 4, 8, 11, 12, 14],
  lists:foldl(fun erlang:'*'/2, 1,
              lists:map(fun(Pos) ->
                            lists:nth(Pos, MyTicket)
                        end, DepartureFields)).

-ifdef(PRINT_VALID_FIELDS).
show_valid_fields(#{fields := Fields,
                    myticket := MyTicket,
                    nearbytickets := Tickets}) ->
  ValidTickets =
    lists:reverse(
      lists:filter(fun(Ticket) -> is_ticket_valid(Ticket, Fields) end,
                   Tickets)),

  io:format("Valid tickets:~n~120p~n", [ValidTickets]),

  lists:foreach(
    fun(Pos) ->
        ValidFieldsForPos =
          find_field(Pos, [MyTicket|ValidTickets], Fields),
        io:format("~nValid fields for pos ~p~n~p~n", [Pos, ValidFieldsForPos])
    end, lists:seq(1, maps:size(Fields))).

%% These were found by printing out valid fields for each position and
%% then doing a ocular inspection...
field_map('arrival location') -> 19;
field_map('arrival platform') -> 2;
field_map('arrival station') -> 17;
field_map('arrival track') -> 6;
field_map('departure date') -> 4;
field_map('departure location') -> 14;
field_map('departure platform') -> 12;
field_map('departure station') -> 11;
field_map('departure time') -> 8;
field_map('departure track') -> 3;
field_map(class) -> 5;
field_map(duration) -> 18;
field_map(price) -> 10;
field_map(route) -> 15;
field_map(row) -> 9;
field_map(seat) -> 13;
field_map(train) -> 1;
field_map(type) -> 16;
field_map(wagon) -> 7;
field_map(zone) -> 20.

%% Find what field corresponds to position `Pos'
find_field(Pos, Tickets, Fields) ->
  FieldVals =
    lists:map(fun(Ticket) ->
                  lists:nth(Pos, Ticket)
              end, Tickets),

  maps:filter(fun(_Field, Range) ->
                  lists:all(fun(V) -> in_range(V, Range) end,
                            FieldVals)
              end, Fields).


%% Valid tickets are tickets where no fields are invalid, i.e.  all
%% numbers has a field they can belong to.
is_ticket_valid(Ticket, Fields) ->
  lists:all(
    fun(Num) ->
        is_valid_for_some_field(Num, Fields)
    end, Ticket).

-else.
show_valid_fields(_) ->
  ok.
-endif.

%% Determine if a given number is valid, i.e. if there is a field
%% which this number could belong to.
is_valid_for_some_field(Num, Fields) ->
  lists:any(
    fun(Range) ->
        in_range(Num, Range)
    end, maps:values(Fields)).

in_range(Num, {{A1, A2}, {B1, B2}}) ->
  ((Num >= A1) and (Num =< A2))
    orelse ((Num >= B1) and (Num =< B2)).


%% Too high: 2332518

%% ======================================================================
%% Parser
%% ======================================================================

parse(Lines) ->
  Map =
    lists:foldl(
      fun("your ticket:", #{section := fields} = Acc) ->
          maps:update(section, myticket, Acc);
         ("nearby tickets:", #{section := myticket} = Acc) ->
          maps:update(section, nearbytickets, Acc);
         (L, #{section := fields} = Acc) ->
          case re:run(L, "([a-z ]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)",
                      [{capture, all_but_first, list}]) of
            {match, [F, A1, A2, B1, B2]} ->
              Range = {{stoi(A1), stoi(A2)}, {stoi(B1), stoi(B2)}},
              F0 = list_to_atom(F),
              maps:update_with(
                fields,
                fun(Old) -> maps:put(F0, Range, Old) end,
                #{F0 => Range}, Acc)
          end;
         (L, #{section := myticket} = Acc) ->
          maps:put(myticket, str_to_int_list(L), Acc);
         (L, #{section := nearbytickets} = Acc) ->
          Nums = str_to_int_list(L),
          maps:update_with(
            nearbytickets,
            fun(Old) -> [Nums|Old] end, [Nums], Acc)
      end, #{section => fields}, Lines),
  maps:remove(section, Map).

%% ======================================================================
%% Helpers
%% ======================================================================

str_to_int_list(S) ->
  lists:map(fun list_to_integer/1, string:split(S, ",", all)).

stoi(S) ->
   list_to_integer(S).

%% Input reader (place downloaded input file in
%% priv/inputs/2020/input16.txt).
get_input() ->
  inputs:get_as_lines(2020, 16).

%% Tests
main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(27850, part1(Input))}
  , {"Part 2", ?_assertEqual(491924517533, part2(Input))}
  ].

test_input() ->
  ["class: 1-3 or 5-7",
   "row: 6-11 or 33-44",
   "seat: 13-40 or 45-50",
   "your ticket:",
   "7,1,14",
   "nearby tickets:",
   "7,3,47",
   "40,4,50",
   "55,2,20",
   "38,6,12"].

ex1_test_() ->
  ?_assertEqual(71, part1(test_input())).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
