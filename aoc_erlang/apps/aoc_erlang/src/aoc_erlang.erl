%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2019, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created :  2 Nov 2019 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(aoc_erlang).

-export([ main/1 ]).

%% option_spec() ->
%%   [ {list, $l, "list", boolean, "List available solutions."}
%%   ].

main(_Argv) ->
  Solutions = enumerate_solutions(),
  io:format("Solutions: ~p~n", [Solutions]).

enumerate_solutions() ->
  Years = lists:seq(2015, 2019),
  Days = lists:seq(1, 25),
  AllSolutions = [{Year, Day} || Year <- Years, Day <- Days],
  lists:filtermap(
    fun({Year, Day}) ->
        Module = solution_module(Year, Day),
        case code:load_file(Module) of
          {module, _} ->
            case is_solution(Module) of
              true -> {true, Module};
              false ->
                io:format("Not a 'aoc_solution' behavior module: ~p~n",
                          [Module]),
                false
            end;
          {error, nofile} -> false;
          {error, Error} ->
            io:format(
              "Error while enumerating solutions for module '~p': ~p~n",
              [Module, Error]),
            false
        end
    end, AllSolutions).

-spec solution_module(integer(), integer()) -> atom().
solution_module(Year, Day) ->
  list_to_atom(io_lib:format("aoc~B_day~2..0B", [Year, Day])).

-spec is_solution(Module :: module()) -> boolean().
is_solution(Module) ->
  lists:member(aoc_solution, get_implemented_behaviors(Module)).

-spec get_implemented_behaviors(LoadedModule :: module()) -> list(atom()).
get_implemented_behaviors(LoadedModule) ->
  Attrs = proplists:get_value(attributes, LoadedModule:module_info()),
  proplists:get_value(behavior, Attrs, []) ++
    proplists:get_value(behaviour, Attrs, []).
