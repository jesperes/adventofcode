%%% @author Jesper Eskilson <jesper.eskilson@klarna.com>
%%% @copyright (C) 2019, Jesper Eskilson
%%% @doc
%%%
%%% @end
%%% Created :  2 Nov 2019 by Jesper Eskilson <jesper.eskilson@klarna.com>

-module(aoc_solution).

%% The result of a single
-type result() :: integer() | string().

-type solution() ::
        %% Most days have a part 1 and part 2
        {Part1 :: result(), Part2 :: result()} |

        %% Some days (like the 25th) only have single result.
        result().

%% Compute the solution.
-callback start() -> solution().
