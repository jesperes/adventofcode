%%% @author  <jespe@LAPTOP-P6HKA27J>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% Created : 22 Dec 2018 by  <jespe@LAPTOP-P6HKA27J>

-module(puzzle22).
-compile([export_all]).

start() ->
    S0 = #{depth => 510, target => {10, 10}},
    S1 = #{depth => 5913, target => {8, 701}},
    {0, _} = geologic_index({0, 0}, S0),
    {48271, _} = geologic_index({0, 1}, S0),
    {16807, _} = geologic_index({1, 0}, S0),
    {145722555, _} = geologic_index({1, 1}, S0),
    {0, _} = geologic_index({10, 10}, S0),
    
    {114, _} = risk_level(S0),
    {Part1Sol, _} = risk_level(S1),
    {{part1, Part1Sol},
     {part2, unknown}}.


geologic_index({0, 0}, State) ->
    {0, State};
geologic_index({0, Y}, State) ->
    {Y * 48271, State};
geologic_index({X, 0}, State) ->
    {X * 16807, State};
geologic_index({X, Y} = Pos, State) ->
    Target = maps:get(target, State),
    case Target of
        Pos ->
            {0, State};
        _ ->
            {E1, S0} = erosion_level({X - 1, Y}, State),
            {E2, S1} = erosion_level({X, Y - 1}, S0),
            {E1 * E2, S1}
    end.

erosion_level(Pos, State) ->
    case maps:is_key(Pos, State) of
        true ->
            ErosionLevel = maps:get(Pos, State),
            {ErosionLevel, State};
        false ->
            {GI, S0} = geologic_index(Pos, State),
            ErosionLevel = (GI + maps:get(depth, S0)) rem 20183,
            {ErosionLevel, 
             maps:put(Pos, ErosionLevel, S0)}
    end.

region_type(Pos, State) ->    
    {ErosionLevel, S0} = erosion_level(Pos, State),
    case ErosionLevel rem 3 of
        0 -> {rocky, S0};
        1 -> {wet, S0};
        2 -> {narrow, S0}
    end.

    

risk_level(Pos, State) ->
    {RegionType, S0} = region_type(Pos, State),
    RL = 
        case RegionType of
            rocky -> 0;
            wet -> 1;
            narrow -> 2
        end,
    {RL, S0}.

risk_level(State) ->
    risk_level({0, 0}, maps:get(target, State), State).

risk_level({Xs, Ys}, {Xt, Yt}, State) ->
    PosList = 
        [{X, Y} ||
            X <- lists:seq(Xs, Xt),
            Y <- lists:seq(Ys, Yt)],
    lists:foldl(fun(Pos, {N, StateIn}) ->
                        {RL, S0} = risk_level(Pos, StateIn),
                        {RL + N, S0}
                end, {0, State}, PosList).
