-module(aoc2015_day04).

-include_lib("eunit/include/eunit.hrl").

main_test_() ->
  {timeout, 60,
   fun() ->
       {282749, 9962624} = part("yzbqklnj", 0, undef)
   end}.

-spec part(string(), integer(), integer() | 'undef') ->
              {integer(), integer()}.
part(Input, N, P1) ->
  case erlang:md5(Input ++ integer_to_list(N)) of
    <<0, 0, 0, _/binary>> ->
      %% Found part 2, we are done
      {P1, N};
    <<0, 0, 0:4, _/bitstring>> when P1 =:= undef ->
      %% Found part 1 solution, continue with part 2.
      part(Input, N + 1, N);
    _ ->
      part(Input, N + 1, P1)
  end.
