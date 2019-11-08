-module(aoc2015_day16).

-include_lib("eunit/include/eunit.hrl").

mfcsam_output() ->
    #{
      children => 3,
      cats => 7,
      samoyeds => 2,
      pomeranians => 3,
      akitas => 0,
      vizslas => 0,
      goldfish => 5,
      trees => 3,
      cars => 2,
      perfumes => 1
     }.

matches1(cats, AuntVal, MfcSamValue)        -> AuntVal == MfcSamValue;
matches1(trees, AuntVal, MfcSamValue)       -> AuntVal == MfcSamValue;
matches1(pomeranians, AuntVal, MfcSamValue) -> AuntVal == MfcSamValue;
matches1(goldfish, AuntVal, MfcSamValue)    -> AuntVal == MfcSamValue;
matches1(_, AuntVal, MfcSamValue)           -> AuntVal == MfcSamValue.

matches2(cats, AuntVal, MfcSamValue)        -> AuntVal > MfcSamValue;
matches2(trees, AuntVal, MfcSamValue)       -> AuntVal > MfcSamValue;
matches2(pomeranians, AuntVal, MfcSamValue) -> AuntVal < MfcSamValue;
matches2(goldfish, AuntVal, MfcSamValue)    -> AuntVal < MfcSamValue;
matches2(_, AuntVal, MfcSamValue)           -> AuntVal == MfcSamValue.

main_test_() ->
  Binary = inputs:get_as_binary(2015, 16),

  [ fun() -> ?assertEqual(213, find(Binary, fun matches1/3)) end
  , fun() -> ?assertEqual(323, find(Binary, fun matches2/3)) end
  ].

find(Binary, MatchFun) ->
    Map =
        maps:from_list(
          lists:map(fun(Line) ->
                            [_, Num|Rest] = string:tokens(Line, ": ,"),
                            {list_to_integer(Num),
                             list_to_dict(Rest)}
                    end, string:tokens(binary_to_list(Binary), "\r\n"))),

    MfcSam = mfcsam_output(),
    MfcSamKeys = sets:from_list(maps:keys(MfcSam)),

    ResultMap =
        maps:filter(fun(_, M) ->
                            Keys1 = sets:from_list(maps:keys(M)),
                            CommonKeys =
                                sets:intersection(Keys1, MfcSamKeys),
                            lists:all(fun(K) ->
                                              V1 = maps:get(K, M),
                                              V2 = maps:get(K, MfcSam),
                                              MatchFun(K, V1, V2)
                                      end, sets:to_list(CommonKeys))
                    end, Map),

    [{X,_}|_] = maps:to_list(ResultMap),
    X.

list_to_dict([]) ->
    #{};
list_to_dict([Key, Value|Rest]) ->
    maps:put(list_to_atom(Key),
             list_to_integer(Value), list_to_dict(Rest)).
