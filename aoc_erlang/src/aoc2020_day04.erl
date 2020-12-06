%%% Advent of Code solution for 2020 day 04.
%%% Created: 2020-12-04T06:20:31+00:00

-module(aoc2020_day04).
-include_lib("eunit/include/eunit.hrl").

%% Puzzle solution
part1(Input) ->
  Passports = parse_passports(Input),
  ValidPassports = lists:filter(fun valid_passport1/1, Passports),
  length(ValidPassports).

part2(Input) ->
  Passports = parse_passports(Input),
  ValidPassports = lists:filter(fun valid_passport2/1, Passports),
  length(ValidPassports).

%% ============================================================
%% Input parsing
%% ============================================================

parse_passports(Lines) ->
  {_, Passports} =
    lists:foldl(
      fun(Line, {Map, Acc}) ->
          case Line of
            "" ->
              %% TODO optimization: move password validation here to
              %% avoid accumulating invalid passwords.
              {#{}, [Map|Acc]};
            _ ->
              Pairs =
                lists:map(fun(S) ->
                              string:split(S, ":")
                          end, string:split(Line, " ", all)),
              Pairs1 =
                lists:foldl(
                  fun([K, V], Acc0) ->
                      KA = list_to_atom(K),
                      maps:put(KA, parse_value(KA, V), Acc0)
                  end, #{}, Pairs),
              {maps:merge(Map, Pairs1), Acc}
          end
      end, {#{}, []}, Lines),
  Passports.

parse_value(byr, V) -> ltoi(V);
parse_value(eyr, V) -> ltoi(V);
parse_value(iyr, V) -> ltoi(V);
parse_value(ecl, V) -> ltoa(V);
parse_value(hgt, V) ->
  case re:run(V, "([0-9]+)(in|cm)", [{capture, all, list}]) of
    {match, [_, N, Unit]} ->
      {list_to_atom(Unit), list_to_integer(N)};
    _ ->
      invalid
  end;
parse_value(_K, V) -> V.

%% ============================================================
%% Input validation
%% ============================================================

required_fields() ->
  [byr, iyr, eyr, hgt, hcl, ecl, pid].

%% Part 1
valid_passport1(P) ->
  lists:all(fun(K) ->
                maps:is_key(K, P)
            end, required_fields()).

%% Part 2
valid_passport2(#{byr := Byr,
                  iyr := Iyr,
                  eyr := Eyr,
                  ecl := Ecl,
                  hgt := Hgt,
                  hcl := Hcl,
                  pid := Pid}) ->
  Byr >= 1920 andalso Byr =< 2002 andalso
    Iyr >= 2010 andalso Iyr =< 2020 andalso
    Eyr >= 2020 andalso Eyr =< 2030 andalso
    valid_height(Hgt) andalso
    valid_hair_color(Hcl) andalso
    valid_eye_color(Ecl) andalso
    valid_pid(Pid);
valid_passport2(_) ->
  false.

valid_height({cm, Cm}) ->
  Cm >= 150 andalso Cm =< 193;
valid_height({in, In}) ->
  In >= 59 andalso In =< 76;
valid_height(_) ->
  false.

valid_hair_color(Hcl) ->
  re_match(Hcl, "^#[0-9a-f]{6}$").

valid_eye_color(amb) -> true;
valid_eye_color(blu) -> true;
valid_eye_color(brn) -> true;
valid_eye_color(gry) -> true;
valid_eye_color(grn) -> true;
valid_eye_color(hzl) -> true;
valid_eye_color(oth) -> true;
valid_eye_color(_) -> false.

valid_pid(Pid) ->
  re_match(Pid, "^\\d{9}$").

%% ============================================================
%% Helpers
%% ============================================================

re_match(S, RE) ->
  case re:run(S, RE) of
    {match, _} -> true;
    _ -> false
  end.

ltoi(S) -> list_to_integer(S).

ltoa(S) -> list_to_atom(S).

%% ============================================================
%% Input reader (place downloaded input file in
%% priv/inputs/2020/input04.txt).
get_input() ->
  string:split(inputs:get_as_string(2020, 04), "\n", all).

%% ============================================================
%% Tests
%% ============================================================

main_test_() ->
  Input = get_input(),

  [ {"Part 1", ?_assertEqual(200, part1(Input))}
  , {"Part 2", ?_assertEqual(116, part2(Input))}
  ].

invalid_part2_test() ->
  Invalid =
    parse_passports(
      ["eyr:1972 cid:100",
       "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926",
       [],
       "iyr:2019",
       "hcl:#602927 eyr:1967 hgt:170cm",
       "ecl:grn pid:012533040 byr:1946",
       [],
       "hcl:dab227 iyr:2012",
       "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277",
       [],
       "hgt:59cm ecl:zzz",
       "eyr:2038 hcl:74454a iyr:2023",
       "pid:3556412378 byr:2007"
      ]),
  ?assert(lists:all(fun(P) -> not valid_passport2(P) end, Invalid)).

valid_part2_test() ->
  Valid =
    parse_passports(
      ["pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980",
       "hcl:#623a2f",
       [],
       "eyr:2029 ecl:blu cid:129 byr:1989",
       "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm",
       [],
       "hcl:#888785",
       "hgt:164cm byr:2001 iyr:2015 cid:88",
       "pid:545766238 ecl:hzl",
       "eyr:2022",
       [],
       "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
      ]),
  ?assert(lists:all(fun valid_passport2/1, Valid)).

misc_test() ->
  ?assert(valid_pid("012345689")),
  ?assertNot(valid_pid("01234568900")).


%% Too high: 117

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
