package aoc2020;

import static java.util.Arrays.stream;
import static java.util.stream.Collectors.toList;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import aoc2020.Day19.Input19;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocLongPuzzle;
import common2.InputUtils;

public class Day19 implements IAocLongPuzzle<Input19> {

    Pattern terminal = Pattern.compile("\"(?<terminal>a|b)\"");

    record Rule(String lhs, List<String> rhs) {
    }

    record Input19(Map<String, Rule> rules, List<String> strings) {

    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 19, "Monster Messages", true);
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(147L, 263L);
    }

    @Override
    public Input19 parse(Optional<File> file) {
        String[] elems = InputUtils.asString(file.get()).split("\n\n");
        return new Input19(stream(elems[0].split("\n")).map((String line) -> {
            String[] e = line.split(":");
            return new Rule(e[0], stream(e[1].split(" "))
                    .filter(s -> s.length() > 0).collect(toList()));
        }).collect(Collectors.toMap(k -> k.lhs, v -> v)),
                List.of(elems[1].split("\n")));
    }

    @Override
    public Long part1(Input19 input) {
        String regex = toRegex(input, "0");
        Pattern p = Pattern.compile(regex);
        return input.strings().stream().filter(s -> p.matcher(s).matches())
                .count();
    }

    @Override
    public Long part2(Input19 input) {
        /*
         * For part 2 we observe that the rules are modified such that we want
         * to match: (42){1..} (42){N} (31){N}
         */
        String re42 = toRegex(input, "42");
        String re31 = toRegex(input, "31");
        Pattern rule42 = Pattern.compile("^(" + re42 + ")+$");
        Pattern rule42e = Pattern.compile("^(?<first>.*)" + re42 + "$");
        Pattern rule31e = Pattern.compile("^(?<first>.*)" + re31 + "$");

        return input.strings().stream()
                .filter(s -> matches2(input, rule42, rule42e, rule31e, s))
                .count();
    }

    public boolean matches2(Input19 input, Pattern rule42, Pattern rule42e,
            Pattern rule31e, String str) {

        int n = 0;
        while (true) {
            // Count number of matches of rule 31 can shave of the end
            Matcher m31e = rule31e.matcher(str);
            if (m31e.matches()) {
                n++;
                str = m31e.group("first");
            } else {
                break;
            }
        }

        // We need at least *one* production of rule 31
        if (n == 0)
            return false;

        // Match the same number of rule 42 at the end
        for (int i = 0; i < n; i++) {
            Matcher m42e = rule42e.matcher(str);
            if (m42e.matches()) {
                str = m42e.group("first");
            } else {
                return false;
            }
        }

        return rule42.matcher(str).matches();
    }

    private String toRegex(Input19 input, String lhs) {
        Rule rule = input.rules.get(lhs);
        switch (rule.rhs.size()) {
        case 5: // 1 2 | 3 4
        {
            String a = toRegex(input, rule.rhs.get(0));
            String b = toRegex(input, rule.rhs.get(1));
            String c = toRegex(input, rule.rhs.get(3));
            String d = toRegex(input, rule.rhs.get(4));
            return "(" + a + b + "|" + c + d + ")";
        }
        case 3: // 1 | 2
        {
            String a = toRegex(input, rule.rhs.get(0));
            String b = toRegex(input, rule.rhs.get(2));
            return "(" + a + "|" + b + ")";
        }
        case 2: // 1 2
        {
            String a = toRegex(input, rule.rhs.get(0));
            String b = toRegex(input, rule.rhs.get(1));
            return a + b;
        }
        case 1: // 1 or "a"
        {
            Matcher m = terminal.matcher(rule.rhs.get(0));
            if (m.matches()) {
                return m.group("terminal");
            } else {
                return toRegex(input, rule.rhs.get(0));
            }
        }
        default:
            throw new RuntimeException();
        }
    }

    public static void main(String[] args) throws FileNotFoundException {
        AocBaseRunner.run(new Day19());
    }
}
