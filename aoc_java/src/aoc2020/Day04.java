package aoc2020;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.collect.Sets;

import aoc2020.Day04.Passport;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;
import common2.InputUtils;

public class Day04 implements IAocPuzzle<List<Passport>, Long, Long> {

    record Passport(Map<Field, String> fields) {

    }

    static Predicate<String> rangePred(int min, int max) {
        return new Predicate<String>() {
            @Override
            public boolean test(String t) {
                int x = Integer.valueOf(t);
                return x >= min && x <= max;
            }
        };
    }

    enum EyeColor {
        amb, blu, brn, gry, grn, hzl, oth;
    }

    enum Unit {
        in, cm;
    }

    static Set<Field> allFields = Sets.newHashSet(Field.values());

    /*
     * Store the rules for which fields are valid as predicates in the Field
     * enum.
     */
    enum Field {
        byr(rangePred(1920, 2002)), //
        iyr(rangePred(2010, 2020)), //
        eyr(rangePred(2020, 2030)), //
        hcl(Pattern.compile("^#[0-9a-f]{6}$").asMatchPredicate()), //
        pid(Pattern.compile("^\\d{9}$").asMatchPredicate()), //
        ecl(new Predicate<String>() {
            @Override
            public boolean test(String t) {
                try {
                    EyeColor.valueOf(t);
                    return true;
                } catch (IllegalArgumentException e) {
                    return false;
                }
            }
        }), //
        hgt(new Predicate<String>() {

            Pattern heightPattern = Pattern
                    .compile("^(?<height>\\d+)(?<unit>cm|in)$");

            @Override
            public boolean test(String t) {
                Matcher m = heightPattern.matcher(t);
                if (m.matches()) {
                    int h = Integer.valueOf(m.group("height"));
                    switch (Unit.valueOf(m.group("unit"))) {
                    case cm:
                        return h >= 150 && h <= 193;
                    case in:
                        return h >= 59 && h <= 76;
                    default:
                        throw new RuntimeException();
                    }
                } else {
                    return false;
                }
            }
        });

        private Predicate<String> pred;

        Field(Predicate<String> pred) {
            this.pred = pred;
        }
    }

    @Override
    public List<Passport> parse(Optional<File> file) {
        List<String> list = InputUtils.asStringList(file.get());
        List<Passport> passports = new ArrayList<>();
        Map<Field, String> fields = new HashMap<>();

        for (String line : list) {
            if (line.length() == 0) {
                // Start a new passport
                passports.add(new Passport(fields));
                fields = new HashMap<>();
                continue;
            }

            for (String pair : line.split(" ")) {
                String[] elems = pair.split(":");
                if (elems.length == 0)
                    continue;
                if (elems[0].equals("cid"))
                    continue;
                fields.put(Field.valueOf(elems[0]), elems[1]);
            }
        }

        if (fields.size() > 0) {
            passports.add(new Passport(fields));
        }

        return passports;
    }

    @Override
    public Long part1(List<Passport> input) {
        return input.stream().filter(this::isValid1).count();
    }

    @Override
    public Long part2(List<Passport> input) {
        return input.stream().filter(this::isValid2).count();
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(200L, 116L);
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 4, "Passport Processing", true);
    }

    private boolean isValid1(Passport p) {
        return allFields.equals(p.fields.keySet());
    }

    private boolean isValid2(Passport p) {
        if (!isValid1(p))
            return false;

        for (Entry<Field, String> e : p.fields.entrySet()) {
            if (!e.getKey().pred.test(e.getValue()))
                return false;
        }

        return true;
    }
}
