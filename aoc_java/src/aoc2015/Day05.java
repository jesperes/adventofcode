package aoc2015;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import com.google.common.collect.Sets;

import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocLongPuzzle;
import common2.InputUtils;

public class Day05 implements IAocLongPuzzle<List<String>> {

    private final static Set<String> UGLY = Sets.newHashSet("ab", "cd", "pq",
            "xy");
    private final static Set<Character> VOWELS = Sets.newHashSet('a', 'e', 'i',
            'o', 'u');

    private boolean isNice1(String str) {
        for (String ugly : UGLY) {
            if (str.contains(ugly))
                return false;
        }

        int vowels = 0;
        boolean hasRepeat = false;
        char[] chars = str.toCharArray();

        for (int i = 0; i < chars.length; i++) {
            if (VOWELS.contains(chars[i]))
                vowels++;

            if (i + 1 < chars.length && chars[i] == chars[i + 1])
                hasRepeat = true;
        }

        if (vowels < 3 || !hasRepeat)
            return false;

        return true;
    }

    private boolean hasTwoPairs(String str) {
        Map<String, Integer> map = new HashMap<>();
        for (int i = 0; i < str.length() - 1; i++) {
            var pair = str.substring(i, i + 2);
            if (map.containsKey(pair)) {
                if (i >= map.get(pair) + 2)
                    return true;
            } else {
                map.put(pair, i);
            }
        }

        return false;
    }

    private boolean isNice2(String str) {
        if (!hasTwoPairs(str))
            return false;

        for (int i = 0; i < str.length() - 2; i++) {
            char c1 = str.charAt(i);
            char c2 = str.charAt(i + 2);
            if (c1 == c2)
                return true;
        }

        return false;
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 5,
                "Doesn't He Have Intern-Elves For This?", true);
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(238L, 69L);
    }

    @Override
    public List<String> parse(Optional<File> file) {
        return InputUtils.asStringList(file.get());
    }

    @Override
    public Long part1(List<String> input) {
        return input.stream().parallel().filter(this::isNice1).count();
    }

    @Override
    public Long part2(List<String> input) {
        return input.stream().parallel().filter(this::isNice2).count();
    }
}
