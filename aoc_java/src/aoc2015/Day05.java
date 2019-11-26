package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.junit.Test;

import common.IntPair;

public class Day05 {

    private final static Set<String> UGLY = new HashSet<>();

    private final static Set<Character> VOWELS = new HashSet<>();

    public Day05() {
        UGLY.add("ab");
        UGLY.add("cd");
        UGLY.add("pq");
        UGLY.add("xy");
        VOWELS.add('a');
        VOWELS.add('e');
        VOWELS.add('i');
        VOWELS.add('o');
        VOWELS.add('u');
    }

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
        Map<IntPair, Integer> map = new HashMap<>();
        for (int i = 0; i < str.length() - 1; i++) {
            IntPair pair = new IntPair((int) str.charAt(i),
                    (int) str.charAt(i + 1));
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

    @Test
    public void testDay05() throws IOException {
        try (BufferedReader reader = new BufferedReader(
                new FileReader("inputs/2015/day05.txt"))) {

            String str;
            int nice1 = 0;
            int nice2 = 0;

            while ((str = reader.readLine()) != null) {
                if (isNice1(str))
                    nice1++;
                if (isNice2(str))
                    nice2++;
            }

            assertEquals(238, nice1);
            assertEquals(69, nice2);
        }
    }
}
