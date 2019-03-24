package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.junit.Test;

public class Day19 {
    private Collection<String> getAllReductions(String input,
            Map<String, String> rules) {
        Set<String> set = new HashSet<>();
        for (Entry<String, String> e : rules.entrySet()) {

            for (int i = 0; i < input.length(); i++) {
                int start = i;
                int end = i + e.getValue().length();
                if (end > input.length())
                    continue;

                String substr = input.substring(start, end);
                if (substr.equals(e.getValue())) {
                    String s = new StringBuilder(input)
                            .replace(start, end, e.getKey()).toString();
                    set.add(s);
                }
            }
        }
        return set;
    }

    @Test
    public void testDay19() throws Exception {
        Map<String, String> rules = new HashMap<>();
        String input = "";

        try (BufferedReader reader = new BufferedReader(
                new FileReader("inputs/2015/day19.txt"))) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.replace("Rn", "(").replace("Ar", ")").replace("Y",
                        ",");
                String s[] = line.split(" ");
                switch (s.length) {
                case 3:
                    rules.put(s[2], s[0]);
                    break;
                case 1:
                    input = line;
                    break;
                }
            }
        }

        // Part 1
        assertEquals(576, getAllReductions(input, rules).size());

        /*
         * Part 2. The minimum number of steps needed can be computed *without
         * knowing the actual substitutions*, by simply looking at the rules. I
         * stole the solution to this from Reddit:
         * https://www.reddit.com/r/adventofcode/comments/3xflz8/
         * day_19_solutions/cy4etju.
         *
         * The steps variable here is increased by one each element, then
         * decreased for each rule which can reduce the string by more than one
         * step. The initial -1 is the length of the start molecule "e".
         */
        int steps = -1;

        for (char c : input.toCharArray()) {
            if (!Character.isLowerCase(c))
                steps++;

            switch (c) {
            case '(':
            case ')':
                steps--;
                break;
            case ',':
                steps -= 2;
                break;
            }
        }

        assertEquals(207, steps);
    }
}
