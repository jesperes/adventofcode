package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Test;

import common.Combinatorics;

public class Day13 {

    int happiness(List<String> arrangement, Map<String, Integer> happies) {
        int h = 0;
        int len = arrangement.size();

        for (int i = 0; i < len; i++) {
            int next = (i + 1) % len;
            String a = arrangement.get(i);
            String b = arrangement.get(next);
            h += happies.getOrDefault(a + b, 0);
            h += happies.getOrDefault(b + a, 0);
        }
        return h;
    }

    @Test
    public void testDay13() throws IOException {

        Set<String> names = new HashSet<>();
        Map<String, Integer> happies = new HashMap<>();

        try (BufferedReader reader = new BufferedReader(
                new FileReader("inputs/2015/day13.txt"))) {

            String line;
            while ((line = reader.readLine()) != null) {
                String[] s = line.split("[ \\.]");
                int lose = s[2].equals("lose") ? -1 : 1;
                happies.put(s[0] + s[10], Integer.valueOf(s[3]) * lose);
                names.add(s[0]);
                names.add(s[10]);
            }
        }

        assertEquals(709, Combinatorics.permutations(names).stream()
                .mapToInt(list -> happiness(list, happies)).max().getAsInt());

        names.add("self");

        assertEquals(668, Combinatorics.permutations(names).stream()
                .mapToInt(list -> happiness(list, happies)).max().getAsInt());
    }
}
