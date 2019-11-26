package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.Test;

import com.google.common.collect.Sets;

import common.IntPair;

public class Day17 {

    @Test
    public void testDay17() throws Exception {
        Set<IntPair> buckets = new HashSet<>();

        try (BufferedReader reader = new BufferedReader(
                new FileReader("inputs/2015/day17.txt"))) {
            String line;
            int n = 1;
            while ((line = reader.readLine()) != null) {
                int x = Integer.valueOf(line);
                buckets.add(new IntPair(x, n++));
            }
        }

        List<Set<IntPair>> combos = new ArrayList<>();

        for (int i = 1; i < buckets.size(); i++) {
            Set<Set<IntPair>> combinations = Sets.combinations(buckets, i);
            combinations.stream().filter(
                    set -> set.stream().mapToInt(pair -> pair.x).sum() == 150)
                    .forEach(combos::add);
        }

        assertEquals(1638, combos.size());

        int minSize = combos.stream().mapToInt(set -> set.size()).min()
                .getAsInt();

        assertEquals(17,
                combos.stream().filter(set -> set.size() == minSize).count());
    }
}
