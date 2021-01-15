package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.junit.Test;

import com.google.common.collect.Sets;

import common.IntPair;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day17 implements IAocIntPuzzle<List<Integer>> {

    record Pair(int x, int y) {
    }

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

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 17, "No Such Thing as Too Much", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(1638, 17);
    }

    @Override
    public List<Integer> parse(Optional<File> file) throws IOException {
        return InputUtils.asIntList(file.get());
    }

    @Override
    public Integer part1(List<Integer> input) {
        Set<Pair> buckets = new HashSet<>();
        int n = 1;
        for (int x : input) {
            buckets.add(new Pair(x, n++));
        }

        List<Set<Pair>> combos = new ArrayList<>();

        for (int i = 1; i < buckets.size(); i++) {
            Sets.combinations(buckets, i).stream().filter(
                    set -> set.stream().mapToInt(pair -> pair.x).sum() == 150)
                    .forEach(combos::add);
        }
        return combos.size();
    }

    @Override
    public Integer part2(List<Integer> input) {
        Set<Pair> buckets = new HashSet<>();
        int n = 1;
        for (int x : input) {
            buckets.add(new Pair(x, n++));
        }

        List<Set<Pair>> combos = new ArrayList<>();

        for (int i = 1; i < buckets.size(); i++) {
            Sets.combinations(buckets, i).stream().filter(
                    set -> set.stream().mapToInt(pair -> pair.x).sum() == 150)
                    .forEach(combos::add);
        }

        int minSize = combos.stream().mapToInt(set -> set.size()).min()
                .getAsInt();

        return (int) combos.stream().filter(set -> set.size() == minSize)
                .count();
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day17());
    }
}
