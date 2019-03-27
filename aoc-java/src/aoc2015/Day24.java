package aoc2015;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.Test;

import com.google.common.collect.Sets;

public class Day24 {
    int[] packages = new int[] { 1, 2, 3, 7, 11, 13, 17, 19, 23, 31, 37, 41, 43,
            47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109,
            113 };

    static private long product(Collection<Long> set) {
        return set.stream().reduce(1L, (a, b) -> a * b).longValue();
    }

    static private long sum(Collection<Long> set) {
        return set.stream().mapToLong(x -> x).sum();
    }

    long splitIntoGroups(int n) {
        Set<Long> packageSet = Arrays.stream(packages).asLongStream().boxed()
                .collect(Collectors.toSet());

        long groupSum = packageSet.stream().mapToLong(x -> x).sum() / n;

        List<Collection<Long>> groups = new ArrayList<>();

        // Check all groups up to size 7, to avoid combinatorial explosion.
        for (int i = 1; i < 7; i++)
            Sets.combinations(packageSet, i).stream()
                    .filter(set -> sum(set) == groupSum)
                    .forEach(set -> groups.add(set));

        Comparator<Collection<Long>> order = new Comparator<Collection<Long>>() {
            @Override
            public int compare(Collection<Long> arg0, Collection<Long> arg1) {
                int n = Long.compare(arg0.size(), arg1.size());
                return (n != 0) ? n
                        : Long.compare(product(arg0), product(arg1));
            }
        };

        return product(groups.stream().min(order).get());
    }

    @Test
    public void testDay24() throws Exception {
        assertEquals(11846773891L, splitIntoGroups(3));
        assertEquals(80393059L, splitIntoGroups(4));
    }
}
