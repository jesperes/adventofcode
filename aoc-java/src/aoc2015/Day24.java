package aoc2015;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.Test;

import com.google.common.collect.Sets;

public class Day24 {
    // int[] packages = new int[] { 1, 2, 3, 7, 11, 13, 17, 19, 23, 31, 37, 41,
    // 43,
    // 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109,
    // 113 };
    int[] packages = new int[] { 1, 2, 3, 4, 5, 7, 8, 9, 10, 11 };

    static private int product(Collection<Integer> set) {
        int prod = 1;
        for (int n : set)
            prod *= n;
        return prod;
    }

    static private int sum(Collection<Integer> set) {
        int sum = 0;
        for (int n : set)
            sum += n;
        return sum;
    }

    @Test
    public void testDay24() throws Exception {
        int maxcombo = 6;

        Set<Integer> packageSet = new HashSet<Integer>();
        for (int p : packages) {
            packageSet.add(p);
        }

        int groupSum = sum(packageSet) / 3;

        List<Collection<List<Integer>>> groups = new ArrayList<>();

        for (int i = 1; i < maxcombo; i++) {
            for (Collection<Integer> set : Sets.combinations(packageSet, i)) {
                if (sum(set) == groupSum) {
                    /*
                     * Check that the remaining integers can be split into two
                     */
                    Set<Integer> rest = new HashSet<>();
                    for (Integer p : packageSet) {
                        if (!set.contains(p))
                            rest.add(p);
                    }

                    groups.add(set);
                }
            }
        }

        Collections.sort(groups, new Comparator<Collection<Integer>>() {
            @Override
            public int compare(Collection<Integer> arg0,
                    Collection<Integer> arg1) {
                int n = Integer.compare(arg0.size(), arg1.size());
                if (n != 0)
                    return n;
                else {
                    return Integer.compare(product(arg0), product(arg1));
                }
            }
        });
        System.out.println(groupSum);
        for (Collection<Integer> g : groups) {
            System.out.format("%s (%s)%n", g, product(g));
        }
    }
}
