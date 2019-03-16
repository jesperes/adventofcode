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

public class Day09 {

    String makeKey(String s1, String s2) {
        if (s1.compareTo(s2) < 0) {
            return s1 + s2;
        } else {
            return s2 + s1;
        }
    }

    int routeLen(List<String> cities, Map<String, Integer> lengths) {
        int len = 0;
        for (int i = 0; i < cities.size() - 1; i++) {
            String city1 = cities.get(i);
            String city2 = cities.get(i + 1);
            len += lengths.get(makeKey(city1, city2));
        }
        return len;
    }

    @Test
    public void testDay09() throws IOException {
        Set<String> cities = new HashSet<>();
        Map<String, Integer> routes = new HashMap<>();

        try (BufferedReader reader = new BufferedReader(
                new FileReader("inputs/2015/day09.txt"))) {
            String line;
            while ((line = reader.readLine()) != null) {
                String s[] = line.split(" ");
                String from = s[0];
                String to = s[2];
                routes.put(makeKey(from, to), Integer.valueOf(s[4]));
                cities.add(from);
                cities.add(to);
            }
        }

        List<List<String>> permutations = Combinatorics.permutations(cities);

        int minRouteLen = Integer.MAX_VALUE;
        int maxRouteLen = Integer.MIN_VALUE;

        for (List<String> route : permutations) {
            int l = routeLen(route, routes);
            minRouteLen = Math.min(l, minRouteLen);
            maxRouteLen = Math.max(l, maxRouteLen);
        }

        assertEquals(251, minRouteLen);
        assertEquals(898, maxRouteLen);
    }
}
