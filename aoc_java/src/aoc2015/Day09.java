package aoc2015;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import aoc2015.Day09.InputData;
import common.Combinatorics;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day09 implements IAocIntPuzzle<InputData> {

    record InputData(Set<String> cities, Map<String, Integer> routes) {
    }

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

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 9, "All in a Single Night", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(251, 898);
    }

    @Override
    public InputData parse(Optional<File> file) throws IOException {
        Set<String> cities = new HashSet<>();
        Map<String, Integer> routes = new HashMap<>();

        for (String line : InputUtils.asStringList(file.get())) {
            String s[] = line.split(" ");
            String from = s[0];
            String to = s[2];
            routes.put(makeKey(from, to), Integer.valueOf(s[4]));
            cities.add(from);
            cities.add(to);
        }

        return new InputData(cities, routes);
    }

    @Override
    public Integer part1(InputData input) {
        return Combinatorics.permutations(input.cities).stream()
                .collect(Collectors
                        .summarizingInt(route -> routeLen(route, input.routes)))
                .getMin();
    }

    @Override
    public Integer part2(InputData input) {
        return Combinatorics.permutations(input.cities).stream()
                .collect(Collectors
                        .summarizingInt(route -> routeLen(route, input.routes)))
                .getMax();
    }
}
