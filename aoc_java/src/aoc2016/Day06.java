package aoc2016;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.stream.Collectors;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;
import common2.InputUtils;

public class Day06 implements IAocPuzzle<List<String>, String, String> {

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2016, 6, "Signals and Noise", true);
    }

    @Override
    public AocResult<String, String> getExpected() {
        return AocResult.of("dzqckwsd", "lragovly");
    }

    @Override
    public List<String> parse(Optional<File> file) throws IOException {
        return InputUtils.asStringList(file.get());
    }

    @Override
    public String part1(List<String> input) {
        return findMessage(input, true);
    }

    @Override
    public String part2(List<String> input) {
        return findMessage(input, false);
    }

    private String findMessage(List<String> input, boolean reverse) {
        List<Map<Character, Integer>> freqMaps = input.get(0).chars()
                .mapToObj(i -> new HashMap<Character, Integer>())
                .collect(Collectors.toList());

        for (String s : input) {
            for (int i = 0; i < s.length(); i++) {
                var map = freqMaps.get(i);
                var c = s.charAt(i);
                map.put(c, map.getOrDefault(c, 0) + 1);
            }
        }

        Comparator<Entry<Character, Integer>> cmpByVal = (a, b) -> a.getValue()
                .compareTo(b.getValue());

        return freqMaps.stream().map(map -> map.entrySet().stream()
                .sorted(reverse ? Collections.reverseOrder(cmpByVal) : cmpByVal)
                .findFirst().get().getKey()).map(c -> String.valueOf(c))
                .collect(Collectors.joining());
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day06());
    }
}
