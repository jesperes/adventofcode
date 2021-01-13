package aoc2015;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import aoc2015.Day13.InputData;
import common.Combinatorics;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day13 implements IAocIntPuzzle<InputData> {

    record InputData(Set<String> names, Map<String, Integer> happies) {
    }

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

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 13, "Knights of the Dinner Table", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(709, 668);
    }

    @Override
    public InputData parse(Optional<File> file) throws IOException {

        Set<String> names = new HashSet<>();
        Map<String, Integer> happies = new HashMap<>();

        for (String line : InputUtils.asStringList(file.get())) {
            String[] s = line.split("[ \\.]");
            int lose = s[2].equals("lose") ? -1 : 1;
            happies.put(s[0] + s[10], Integer.valueOf(s[3]) * lose);
            names.add(s[0]);
            names.add(s[10]);
        }

        return new InputData(names, happies);
    }

    @Override
    public Integer part1(InputData input) {
        return Combinatorics.permutations(input.names()).stream()
                .mapToInt(list -> happiness(list, input.happies())).max()
                .getAsInt();
    }

    @Override
    public Integer part2(InputData input) {
        input.names().add("self");
        return Combinatorics.permutations(input.names()).stream()
                .mapToInt(list -> happiness(list, input.happies())).max()
                .getAsInt();
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day13());
    }
}
