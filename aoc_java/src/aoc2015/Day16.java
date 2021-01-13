package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;

import org.junit.Test;

import aoc2015.Day16.AuntSue;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day16 implements IAocIntPuzzle<AuntSue> {

    record AuntSue(Map<String, Map<String, Integer>> sues) {
    }

    static Map<String, Integer> sample = getSamples();

    static Map<String, Integer> getSamples() {
        Map<String, Integer> sample = new HashMap<String, Integer>();
        sample.put("children", 3);
        sample.put("cats", 7);
        sample.put("samoyeds", 2);
        sample.put("pomeranians", 3);
        sample.put("akitas", 0);
        sample.put("vizslas", 0);
        sample.put("goldfish", 5);
        sample.put("trees", 3);
        sample.put("cars", 2);
        sample.put("perfumes", 1);
        return sample;
    }

    @Test
    public void testDay16() throws Exception {

        try (BufferedReader reader = new BufferedReader(
                new FileReader("inputs/2015/day16.txt"))) {
            String line;
            while ((line = reader.readLine()) != null) {
                String s[] = line.split("[ :]");
                int sueNum = Integer.valueOf(s[1]);
                int i = line.indexOf(':');
                String rest = line.substring(i + 1);
                s = rest.split(",");
                Map<String, Integer> sue = new HashMap<>();

                boolean matches = true;
                boolean matchesReally = true;

                for (String pair : s) {
                    String[] c = pair.split(":");
                    String compound = c[0].trim();
                    int amount = Integer.valueOf(c[1].trim());
                    sue.put(compound, amount);

                    int sampleAmount = sample.get(compound);

                    // Part 1
                    if (amount != sampleAmount)
                        matches = false;

                    // Part 2
                    switch (compound) {
                    case "cats":
                    case "trees":
                        if (amount <= sampleAmount)
                            matchesReally = false;
                        break;
                    case "pomeranians":
                    case "goldfish":
                        if (amount >= sampleAmount)
                            matchesReally = false;
                        break;
                    default:
                        if (amount != sampleAmount)
                            matchesReally = false;
                    }
                }

                if (matches) {
                    assertEquals(213, sueNum);
                    break;
                }

                if (matchesReally) {
                    assertEquals(323, sueNum);
                }
            }
        }
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 16, "Aunt Sue", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(213, 323);
    }

    @Override
    public AuntSue parse(Optional<File> file) throws IOException {
        Map<String, Map<String, Integer>> sues = new HashMap<>();

        InputUtils.asStringList(file.get()).stream().forEach(line -> {
            Map<String, Integer> sue = new HashMap<>();
            String s[] = line.split(":", 2);
            sues.put(s[0].trim(), sue);

            for (String elem : s[1].split(",")) {
                String[] t = elem.split(":", 2);
                sue.put(t[0].trim(), Integer.parseInt(t[1].trim()));
            }
        });

        return new AuntSue(sues);
    }

    @Override
    public Integer part1(AuntSue input) {
        for (Entry<String, Map<String, Integer>> sue : input.sues.entrySet()) {
            boolean matches = true;

            for (Entry<String, Integer> compound : sue.getValue().entrySet()) {
                int sampleAmount = sample.get(compound.getKey());
                if (sampleAmount != compound.getValue()) {
                    matches = false;
                }
            }

            if (matches) {
                return Integer.valueOf(sue.getKey().substring(4));
            }
        }

        throw new RuntimeException();
    }

    @Override
    public Integer part2(AuntSue input) {
        for (Entry<String, Map<String, Integer>> sue : input.sues.entrySet()) {
            boolean matches = true;

            for (Entry<String, Integer> compound : sue.getValue().entrySet()) {
                int sampleAmount = sample.get(compound.getKey());
                int amount = compound.getValue();

                switch (compound.getKey()) {
                case "cats":
                case "trees":
                    if (amount <= sampleAmount)
                        matches = false;
                    break;
                case "pomeranians":
                case "goldfish":
                    if (amount >= sampleAmount)
                        matches = false;
                    break;
                default:
                    if (amount != sampleAmount)
                        matches = false;
                }
            }

            if (matches) {
                return Integer.valueOf(sue.getKey().substring(4));
            }
        }

        throw new RuntimeException();
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day16());
    }
}
