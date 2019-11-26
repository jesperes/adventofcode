package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

public class Day16 {
    @Test
    public void testDay16() throws Exception {
        Map<String, Integer> sample = new HashMap<>();

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
}
