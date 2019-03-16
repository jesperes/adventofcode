package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.junit.Test;

import com.google.gson.Gson;

public class Day12 {

    @Test
    public void testDay12() throws IOException {
        Gson gson = new Gson();
        try (BufferedReader reader = new BufferedReader(
                new FileReader("inputs/2015/day12.txt"))) {

            Object json = gson.fromJson(reader, Object.class);

            assertEquals(119433, countNumbers(json, false));
            assertEquals(68466, countNumbers(json, true));
        }
    }

    @SuppressWarnings({ "rawtypes" })
    private int countNumbers(Object json, boolean excludeReds) {
        int nums = 0;

        if (json instanceof Map) {
            Map map = (Map) json;

            if (excludeReds && map.values().contains("red"))
                return 0;

            for (Object obj : map.values()) {
                nums += countNumbers(obj, excludeReds);
            }
        } else if (json instanceof List) {
            List list = (List) json;
            for (Object obj : list) {
                nums += countNumbers(obj, excludeReds);
            }
        } else if (json instanceof Number) {
            Number number = (Number) json;
            nums += number.intValue();
        }

        return nums;
    }
}
