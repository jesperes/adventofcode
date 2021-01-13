package aoc2015;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.google.gson.Gson;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day12 implements IAocIntPuzzle<Object> {
    static Gson gson = new Gson();

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

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 12, "JSAbacusFramework.io", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(119433, 68466);
    }

    @Override
    public Object parse(Optional<File> file) throws IOException {
        return gson.fromJson(InputUtils.asString(file.get()), Object.class);
    }

    @Override
    public Integer part1(Object input) {
        return countNumbers(input, false);
    }

    @Override
    public Integer part2(Object input) {
        return countNumbers(input, true);
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day12());
    }
}
