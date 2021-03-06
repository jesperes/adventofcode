package aoc2020;

import java.io.File;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;
import common2.InputUtils;

public class Day05 implements IAocPuzzle<List<Integer>, Integer, Integer> {

    @Override
    public List<Integer> parse(Optional<File> file) {
        List<Integer> list = InputUtils.asStringList(file.get(), this::seatId);
        Collections.sort(list, Collections.reverseOrder());
        return list;
    }

    @Override
    public Integer part1(List<Integer> input) {
        return input.get(0);
    }

    @Override
    public Integer part2(List<Integer> input) {
        for (int i = 0; i < input.size() - 1; i++) {
            int a = input.get(i);
            int b = input.get(i + 1);
            if (a == b + 2)
                return b + 1;
        }
        throw new RuntimeException();
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(928, 610);
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 5, "Binary Boarding", true);
    }

    private Integer seatId(String str) {
        int id = 0;
        for (int i = 0; i < str.length(); i++) {
            id = id << 1;
            switch (str.charAt(i)) {
            case 'B':
            case 'R':
                id |= 1;
            }
        }
        return id;
    }
}
