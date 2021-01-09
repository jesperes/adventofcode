package aoc2015;

import java.io.File;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import aoc2015.Day02.Dimensions;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day02 implements IAocIntPuzzle<List<Dimensions>> {

    record Dimensions(int x, int y, int z) {

        int wrapping() {
            return (2 * x * y) + (2 * x * z) + (2 * z * y)
                    + min(x * y, y * z, z * x);
        }

        int ribbon() {
            return 2 * min(x + y, y + z, z + x) + (x * y * z);
        }

        int min(int x1, int x2, int x3) {
            if (x1 < x2 && x1 < x3)
                return x1;
            else if (x2 < x3)
                return x2;
            else
                return x3;
        }
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 2, "I Was Told There Would Be No Math",
                true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(1586300, 3737498);
    }

    @Override
    public List<Dimensions> parse(Optional<File> file) {
        return InputUtils.asStringList(file.get()).stream().map(line -> {
            var elems = line.split("x");
            return new Dimensions(Integer.parseInt(elems[0]),
                    Integer.parseInt(elems[1]), Integer.parseInt(elems[2]));
        }).collect(Collectors.toList());
    }

    @Override
    public Integer part1(List<Dimensions> input) {
        return input.stream()
                .collect(Collectors.summingInt(dim -> dim.wrapping()));
    }

    @Override
    public Integer part2(List<Dimensions> input) {
        return input.stream()
                .collect(Collectors.summingInt(dim -> dim.ribbon()));
    }
}
