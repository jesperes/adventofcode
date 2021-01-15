package aoc2016;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day03 implements IAocIntPuzzle<int[][]> {

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2016, 3, "Squares With Three Sides", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(917, 1649);
    }

    private boolean valid(int[] tuple) {
        int x = tuple[0];
        int y = tuple[1];
        int z = tuple[2];
        return (x + y) > z && (z + x) > y && (z + y) > x;
    }

    @Override
    public int[][] parse(Optional<File> file) throws IOException {
        var list = InputUtils.asStringList(file.get());
        int[][] tuples = new int[list.size()][3];
        for (int i = 0; i < list.size(); i++) {
            String[] s = list.get(i).trim().split("\\s+");
            tuples[i][0] = Integer.parseInt(s[0]);
            tuples[i][1] = Integer.parseInt(s[1]);
            tuples[i][2] = Integer.parseInt(s[2]);
        }
        return tuples;
    }

    @Override
    public Integer part1(int[][] input) {
        int valid = 0;
        for (int row = 0; row < input.length; row++) {
            if (valid(input[row]))
                valid++;
        }
        return valid;
    }

    @Override
    public Integer part2(int[][] input) {
        int valid = 0;
        for (int col = 0; col < 3; col++) {
            for (int row = 0; row < input.length; row += 3) {
                int[] tuple = new int[3];
                tuple[0] = input[row][col];
                tuple[1] = input[row + 1][col];
                tuple[2] = input[row + 2][col];
                if (valid(tuple))
                    valid++;
            }
        }
        return valid;
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day03());
    }
}
