package aoc2017;

import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day08 implements IAocIntPuzzle<List<String>> {

    int interpret(List<String> input, boolean part2) {

        Map<String, Integer> variables = new HashMap<>();
        int maxValue = Integer.MIN_VALUE;

        for (String line : input) {
            String[] elems = line.split(" ");
            if (elems.length != 7)
                continue;
            String var = elems[0];
            String op1 = elems[1];
            int value1 = Integer.valueOf(elems[2]);
            String lh = elems[4];
            String op2 = elems[5];
            int value2 = Integer.valueOf(elems[6]);

            int lh_value = variables.getOrDefault(lh, 0);
            boolean condition = false;
            switch (op2) {
            case "<":
                condition = (lh_value < value2);
                break;
            case ">":
                condition = (lh_value > value2);
                break;
            case ">=":
                condition = (lh_value >= value2);
                break;
            case "<=":
                condition = (lh_value <= value2);
                break;
            case "!=":
                condition = (lh_value != value2);
                break;
            case "==":
                condition = (lh_value == value2);
                break;
            default:
                fail("unsupported operand: " + op2);
            }

            if (condition) {
                // Condition is true, perform operation on left.

                // Read the previous value of the variable we are going to
                // change.
                int value = variables.getOrDefault(var, 0);
                switch (op1) {
                case "dec":
                    value -= value1;
                    break;
                case "inc":
                    value += value1;
                    break;
                default:
                    fail("unsupported operand: " + op1);
                }

                if (part2) {
                    maxValue = Math.max(value, maxValue);
                }

                variables.put(var, value);
            }
        }

        if (!part2) {
            maxValue = variables.values().stream().mapToInt(n -> n).max()
                    .getAsInt();
        }

        return maxValue;
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2017, 8, "I Heard You Like Registers", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(6611, 6619);
    }

    @Override
    public List<String> parse(Optional<File> file) throws IOException {
        return InputUtils.asStringList(file.get());
    }

    @Override
    public Integer part1(List<String> input) {
        return interpret(input, false);
    }

    @Override
    public Integer part2(List<String> input) {
        return interpret(input, true);
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day08());
    }
}
