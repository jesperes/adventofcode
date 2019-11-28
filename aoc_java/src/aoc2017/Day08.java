package aoc2017;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.junit.Test;

import common.AocPuzzle;

public class Day08 extends AocPuzzle {

    public Day08() {
        super(2017, 8);
    }

    // @formatter:off
	String PUZZLE_INPUT = 
			"b inc 5 if a > 1\r\n" + 
			"a inc 1 if b < 5\r\n" + 
			"c dec -10 if a >= 1\r\n" + 
			"c inc -20 if c == 10";
	// @formatter:on

    int interpret(String input) {

        Map<String, Integer> variables = new HashMap<>();

        for (String line : input.split("[\\r\\n]")) {
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

                variables.put(var, value);
            }
        }

        int maxValue = Integer.MIN_VALUE;

        for (Entry<String, Integer> e : variables.entrySet()) {
            if (e.getValue() > maxValue) {
                maxValue = e.getValue();
            }
        }

        return maxValue;
    }

    int interpret_part2(String input) {

        Map<String, Integer> variables = new HashMap<>();
        int maxValue = Integer.MIN_VALUE;
        for (String line : input.split("[\\r\\n]")) {
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

                if (value > maxValue) {
                    maxValue = value;
                }

                variables.put(var, value);
            }
        }

        return maxValue;
    }

    @Test
    public void testPart1_short() throws Exception {
        assertEquals(1, interpret(PUZZLE_INPUT));
    }

    @Test
    public void testPart2_short() throws Exception {
        assertEquals(10, interpret_part2(PUZZLE_INPUT));
    }

    @Test
    public void testPart1_full() throws Exception {
        assertEquals(6611, interpret(getInputAsString()));
    }

    @Test
    public void testPart2_full() throws Exception {
        assertEquals(6619, interpret_part2(getInputAsString()));
    }
}
