package day08;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import org.junit.Test;

public class Day08 {

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

        System.out.println("Variable values are: " + variables);

        int maxValue = Integer.MIN_VALUE;
        String maxVariable = null;
        for (Entry<String, Integer> e : variables.entrySet()) {
            if (e.getValue() > maxValue) {
                maxValue = e.getValue();
                maxVariable = e.getKey();
            }
        }

        System.out.format("Largest variable is %s = %d%n", maxVariable,
                maxValue);
        return maxValue;
    }

    int interpret_part2(String input) {

        Map<String, Integer> variables = new HashMap<>();
        int maxValue = Integer.MIN_VALUE;
        String maxVariable = null;

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
                    maxVariable = var;
                    maxValue = value;
                }

                variables.put(var, value);
            }
        }

        System.out.format("Largest variable is %s = %d%n", maxVariable,
                maxValue);
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
        try (BufferedReader inputReader = new BufferedReader(
                new InputStreamReader(getClass().getClassLoader()
                        .getResourceAsStream("day08/input.txt")))) {

            System.out.println("[Day08]: Largest value: " + interpret(
                    inputReader.lines().collect(Collectors.joining("\n"))));
        }
    }

    @Test
    public void testPart2_full() throws Exception {
        try (BufferedReader inputReader = new BufferedReader(
                new InputStreamReader(getClass().getClassLoader()
                        .getResourceAsStream("day08/input.txt")))) {

            System.out.println("[Day08]: Largest value (part 2): "
                    + interpret_part2(inputReader.lines()
                            .collect(Collectors.joining("\n"))));
        }
    }
}
