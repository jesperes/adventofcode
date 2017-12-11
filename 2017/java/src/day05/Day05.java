package day05;

import static org.junit.Assert.*;

import java.io.BufferedReader;
import java.io.InputStreamReader;

import org.junit.Test;

public class Day05 {

	public int execute(int[] program) {
		int pc = 0;
		int steps = 0;

		while (pc >= 0 && pc < program.length) {
			// System.out.format("Step %d: %s%n", steps,
			// Arrays.toString(program));

			// Each instruction is a relative jump.
			int nextPc = pc + program[pc];

			// Each jump/offset is incremented by one
			// after taking it.
			program[pc]++;

			// Update pc/steps
			pc = nextPc;
			steps++;
		}

		return steps;
	}

	public int executePart2(int[] program) {
		int pc = 0;
		int steps = 0;

		while (pc >= 0 && pc < program.length) {
			// System.out.format("Step %d: %s%n", steps,
			// Arrays.toString(program));

			// Each instruction is a relative jump.
			int offset = program[pc];
			int nextPc = pc + offset;

			// Increment each offset after taking it. If the
			// offset is 3 or more, instead decrease it by
			// 1.
			if (offset >= 3)
				program[pc]--;
			else
				program[pc]++;

			// Update pc/steps
			pc = nextPc;
			steps++;
		}

		return steps;
	}

	@Test
	public void testPart1_short() {
		assertEquals(5, execute(new int[] { 0, 3, 0, 1, -3 }));
	}

	@Test
	public void testPart2_short() {
		assertEquals(10, executePart2(new int[] { 0, 3, 0, 1, -3 }));
	}

	@Test
	public void testPart1_full() throws Exception {
		try (BufferedReader inputReader = new BufferedReader(
				new InputStreamReader(Day05.class.getClassLoader()
						.getResourceAsStream("day05/input.txt")))) {
			int[] program = inputReader.lines().map(s -> Integer.valueOf(s))
					.mapToInt(n -> n.intValue()).toArray();
			int steps = execute(program);
			System.out.println("[Day05]: Steps: " + steps);
		}
	}

	@Test
	public void testPart2_full() throws Exception {
		try (BufferedReader inputReader = new BufferedReader(
				new InputStreamReader(Day05.class.getClassLoader()
						.getResourceAsStream("day05/input.txt")))) {
			int[] program = inputReader.lines().map(s -> Integer.valueOf(s))
					.mapToInt(n -> n.intValue()).toArray();
			int steps = executePart2(program);
			System.out.println("[Day05]: Steps (part 2): " + steps);
		}
	}
}
