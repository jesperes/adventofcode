package day11;

import static org.junit.Assert.*;

import java.util.List;
import java.util.Optional;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class Day11AltTests {

	Day11Alt day11 = new Day11Alt();

	private List<String> showMoves(String input) {
		List<String> moves = day11.getMoves(input);

		System.out.println("Input state:");
		System.out.println(day11.showState(input));

		System.out.println("Output states:");
		for (String move : moves) {
			System.out.println(day11.showState(move));
		}
		return moves;
	}

	@Rule
	public TestName nameRule = new TestName();

	@Before
	public void before() {
		System.out.format("==== BEGIN %s ====%n", nameRule.getMethodName());
	}

	@After
	public void after() {
		System.out.format("==== AFTER %s ====%n", nameRule.getMethodName());
	}

	@Test
	public void testIsSolution() throws Exception {
		assertFalse(day11.isSolution(day11.inputState));
		assertTrue(day11.isSolution("2/4/4/4/4"));
		assertTrue(day11.isSolution("x/4/4"));
		assertFalse(day11.isSolution("x/1/2/4/4"));
		assertFalse(day11.isSolution("x/4/4/1/2"));
	}

	@Test
	public void testIsValidMove() throws Exception {
		assertTrue(day11.isValidMove("4/4/4/4/4"));
		assertTrue(day11.isValidMove("4/4/3/4/4"));
		assertTrue(day11.isValidMove("4/4/3/4/3"));
		assertFalse(day11.isValidMove("4/3/4/4/3"));
	}

	@Test
	public void testComputeNewState() throws Exception {
		day11.components = new String[] { "E", "HG", "HM", "LG", "LM" };

		assertEquals("2/2/2/3/4",
				day11.computeNewState("1/1/2/3/4", 2, day11.setOf("HG")));
		assertEquals("2/2/2/2/4",
				day11.computeNewState("1/2/1/1/4", 2, day11.setOf("HM", "LG")));
	}

	@Test
	public void testGetMoves() throws Exception {
		day11.components = new String[] { "E", "HG", "HM", "LG", "LM" };
		String input = "1/2/1/3/1";
		List<String> moves = showMoves(input);
		assertEquals(1, moves.size());
	}

	@Test
	public void testGetMoves2() throws Exception {
		day11.components = new String[] { "E", "HG", "HM", "LG", "LM" };
		String input = "3/4/3/4/3";

		// The solution state should be somewhere in there
		assertTrue(day11.getMoves(input).stream()
				.filter(s -> day11.isSolution(s)).findAny().isPresent());
	}

	@Test
	public void testBFS1() throws Exception {
		day11.components = new String[] { "E", "HG", "HM", "LG", "LM" };
		String input;

		input = "3/4/3/4/3";
		assertTrue(day11.bfs(input).isPresent());

		input = "4/4/3/4/4";
		assertTrue(day11.bfs(input).isPresent());

		input = "3/3/3/3/4";
		assertTrue(day11.bfs(input).isPresent());
	}

	@Test
	public void testBFS2() throws Exception {
		day11.components = new String[] { "E", "HG", "HM", "LG", "LM" };
		String input = day11.inputState;
		Optional<List<String>> solution = day11.bfs(input);
		solution.get().stream()
				.forEach(s -> System.out.println(day11.showState(s)));
		assertEquals(11, solution.get().size());
	}

	@Test
	public void testPart1() throws Exception {
		/*
		 * The first floor contains a strontium generator, a
		 * strontium-compatible microchip, a plutonium generator, and a
		 * plutonium-compatible microchip.
		 * 
		 * The second floor contains a thulium generator, a ruthenium generator,
		 * a ruthenium-compatible microchip, a curium generator, and a
		 * curium-compatible microchip.
		 * 
		 * The third floor contains a thulium-compatible microchip.
		 * 
		 * The fourth floor contains nothing relevant.
		 */
		day11.components = new String[] { "E", "SG", "SM", "PG", "PM", "TG",
				"TM", "RG", "RM", "CG", "CM" };

		String input = "1/"// elevator starts on floor 1
				+ "1/1/" // SG SM
				+ "1/1/" // PG PM
				+ "2/3/" // TG TM
				+ "2/2/" // RG RM
				+ "2/2"; // CG CM

		System.out.println(day11.showState(input));

		assertTrue(day11.isValidMove(input));

		// List<String> moves = day11.getMoves(input);

		// showMoves(input);

		Optional<List<String>> solution = day11.bfs(input);
		solution.get().stream()
				.forEach(s -> System.out.println(day11.showState(s)));
		System.out.println(solution.get());
	}
}
