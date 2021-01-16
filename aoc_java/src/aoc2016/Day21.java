package aoc2016;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.Test;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;
import common2.InputUtils;

public class Day21 implements IAocPuzzle<List<String>, String, String> {

	@FunctionalInterface
	interface Scrambler {
		void scramble(Matcher m, StringBuilder s);
	}

	static Map<Pattern, Scrambler> rules = new HashMap<>();

	static {
		rules.put(Pattern.compile("swap position (\\d+) with position (\\d+)"), (Matcher m, StringBuilder s) -> {
			int x = Integer.valueOf(m.group(1));
			int y = Integer.valueOf(m.group(2));
			swapPos(s, x, y);
		});
		rules.put(Pattern.compile("swap letter ([a-z]) with letter ([a-z])"), (Matcher m, StringBuilder s) -> {
			char a = m.group(1).charAt(0);
			char b = m.group(2).charAt(0);
			swapLetter(s, a, b);
		});
		rules.put(Pattern.compile("rotate (left|right) (\\d+) step(s?)"), (Matcher m, StringBuilder s) -> {
			String dir = m.group(1);
			int steps = Integer.valueOf(m.group(2));
			rotate(s, dir, steps);
		});
		rules.put(Pattern.compile("rotate based on position of letter ([a-z])"), (Matcher m, StringBuilder s) -> {
			char letter = m.group(1).charAt(0);
			rotateBasedOnLetter(s, letter);
		});
		rules.put(Pattern.compile("reverse positions (\\d+) through (\\d+)"), (Matcher m, StringBuilder s) -> {
			int x = Integer.valueOf(m.group(1));
			int y = Integer.valueOf(m.group(2));
			reverse(s, x, y);
		});
		rules.put(Pattern.compile("move position (\\d+) to position (\\d+)"), (Matcher m, StringBuilder s) -> {
			int x = Integer.valueOf(m.group(1));
			int y = Integer.valueOf(m.group(2));
			move(s, x, y);
		});
	}

	private static void rotate(StringBuilder s, String dir, int steps) {
		steps = steps % s.length();
		switch (dir) {
		case "left": {
			String a = s.substring(0, steps);
			s.delete(0, steps);
			s.append(a);
			break;
		}
		case "right": {
			int l = s.length();
			String a = s.substring(l - steps, l);
			s.delete(l - steps, l);
			s.insert(0, a);
		}
		}
	}

	private static void swapPos(StringBuilder s, int x, int y) {
		char c_x = s.charAt(x);
		char c_y = s.charAt(y);

		s.setCharAt(x, c_y);
		s.setCharAt(y, c_x);
	}

	private static void swapLetter(StringBuilder s, char a, char b) {
		for (int i = 0; i < s.length(); i++) {
			if (s.charAt(i) == a)
				s.setCharAt(i, b);
			else if (s.charAt(i) == b)
				s.setCharAt(i, a);
		}
	}

	private static void reverse(StringBuilder s, int x, int y) {
		int half = (y - x + 1) / 2;
		for (int i = 0; i < half; i++) {
			swapPos(s, x + i, y - i);
		}
	}

	private static void move(StringBuilder s, int x, int y) {
		char c = s.charAt(x);
		s.deleteCharAt(x);
		s.insert(y, c);
		assertEquals(c, s.charAt(y));
	}

	private static void rotateBasedOnLetter(StringBuilder s, char letter) {
		int index = s.toString().indexOf(letter);
		rotate(s, "right", 1 + index + (index >= 4 ? 1 : 0));
	}

	private static String scramble(String s, List<String> list) {
		StringBuilder string = new StringBuilder(s);

		for (String line : list) {
			int numMatches = 0;

			for (Entry<Pattern, Scrambler> e : rules.entrySet()) {
				Matcher m = e.getKey().matcher(line);
				if (m.matches()) {
					e.getValue().scramble(m, string);
					numMatches++;
				}
			}

			if (numMatches == 0) {
				fail();
			}
		}
		return string.toString();
	}

	private static List<String> permutation(String str) {
		List<String> list = new ArrayList<>();
		permutation("", str, list);
		return list;
	}

	private static void permutation(String prefix, String str, List<String> acc) {
		int n = str.length();
		if (n == 0)
			acc.add(prefix);
		else {
			for (int i = 0; i < n; i++)
				permutation(prefix + str.charAt(i), str.substring(0, i) + str.substring(i + 1, n), acc);
		}
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2016, 21, "Scrambled Letters and Hash", true);
	}

	@Override
	public AocResult<String, String> getExpected() {
		return AocResult.of("cbeghdaf", "bacdefgh");
	}

	@Override
	public List<String> parse(Optional<File> file) throws IOException {
		return InputUtils.asStringList(file.get());
	}

	@Override
	public String part1(List<String> input) {
		return scramble("abcdefgh", input);
	}

	@Override
	public String part2(List<String> input) {
		String scrambled = "fbgdceah";
		List<String> list = permutation(scrambled);

		for (String s : list) {
			String p = scramble(s, input);
			if (p.equals(scrambled)) {
				return s;
			}
		}
		fail();
		return "";
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day21());
	}

	/*
	 * =========================================================================
	 * Tests
	 * =========================================================================
	 */

	@Test
	public void testStringOps() throws Exception {
		StringBuilder s = new StringBuilder("abcde");
		swapPos(s, 0, 4);
		assertEquals("ebcda", s.toString());

		swapLetter(s, 'd', 'b');
		assertEquals("edcba", s.toString());

		reverse(s, 0, 4);
		assertEquals("abcde", s.toString());

		rotate(s, "left", 1);
		assertEquals("bcdea", s.toString());

		move(s, 1, 4);
		assertEquals("bdeac", s.toString());

		move(s, 3, 0);
		assertEquals("abdec", s.toString());

		rotateBasedOnLetter(s, 'b');
		assertEquals("ecabd", s.toString());

		rotateBasedOnLetter(s, 'd');
		assertEquals("decab", s.toString());

		// reverse
		s.setLength(0);
		s.append("abcdefgh");
		reverse(s, 2, 6);
		assertEquals("abgfedch", s.toString());

		s = new StringBuilder("abcfdhge");
		reverse(s, 1, 6);
		assertEquals("aghdfcbe", s.toString());
	}

}
