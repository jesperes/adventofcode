package aoc2016;

import static org.junit.Assert.assertArrayEquals;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import org.junit.Test;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;

public class Day18 implements IAocIntPuzzle<String> {

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2016, 18, "Like a Rogue", false);
	}

	@Override
	public AocResult<Integer, Integer> getExpected() {
		return AocResult.of(2035, 20000577);
	}

	@Override
	public String parse(Optional<File> file) throws IOException {
		return ".^..^....^....^^.^^.^.^^.^.....^.^..^...^^^^^^.^^^^.^.^^^^^^^.^^^^^..^.^^^.^^..^.^^.^....^.^...^^.^.";
	}

	@Override
	public Integer part1(String input) {
		return solve(input, 40);
	}

	@Override
	public Integer part2(String input) {
		return solve(input, 400000);
	}

	private int numSafeTiles(char[] line) {
		int n = 0;
		for (char c : line) {
			if (c == '.')
				n++;
		}
		return n;
	}

	/**
	 * Compute the next row and return the number of safe tiles in it.
	 */
	private int nextLine(char[] prev, char[] next) {
		int safe = 0;
		for (int i = 0; i < prev.length; i++) {
			char a = (i <= 0) ? '.' : prev[i - 1];
			char b = prev[i];
			char c = (i >= prev.length - 1) ? '.' : prev[i + 1];

			if (a == b && c != b) {
				next[i] = '^';
			} else if (b == c && a != b) {
				next[i] = '^';
			} else {
				next[i] = '.';
				safe++;
			}
		}
		return safe;
	}

	private int solve(String input, int n) {
		char[] a = input.toCharArray();
		char[] b = new char[a.length];
		int count = numSafeTiles(a);
		for (int i = 0; i < n - 1; i++) {
			count += nextLine(a, b);
			var tmp = a;
			a = b;
			b = tmp;
		}
		return count;
	}

	@Test
	public void testNextLine() throws Exception {
		char[] line = "..^^.".toCharArray();
		char[] buf = new char[line.length];
		char[] expected = ".^^^^".toCharArray();
		nextLine(line, buf);
		assertArrayEquals(expected, buf);
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day18());
	}
}
