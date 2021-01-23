package aoc2016;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import org.junit.Test;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;

public class Day16 implements IAocPuzzle<String, String, String> {

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2016, 16, "Dragon Checksum", false);
	}

	@Override
	public AocResult<String, String> getExpected() {
		return AocResult.of("10100101010101101", "01100001101101001");
	}

	@Override
	public String parse(Optional<File> file) throws IOException {
		return "11101000110010100";
	}

	@Override
	public String part1(String input) {
		return checksum(fill(input, 272));
	}

	private String checksum(StringBuilder a) {
		while (true) {
			checksum1(a);
			if (a.length() % 2 == 1) {
				return a.toString();
			}
		}
	}

	private void checksum1(StringBuilder a) {
		for (int i = 0, j = 0; i < a.length() - 1; i += 2, j++) {
			char c1 = a.charAt(i);
			char c2 = a.charAt(i + 1);
			if (c1 == c2) {
				a.setCharAt(j, '1');
			} else {
				a.setCharAt(j, '0');
			}
		}
		a.setLength(a.length() / 2);
	}

	StringBuilder fill(String input, int len) {
		StringBuilder a = new StringBuilder(input);

		while (a.length() < len) {
			int n = a.length();
			a.append("0");
			for (int i = n - 1; i >= 0; i--) {
				if (a.charAt(i) == '0')
					a.append("1");
				else
					a.append("0");
			}
		}

		a.setLength(len);
		return a;
	}

	@Override
	public String part2(String input) {
		return checksum(fill(input, 35651584));
	}

	@Test
	public void testFill() throws Exception {
		assertEquals("100", fill("1", 3).toString());
		assertEquals("001", fill("0", 3).toString());
		assertEquals("11111000000", fill("11111", 11).toString());
		assertEquals("1111000010100101011110000", fill("111100001010", 25).toString());
	}

	@Test
	public void testChecksum() throws Exception {
		assertEquals("100", checksum(new StringBuilder("110010110100")).toString());
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day16());
	}
}
