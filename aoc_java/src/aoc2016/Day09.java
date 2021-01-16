package aoc2016;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import org.junit.Test;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;
import common2.InputUtils;

/**
 * Important observation for this puzzle is that we do not need to decompress
 * the data, just compute how large it will be. This is also critical to be able
 * to solve part 2 without consuming too much memory.
 */
public class Day09 implements IAocPuzzle<String, Integer, Long> {
	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2016, 9, "Explosives in Cyberspace", true);
	}

	@Override
	public AocResult<Integer, Long> getExpected() {
		return AocResult.of(74532, 11558231665L);
	}

	@Override
	public String parse(Optional<File> file) throws IOException {
		return InputUtils.asString(file.get());
	}

	/*
	 * =============================================================================
	 * Part 1
	 * =============================================================================
	 */

	@Override
	public Integer part1(String input) {
		int len = 0;

		for (int i = 0; i < input.length(); i++) {
			char c = input.charAt(i);
			if (Character.isWhitespace(c)) {
				continue;
			} else if (c == '(') {
				int[] marker = getMarker(input, i + 1);
				int numChars = marker[MARKER_NUMCHAR];
				int repeatCount = marker[MARKER_REPEATCOUNT];
				int nextIndex = marker[MARKER_NEXTINDEX];
				len += repeatCount * numChars;
				i = nextIndex + numChars - 1;
			} else {
				len++;
			}
		}
		return len;
	}

	/*
	 * =============================================================================
	 * Part 2: Similar to part 1, but markers within compressed data should also be
	 * decompressed.
	 * =============================================================================
	 */

	@Override
	public Long part2(String input) {
		return computeDecompressedLengthOf(input);
	}

	public long computeDecompressedLengthOf(String str) {
		return computeDecompressedLengthOf(str, 0, str.length());
	}

	public long computeDecompressedLengthOf(String str, int start, int length) {
		long len = 0;

		for (int i = start; i < start + length; i++) {
			char c = str.charAt(i);

			if (Character.isWhitespace(c)) {
				continue;
			} else if (c == '(') {
				int marker[] = getMarker(str, i + 1);
				int numChars = marker[MARKER_NUMCHAR];
				int repeatCount = marker[MARKER_REPEATCOUNT];
				int nextIndex = marker[MARKER_NEXTINDEX];

				long decomplen = computeDecompressedLengthOf(str, nextIndex, numChars);
				len += repeatCount * decomplen;
				i = nextIndex + numChars - 1;
			} else {
				len += 1;
			}
		}

		return len;
	}

	/*
	 * =============================================================================
	 * Helpers
	 * =============================================================================
	 */

	final static int MARKER_NUMCHAR = 0;
	final static int MARKER_REPEATCOUNT = 1;
	final static int MARKER_NEXTINDEX = 2;

	int[] getMarker(String s, int pos) {
		int last = -1;

		for (int i = pos; i < s.length(); i++) {
			if (s.charAt(i) == ')') {
				last = i;
				break;
			}
		}

		String[] elems = s.substring(pos, last).split("x");
		int[] result = new int[] { Integer.valueOf(elems[0]), Integer.valueOf(elems[1]), last + 1 };
		return result;
	}

	/*
	 * =============================================================================
	 * Tests
	 * =============================================================================
	 */

	@Test
	public void testDecompress() throws Exception {
		assertEquals(9L, computeDecompressedLengthOf("(3x3)ABC"));
		assertEquals("XABCABCABCABCABCABCY".length(), computeDecompressedLengthOf("X(8x2)(3x3)ABCY"));
		assertEquals(241920L, computeDecompressedLengthOf("(27x12)(20x12)(13x14)(7x10)(1x12)A"));
		assertEquals(445L, computeDecompressedLengthOf("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"));
	}

	@Test
	public void testGetMarker() throws Exception {
		assertArrayEquals(new int[] { 3, 4, 8 }, getMarker("abc(3x4)def", 4));
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day09());
	}
}
