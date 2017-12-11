package day10;

import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.junit.Test;

public class Day10 {

	int pos = 0;
	int skip = 0;

	int[] makeInput(int n) {
		return IntStream.range(0, n).toArray();
	}

	int[] reverse(int[] buf, int start, int len) {
		for (int i = 0; i < len / 2; i++) {
			int a = (start + i) % buf.length;
			int b = (start + len - 1 - i) % buf.length;
			int tmp = buf[a];
			buf[a] = buf[b];
			buf[b] = tmp;
		}

		return buf;
	}

	int knotHashSingleIter(int input[], int lengths[]) {

		for (int i = 0; i < lengths.length; i++) {
			reverse(input, pos, lengths[i]);

			// Move the current position forward
			pos = (pos + lengths[i] + skip) % input.length;

			// Increment the skip length
			skip++;
		}

		return input[0] * input[1];
	}

	int[] makeLengths(String str) {
		int[] lengths = new int[str.length() + 5];
		int i = 0;
		for (byte c : str.getBytes()) {
			lengths[i++] = c;
		}
		lengths[i++] = 17;
		lengths[i++] = 31;
		lengths[i++] = 73;
		lengths[i++] = 47;
		lengths[i++] = 23;
		return lengths;
	}

	int computeDenseHash(int[] hash, int start, int length) {
		int dense = 0;
		for (int i = 0; i < length; i++) {
			dense ^= hash[start + i];
		}
		return dense;
	}

	String knotHash(String inputStr) {
		int[] input = makeInput(256);
		int[] lengths = makeLengths(inputStr);

		pos = 0;
		skip = 0;

		for (int i = 0; i < 64; i++) {
			knotHashSingleIter(input, lengths);
		}

		// compute the dense hash
		int[] dense = new int[16];
		for (int i = 0; i < 16; i++) {
			dense[i] = computeDenseHash(input, i * 16, 16);
		}
		
		return computeHexString(dense);
	}

	private String computeHexString(int[] dense) {
		return Arrays.stream(dense).
				mapToObj(n -> String.format("%02x", n)).collect(Collectors.joining());
	}

	@Test
	public void testReverse() throws Exception {
		assertArrayEquals(new int[] { 4, 3, 2, 1 }, reverse(new int[] { 1, 2, 3, 4 }, 0, 4));
		assertArrayEquals(new int[] { 3, 2, 1, 4 }, reverse(new int[] { 1, 2, 3, 4 }, 2, 3));
		assertArrayEquals(new int[] { 6, 5, 3, 4, 2, 1 }, reverse(new int[] { 1, 2, 3, 4, 5, 6 }, 4, 4));
	}

	@Test
	public void testComputeDenseHash() throws Exception {
		assertEquals(64, computeDenseHash(new int[] { 65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22 }, 0, 16));
	}

	@Test
	public void testPart1_small() throws Exception {
		assertEquals(12, knotHashSingleIter(makeInput(5), new int[] { 3, 4, 1, 5 }));
	}

	@Test
	public void testPart1_full() throws Exception {
		assertEquals(38628, knotHashSingleIter(makeInput(256),
				new int[] { 130, 126, 1, 11, 140, 2, 255, 207, 18, 254, 246, 164, 29, 104, 0, 224 }));
	}

	@Test
	public void testMakeLengths() throws Exception {
		assertArrayEquals(new int[] { 49, 44, 50, 44, 51, 17, 31, 73, 47, 23 }, makeLengths("1,2,3"));
	}

	@Test
	public void testPart2_short() throws Exception {
		assertEquals("a2582a3a0e66e6e86e3812dcb672a272", knotHash(""));
		assertEquals("33efeb34ea91902bb2f59c9920caa6cd", knotHash("AoC 2017"));
		assertEquals("3efbe78a8d82f29979031a4aa0b16a9d", knotHash("1,2,3"));
		assertEquals("63960835bcdc130f0b66d7ff4f6a5a8e", knotHash("1,2,4"));
	}
	
	@Test
	public void testPart2_full() throws Exception {
		String input = "130,126,1,11,140,2,255,207,18,254,246,164,29,104,0,224";
		System.out.format("[Day10] Hash of %s = %s", input, knotHash(input));
	}
}
