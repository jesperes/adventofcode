package aoc2020.solutions;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import aoc2020.AocPuzzleInfo;
import aoc2020.AocResult;
import aoc2020.IAocPuzzle;
import aoc2020.solutions.Day02.PasswordData;

public class Day02 implements IAocPuzzle<List<PasswordData>, Long, Long> {

	class PasswordData {
		int from;
		int to;
		char c;
		String password;

		public PasswordData(String s) {
			String[] elems = s.split("[:\\- ]+");
			assertEquals(4, elems.length);
			from = Integer.valueOf(elems[0]);
			to = Integer.valueOf(elems[1]);
			c = elems[2].charAt(0);
			password = elems[3];
		}
	}

	@Override
	public List<PasswordData> parse(Optional<BufferedReader> reader) {
		return reader.get().lines().map(s -> new PasswordData(s)).collect(Collectors.toList());
	}

	@Override
	public Long part1(List<PasswordData> input) {
		return input.stream().filter(data -> isValidPart1(data)).count();
	}

	@Override
	public Long part2(List<PasswordData> input) {
		return input.stream().filter(data -> isValidPart2(data)).count();
	}

	@Override
	public AocResult<Long, Long> getExpected() {
		return AocResult.of(660L, 530L);
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2020, 2, "Password Philosophy");
	}

	/*
	 * Helpers
	 */

	private boolean isValidPart1(PasswordData data) {
		int n = 0;
		for (char c : data.password.toCharArray()) {
			if (c == data.c)
				n++;
		}
		return n >= data.from && n <= data.to;
	}

	private boolean isValidPart2(PasswordData data) {
		char[] a = data.password.toCharArray();
		boolean c1 = a[data.from - 1] == data.c;
		boolean c2 = a[data.to - 1] == data.c;
		return (c1 == true && c2 == false) || (c1 == false && c2 == true);
	}
}
