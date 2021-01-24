package aoc2015;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;

public class Day11 implements IAocPuzzle<String, String, String> {

	static final String INPUT = "cqjxjnds";

	private void increment(StringBuilder s) {
		for (int i = s.length() - 1; i >= 0; i--) {
			char c = s.charAt(i);
			if (c < 'z') {
				s.setCharAt(i, (char) (c + 1));
				break;
			} else {
				s.setCharAt(i, 'a');
			}
		}
	}

	private boolean hasStraight(String s) {
		for (int i = 2; i < s.length(); i++) {
			char a = s.charAt(i - 2);
			char b = s.charAt(i - 1);
			char c = s.charAt(i);
			if (a + 1 == b && b + 1 == c)
				return true;
		}
		return false;
	}

	private boolean hasConfusingChars(String s) {
		for (char c : s.toCharArray()) {
			switch (c) {
			case 'i':
			case 'o':
			case 'l':
				return true;
			}
		}
		return false;
	}

	private boolean hasPairs(String s) {
		int n = 0;
		for (int i = 1; i < s.length();) {
			if (s.charAt(i) == s.charAt(i - 1)) {
				n++;
				i += 2;

				if (n >= 2)
					return true;
			} else {
				i++;
			}
		}
		return false;
	}

	private void nextPassword(StringBuilder b) {
		while (true) {
			increment(b);
			String s = b.toString();

			if (hasStraight(s) && !hasConfusingChars(s) && hasPairs(s)) {
				return;
			}
		}
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2015, 11, "Corporate Policy", false);
	}

	@Override
	public AocResult<String, String> getExpected() {
		return AocResult.of("cqjxxyzz", "cqkaabcc");
	}

	@Override
	public String parse(Optional<File> file) throws IOException {
		return "cqjxjnds";
	}

	@Override
	public String part1(String input) {
		StringBuilder password = new StringBuilder(INPUT);
		nextPassword(password);
		return password.toString();
	}

	@Override
	public String part2(String input) {
		StringBuilder password = new StringBuilder(getExpected().p1().get());
		nextPassword(password);
		return password.toString();
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day11());
	}
}
