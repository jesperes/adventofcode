package aoc2016;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day07 implements IAocIntPuzzle<List<String>> {

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2016, 7, "Internet Protocol Version 7", true);
	}

	@Override
	public AocResult<Integer, Integer> getExpected() {
		return AocResult.of(115, 231);
	}

	@Override
	public List<String> parse(Optional<File> file) throws IOException {
		return InputUtils.asStringList(file.get());
	}

	private boolean supportsTLS(String line) {
		boolean sq = false;
		boolean hasValidAbba = false;

		for (int i = 0; i < line.length(); i++) {
			char c = line.charAt(i);
			if (c == '[') {
				sq = true;
			} else if (c == ']') {
				sq = false;
			}

			if (i + 4 > line.length())
				break;

			char c1 = c;
			char c2 = line.charAt(i + 1);
			char c3 = line.charAt(i + 2);
			char c4 = line.charAt(i + 3);

			boolean hasAbba = (c1 != c2) && c3 == c2 && c4 == c1;
			if (hasAbba) {
				if (sq)
					return false;
				else
					hasValidAbba = true;
			}

		}
		return hasValidAbba;

	}

	private boolean supportsSSL(String line) {
		boolean sq = false;

		// Split supernet and hypernet strings first, this makes
		// the logic below a bit more straight forward.

		StringBuilder supernet = new StringBuilder();
		StringBuilder hypernet = new StringBuilder();

		for (char c : line.toCharArray()) {
			if (sq) {
				hypernet.append(c);
			} else {
				supernet.append(c);
			}

			if (c == '[') {
				sq = true;
			} else if (c == ']') {
				sq = false;
			}
		}

		char[] spr = supernet.toString().toCharArray();
		for (int i = 0; i < spr.length; i++) {
			if (i + 3 > spr.length)
				break;

			char a = spr[i];
			char b = spr[i + 1];
			if (a != b && spr[i + 2] == a) {
				// found a ABA sequence in the supernet string, locate
				// BAB sequence in hypernet string.

				char[] hpr = hypernet.toString().toCharArray();
				for (int j = 0; j < hpr.length; j++) {
					if (j + 3 > hpr.length)
						break;

					if (hpr[j] == b && hpr[j + 1] == a && hpr[j + 2] == b) {
						return true;
					}
				}
			}
		}
		return false;
	}

	@Override
	public Integer part1(List<String> input) {
		return (int) input.stream().filter(line -> supportsTLS(line)).count();
	}

	@Override
	public Integer part2(List<String> input) {
		return (int) input.stream().filter(line -> supportsSSL(line)).count();
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day07());
	}
}
