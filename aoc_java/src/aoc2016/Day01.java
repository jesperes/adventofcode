package aoc2016;

import static java.lang.Math.abs;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day01 implements IAocIntPuzzle<List<String>> {

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2016, 1, "No Time for a Taxicab", true);
	}

	@Override
	public AocResult<Integer, Integer> getExpected() {
		return AocResult.of(278, 161);
	}

	@Override
	public List<String> parse(Optional<File> file) throws IOException {
		return Arrays.stream(InputUtils.asString(file.get()).split("[ ,]+"))
				.map(s -> s.trim()).collect(Collectors.toUnmodifiableList());
	}

	String key(int x, int y) {
		return x + "-" + y;
	}

	@Override
	public Integer part1(List<String> input) {
		int x = 0;
		int y = 0;
		int dir = 0;
		for (String s : input) {
			dir = (s.charAt(0) == 'R' ? dir + 1 : dir + 3) % 4;
			int steps = Integer.valueOf(s.substring(1));
			int dx = 0, dy = 0;

			switch (dir) {
			case 0: // N
				dy = -1;
				break;
			case 1: // E
				dx = 1;
				break;
			case 2: // S
				dy = 1;
				break;
			case 3: // W
				dx = -1;
				break;
			}

			for (int i = 0; i < steps; i++) {
				x += dx;
				y += dy;
			}
		}
		return abs(x) + abs(y);
	}

	@Override
	public Integer part2(List<String> input) {
		Set<String> visited = new HashSet<>();
		int visitedTwice = -1;

		int x = 0;
		int y = 0;
		int dir = 0;
		visited.add(key(x, y));

		for (String s : input) {
			dir = (s.charAt(0) == 'R' ? dir + 1 : dir + 3) % 4;
			int steps = Integer.valueOf(s.substring(1));
			int dx = 0, dy = 0;

			switch (dir) {
			case 0: // N
				dy = -1;
				break;
			case 1: // E
				dx = 1;
				break;
			case 2: // S
				dy = 1;
				break;
			case 3: // W
				dx = -1;
				break;
			}

			for (int i = 0; i < steps; i++) {
				x += dx;
				y += dy;

				if (visitedTwice == -1) {
					if (visited.contains(key(x, y))) {
						visitedTwice = abs(x) + abs(y);
					} else {
						visited.add(key(x, y));
					}
				}
			}
		}
		return visitedTwice;
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day01());
	}
}
