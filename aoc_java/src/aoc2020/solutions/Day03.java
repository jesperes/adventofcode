package aoc2020.solutions;

import java.io.BufferedReader;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

import aoc2020.AocPuzzleInfo;
import aoc2020.AocResult;
import aoc2020.IAocPuzzle;
import aoc2020.InputUtils;
import aoc2020.solutions.Day03.InputData;

public class Day03 implements IAocPuzzle<InputData, Long, Long> {

	record Coord(int x, int y) {

	}

	record InputData(Set<Coord> trees, int numLines, int width) {

	}

	@Override
	public InputData parse(Optional<BufferedReader> reader) {
		Set<Coord> trees = new HashSet<Coord>();
		int y = 0;
		int width = 0;
		for (String line : InputUtils.asStringList(reader.get())) {
			int x = 0;
			width = line.length();
			for (char c : line.toCharArray()) {
				if (c == '#')
					trees.add(new Coord(x, y));
				x++;
			}
			y++;
		}
		return new InputData(trees, y, width);
	}

	@Override
	public Long part1(InputData input) {
		return treesAlongSlope(new Coord(3, 1), input);
	}

	@Override
	public Long part2(InputData input) {
		return treesAlongSlope(new Coord(1, 1), input) * //
				treesAlongSlope(new Coord(3, 1), input) * //
				treesAlongSlope(new Coord(5, 1), input) * //
				treesAlongSlope(new Coord(7, 1), input) * //
				treesAlongSlope(new Coord(1, 2), input);
	}

	private long treesAlongSlope(Coord slope, InputData input) {
		long trees = 0;
		int x = 0;
		for (int y = 0; y < input.numLines;) {
			x = (x + slope.x) % input.width;
			y += slope.y;

			Coord c = new Coord(x, y);
			if (input.trees.contains(c)) {
				trees++;
			}
		}
		return trees;
	}

	@Override
	public AocResult<Long, Long> getExpected() {
		return AocResult.of(230L, 9533698720L);
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2020, 3, "Toboggan Trajectory");
	}
}
