package aoc2017;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;
import common2.InputUtils;

public class Day19 implements IAocPuzzle<String[], String, Integer> {

	static enum Direction {
		North, South, East, West
	}

	static class Point {
		int row, col;

		public Point(int row, int col) {
			this.row = row;
			this.col = col;
		}

		public boolean inside(String[] diagram) {
			return row >= 0 && row < diagram.length && col >= 0
					&& col < diagram[0].length();
		}

		public char getChar(String[] diagram) {
			return diagram[row].charAt(col);
		}

		public Point getPosition(Direction dir) {
			switch (dir) {
			case East:
				return new Point(row, col + 1);
			case North:
				return new Point(row - 1, col);
			case South:
				return new Point(row + 1, col);
			case West:
				return new Point(row, col - 1);
			default:
				return null;
			}
		}

		public Direction turn(Direction from, String[] input) {
			switch (from) {
			case East:
			case West:
				Point northwards = new Point(row - 1, col);
				if (northwards.inside(input)) {
					char toNorth = northwards.getChar(input);
					if (toNorth == '|' || Character.isLetter(toNorth)) {
						return Direction.North;
					}
				}

				Point southwards = new Point(row + 1, col);
				if (southwards.inside(input)) {
					char toSouth = southwards.getChar(input);
					if (toSouth == '|' || Character.isLetter(toSouth)) {
						return Direction.South;
					}
				}
				break;
			case North:
			case South:
				Point eastwards = new Point(row, col + 1);
				if (eastwards.inside(input)) {
					char toEast = eastwards.getChar(input);
					if (toEast == '-' || Character.isLetter(toEast)) {
						return Direction.East;
					}
				}

				Point westwards = new Point(row, col - 1);
				if (westwards.inside(input)) {
					char toWest = westwards.getChar(input);
					if (toWest == '-' || Character.isLetter(toWest)) {
						return Direction.West;
					}
				}
				break;
			}

			throw new RuntimeException();
		}
	}

	private int followTube(String[] input, StringBuilder builder) {
		String first = input[0];

		int row = 0;
		int col = first.indexOf('|');
		Point p = new Point(row, col);
		Direction dir = Direction.South;
		int steps = 0;

		while (true) {
			if (!p.inside(input))
				return steps;

			char c = p.getChar(input);
			if (c == ' ')
				return steps;

			// All non-space chars on the diagram are counted as steps.
			steps++;

			if (Character.isLetter(c)) {
				// Collect character and continue in the same direction
				builder.append(c);
				p = p.getPosition(dir);
				continue;
			}

			if (c == '+') {
				dir = p.turn(dir, input);
				p = p.getPosition(dir);
				continue;
			}

			if (c == '|' || c == '-') {
				p = p.getPosition(dir);
				continue;
			}
		}
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2017, 19, "A Series of Tubes", true);
	}

	@Override
	public AocResult<String, Integer> getExpected() {
		return AocResult.of("EPYDUXANIT", 17544);
	}

	@Override
	public String[] parse(Optional<File> file) throws IOException {
		return InputUtils.asStringList(file.get()).toArray(n -> new String[n]);
	}

	@Override
	public String part1(String[] input) {
		StringBuilder builder = new StringBuilder();
		followTube(input, builder);
		return builder.toString();
	}

	@Override
	public Integer part2(String[] input) {
		return followTube(input, new StringBuilder());
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day19());
	}
}
