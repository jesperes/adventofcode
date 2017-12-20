package day19;

import org.junit.Test;

public class Day19 {

	String[] smallDiagram = { //
			"    |         ", //
			"    |  +--+   ", //
			"    A  |  C   ", //
			"F---|----E|--+", //
			"    |  |  |  D", //
			"    +B-+  +--+", //
	};

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
	}

	@Test
	public void testDiagram() throws Exception {

		String[] input = smallDiagram;
		String first = input[0];

		int row = 0;
		int col = first.indexOf('|');
		Point p = new Point(row, col);
		Direction dir = Direction.South;
		StringBuilder builder = new StringBuilder();

		while (true) {

		}
	}
}
