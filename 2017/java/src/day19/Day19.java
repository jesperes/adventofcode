package day19;

import static org.junit.Assert.*;

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
			if (inside(diagram))
				return diagram[row].charAt(col);
			else {
				fail("Position not inside diagram");
				return 0;
			}
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
			default:
				break;
			}

			fail("Nowhere to go!");
			return null;
		}
	}

	static void printDiagram(String[] diagram, Point pos, Direction dir) {

		System.out.format("Diagram (%d x %d)%n", diagram[0].length(),
				diagram.length);
		System.out.println("-------------------------------------------------");
		int row = 0;
		for (String rowstr : diagram) {
			int col = 0;
			for (char c : rowstr.toCharArray()) {
				if (pos.row == row && pos.col == col) {
					switch (dir) {
					case East:
						System.out.print('>');
						break;
					case North:
						System.out.print('^');
						break;
					case South:
						System.out.print('v');
						break;
					case West:
						System.out.print('<');
						break;
					default:
						break;
					}
				} else {
					System.out.print(c);
				}
				col++;
			}
			System.out.println();
			row++;
		}
		System.out.println("-------------------------------------------------");
	}

	private String followTube(String[] input) {
		String first = input[0];

		int row = 0;
		int col = first.indexOf('|');
		Point p = new Point(row, col);
		Direction dir = Direction.South;
		StringBuilder builder = new StringBuilder();

		while (p.inside(input)) {

			char c = p.getChar(input);

			// printDiagram(input, p, dir);

			if (Character.isLetter(c)) {
				// Collect character and continue in the same direction
				System.out.println("Collecting char: " + c);
				builder.append(c);
				p = p.getPosition(dir);
				continue;
			}

			if (c == '+') {
				dir = p.turn(dir, input);
				System.out.println("Turned to new direction: " + dir);
				p = p.getPosition(dir);
				continue;
			}

			if (c == '|' || c == '-') {
				p = p.getPosition(dir);
				continue;
			}

			fail("Unexpected char: " + c);
		}

		return builder.toString();
	}

	@Test
	public void testDiagram() throws Exception {
		String[] input = smallDiagram;
		String letters = followTube(input);
		assertEquals("ABCDEF", letters);
	}

}
