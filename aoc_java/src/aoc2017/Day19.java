package aoc2017;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.junit.Test;

import common.AocPuzzle;

public class Day19 extends AocPuzzle {

    public Day19() {
        super(2017, 19);
    }

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

        @Override
        public String toString() {
            return "Point [row=" + row + ", col=" + col + "]";
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

    @Test
    public void testSmall() throws Exception {
        String[] input = smallDiagram;
        StringBuilder builder = new StringBuilder();
        int steps = followTube(input, builder);
        assertEquals("ABCDEF", builder.toString());
        assertEquals(38, steps);
    }

    @Test
    public void testLarge() throws Exception {
        StringBuilder builder = new StringBuilder();
        String[] input = getInputAsLines().toArray(n -> new String[n]);
        int steps = followTube(input, builder);
        assertEquals("EPYDUXANIT", builder.toString());
        assertEquals(17544, steps);
    }
}
