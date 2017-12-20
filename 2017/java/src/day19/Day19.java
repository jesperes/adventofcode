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
        North,
        South,
        East,
        West
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
            printDiagram(input, p, dir);

            // Check next position
            Point nextPos = p.getPosition(dir);
            char charAtNextPos = nextPos.getChar(input);
            if (charAtNextPos >= 'A' && charAtNextPos <= 'Z') {
                // Collect character and continue in the same direction
                System.out.println("Collecting char: " + charAtNextPos);
                builder.append(charAtNextPos);
                p = nextPos;
                continue;
            } else if (charAtNextPos == '|' || charAtNextPos == '-') {
                // Continue in the same direction, unless we've gone outside
                // the diagram, in which case we're done.
                if (!nextPos.inside(input)) {
                    break;
                }
                p = nextPos;
                continue;
            } else if (charAtNextPos == '+') {
                // We need to turn.
            } else {
                fail("Unexpected char: " + charAtNextPos);
            }
        }
    }
}
