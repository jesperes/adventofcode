package aoc2017;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import common.AocPuzzle;

/**
 * https://www.redblobgames.com/grids/hexagons/
 */
public class Day11 extends AocPuzzle {

    public Day11() {
        super(2017, 11);
    }

    enum Direction {
        nw, ne, sw, se, n, s
    }

    static class Position {
        int x = 0;
        int y = 0;
        int z = 0;

        public Position() {
        }

        public Position(int x, int y, int z) {
            this.x = x;
            this.y = y;
        }

        public Position(Position other) {
            this.x = other.x;
            this.y = other.y;
            this.z = other.z;
        }

        @Override
        public String toString() {
            return String.format("(%d,%d,%d)", x, y, z);
        }

        public static void move(Position pos, Direction dir) {
            switch (dir) {
            case n:
                pos.y++;
                pos.z--;
                break;
            case s:
                pos.y--;
                pos.z++;
                break;
            case ne:
                pos.z--;
                pos.x++;
                break;
            case sw:
                pos.z++;
                pos.x--;
                break;
            case nw:
                pos.y++;
                pos.x--;
                break;
            case se:
                pos.y--;
                pos.x++;
                break;
            default:
                break;
            }
        }

        public int distanceTo(Position o) {
            return (Math.abs(x - o.x) + Math.abs(y - o.y) + Math.abs(z - o.z))
                    / 2;
        }
    }

    class Result {
        Position pos;
        int dist;
        int maxDist;

        public Result(Position pos, int dist, int maxDist) {
            this.pos = pos;
            this.dist = dist;
            this.maxDist = maxDist;
        }

        @Override
        public String toString() {
            return "Result [pos=" + pos + ", dist=" + dist + ", maxDist="
                    + maxDist + "]";
        }
    }

    Result computeDistance(String turns) {
        Position pos = new Position();
        Position startPos = new Position();

        int maxDist = Integer.MIN_VALUE;

        for (String dir : turns.split(",")) {
            Position.move(pos, Direction.valueOf(dir));
            int currentDistance = pos.distanceTo(startPos);
            if (currentDistance > maxDist) {
                maxDist = currentDistance;
            }
        }
        int dist = pos.distanceTo(startPos);

        return new Result(pos, dist, maxDist);
    }

    @Test
    public void testExamples() throws Exception {
        assertEquals(3, computeDistance("ne,ne,ne").dist);
        assertEquals(0, computeDistance("ne,ne,sw,sw").dist);
        assertEquals(2, computeDistance("ne,ne,s,s").dist);
        assertEquals(3, computeDistance("se,sw,se,sw,sw").dist);
    }

    @Test
    public void testPuzzle() throws Exception {
        Result result = computeDistance(getInputAsString());
        assertEquals(685, result.dist); // Part 1
        assertEquals(1457, result.maxDist); // Part 2
    }
}
