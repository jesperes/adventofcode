package day11;

import static org.junit.Assert.*;

import java.io.IOException;

import org.junit.Test;

import utils.Utils;

/**
 * Alternate implementation of Day11 using cube coordinates:
 * https://www.redblobgames.com/grids/hexagons/
 */
public class Day11_2 {

    private String getPuzzleInput() throws IOException {
        return Utils.readFileFromClassPathAsString(getClass(),
                "day11/input.txt");
    }

    enum Direction {
        nw,
        ne,
        sw,
        se,
        n,
        s
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
            return (Math.abs(x - o.x) + 
                    Math.abs(y - o.y) +
                    Math.abs(z - o.z))/2;
        }
    }

    int computeDistance(String turns) {
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
        
        System.out.format("Final position: %s%n", pos);
        System.out.format("Distance from center: %d%n", dist);
        System.out.format("Maximum distance from center: %d%n", maxDist);
        
        return dist;
        
    }

    @Test
    public void testPart1_small() throws Exception {
        assertEquals(3, computeDistance("ne,ne,ne"));
        assertEquals(0, computeDistance("ne,ne,sw,sw"));
        assertEquals(2, computeDistance("ne,ne,s,s"));
        assertEquals(3, computeDistance("se,sw,se,sw,sw"));
    }

    @Test
    public void testPart1_full() throws Exception {
        System.out.println(
                "[Day11] Steps (part 1): " + computeDistance(getPuzzleInput()));
    }
}
