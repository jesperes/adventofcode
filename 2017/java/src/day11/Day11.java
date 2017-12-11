package day11;

import static org.junit.Assert.*;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.junit.Test;

import utils.Utils;

/**
 * https://www.redblobgames.com/grids/hexagons/
 */
public class Day11 {

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

        public Position() {
        }

        public Position(int x, int y) {
            this.x = x;
            this.y = y;
        }

        public Position(Position other) {
            this.x = other.x;
            this.y = other.y;
        }

        @Override
        public int hashCode() {
            return Integer.hashCode(x) * Integer.hashCode(y);
        }

        @Override
        public boolean equals(Object obj) {
            Position p = (Position) obj;
            return x == p.x && y == p.y;
        }

        @Override
        public String toString() {
            return String.format("(%d,%d)", x, y);
        }

        public static void move(Position pos, Direction dir) {
            switch (dir) {
            case n:
                pos.y -= 2;
                break;
            case ne:
                pos.x++;
                pos.y--;
                break;
            case se:
                pos.x++;
                pos.y++;
                break;
            case s:
                pos.y += 2;
                break;
            case sw:
                pos.x--;
                pos.y++;
                break;
            case nw:
                pos.y--;
                pos.x--;
                break;
            default:
                fail("unknown direction " + dir);
                return;
            }
        }
    }

    int computeDistance(String turns) {
        Position pos = new Position();

        for (String dir : turns.split(",")) {
            Position.move(pos, Direction.valueOf(dir));
        }

        System.out.format("Final position: %s%n", pos);

        return computeDistanceTo(pos);
    }

    /**
     * 
     * @param dest
     * @return
     */
    private int computeDistanceTo(Position pos) {
        Set<Position> visited = new HashSet<>();
        Direction[] directions = Arrays.stream("n,ne,se,s,sw,nw".split(","))
                .map(s -> Direction.valueOf(s)).toArray(n -> new Direction[n]);

        Set<Position> tovisit = new HashSet<>();
        tovisit.add(new Position());
        int dist = 0;

        while (true) {
            System.out.format("Checking %d positions at distance %s%n",
                    tovisit.size(), dist);

            for (Position visit : tovisit) {
                if (visit.equals(pos)) {
                    System.out.println("Match: " + visit);
                    return dist;
                }

                visited.add(visit);
            }

            tovisit.clear();

            for (Position p : visited) {
                for (Direction dir : directions) {

                    Position newpos = new Position(p);
                    Position.move(newpos, dir);

                    // Skip positions which are in the wrong direction
                    if ((newpos.x > p.x && pos.x < p.x)
                            || (newpos.y > p.y && pos.y < p.y)
                            || (newpos.x < p.x && pos.x > p.x)
                            || (newpos.y < p.y && pos.y > p.y)) {
                        continue;
                    }

                    if (!visited.contains(newpos)) {
                        tovisit.add(newpos);
                    }
                }
            }

            dist++;
        }
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
