package day11;

import static org.junit.Assert.*;

import java.io.IOException;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Set;

import org.junit.Test;

import utils.Utils;

public class Day11 {

    private String getPuzzleInput() throws IOException {
        return Utils.readFileFromClassPathAsString(getClass(),
                "day11/input.txt");
    }

    class Position {
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

        public Position move(String dir) {
            switch (dir) {
            case "n":
                return new Position(x, y - 2);
            case "ne":
                return new Position(x + 1, y - 1);
            case "se":
                return new Position(x + 1, y + 1);
            case "s":
                return new Position(x, y + 2);
            case "sw":
                return new Position(x - 1, y + 1);
            case "nw":
                return new Position(x - 1, y - 1);
            default:
                fail("unknown direction " + dir);
                return null;
            }
        }
    }

    int computeDistance(String turns) {
        Position pos = new Position();

        for (String dir : turns.split(",")) {
            pos = pos.move(dir);
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
        String[] directions = "n,ne,se,s,sw,nw".split(",");

        Set<Position> tovisit = new HashSet<>();
        tovisit.add(new Position());
        int dist = 0;

        while (true) {
            System.out.format("Checking %d positions at distance %s%n", tovisit.size(), dist);

            for (Position visit : tovisit) {
                if (visit.equals(pos)) {
                    System.out.println("Match: " + visit);
                    return dist;
                }

                visited.add(visit);
            }

            tovisit.clear();

            for (Position p : visited) {
                for (String dir : directions) {
                    Position newpos = p.move(dir);

                    // Skip positions which are in the wrong direction
                    if ((newpos.x > p.x && pos.x < p.x)
                            || (newpos.y > p.y && pos.y < p.y)
                            || (newpos.x < p.x && pos.x > p.x)
                            || (newpos.y < p.y && pos.y > p.y)) {
                        continue;
                    }

                    if (!visited.contains(newpos)) {
                        tovisit.add(p.move(dir));
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
