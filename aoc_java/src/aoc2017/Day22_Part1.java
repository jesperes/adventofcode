package aoc2017;

import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import common.AocPuzzle;

public class Day22_Part1 extends AocPuzzle {

    public Day22_Part1() {
        super(2017, 22);
    }

    static class Coord {
        int x;
        int y;

        public Coord(int x, int y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + x;
            result = prime * result + y;
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            Coord other = (Coord) obj;
            if (x != other.x)
                return false;
            if (y != other.y)
                return false;
            return true;
        }

        @Override
        public String toString() {
            return String.format("(%d,%d)", x, y);
        }
    }

    static class Cluster {
        Map<String, Coord> infectedNodes = new HashMap<>();
        Coord current = new Coord(0, 0);
        int dir = 0; // 0 = North, 1 = East, 2 = South, 3 = West
        int numInfections = 0;

        public Cluster(String[] map) {
            int offset = map.length / 2;

            for (int i = 0; i < map.length; i++) {
                for (int j = 0; j < map[i].length(); j++) {
                    if (map[i].charAt(j) == '#') {
                        Coord node = new Coord(j - offset, i - offset);
                        infectedNodes.put(node.toString(), node);
                    }
                }
            }
        }

        public boolean isInfected(Coord node) {
            return infectedNodes.containsKey(node.toString());
        }

        public void infect(Coord node) {
            numInfections++;
            infectedNodes.put(node.toString(), node);
        }

        public void clean(Coord node) {
            infectedNodes.remove(node.toString());
        }

        public int getNumInfections() {
            return numInfections;
        }

        @Override
        public String toString() {
            StringBuilder builder = new StringBuilder();

            int size = 5;

            builder.append("Current scanner direction: " + dir + "\n");

            for (int i = -size + 1; i < size; i++) {
                for (int j = -size + 1; j < size; j++) {
                    Coord node = new Coord(j, i);
                    if (node.equals(current)) {
                        if (infectedNodes.containsKey(node.toString())) {
                            builder.append("[#]");
                        } else {
                            builder.append("[.]");
                        }
                    } else {
                        if (infectedNodes.containsKey(node.toString())) {
                            builder.append(" # ");
                        } else {
                            builder.append(" . ");
                        }
                    }
                }

                builder.append("\n\n");
            }

            return builder.toString();
        }

        public void burst() {
            boolean infected = isInfected(current);

            if (infected) {
                dir = (dir + 1) % 4; // Turn right
                clean(current);
            } else {
                dir = (dir + 3) % 4; // Turn left
                infect(current);
            }

            switch (dir) {
            case 0: // North
                current = new Coord(current.x, current.y - 1);
                break;
            case 1: // East
                current = new Coord(current.x + 1, current.y);
                break;
            case 2: // South
                current = new Coord(current.x, current.y + 1);
                break;
            case 3: // West
                current = new Coord(current.x - 1, current.y);
                break;
            }
        }
    }

    @Test
    public void testCluster() throws Exception {
        String[] map = { "..#", "#..", "..." };

        Cluster cluster = new Cluster(map);

        for (int i = 0; i < 70; i++) {
            cluster.burst();
        }

        assertEquals(41, cluster.getNumInfections());
    }

    @Test
    public void testCluster2() throws Exception {
        String[] map = { "..#", "#..", "..." };

        Cluster cluster = new Cluster(map);

        for (int i = 0; i < 10000; i++) {
            cluster.burst();
        }

        assertEquals(5587, cluster.getNumInfections());
    }

    @Test
    public void testClusterLarge() throws Exception {
        String[] map = getInputAsLines().toArray(n -> new String[n]);

        Cluster cluster = new Cluster(map);

        for (int i = 0; i < 10000; i++) {
            cluster.burst();
        }

        assertEquals(5322, cluster.getNumInfections());
    }
}
