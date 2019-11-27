package aoc2017;

import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import common.AocPuzzle;

public class Day22_Part2 extends AocPuzzle {

    public Day22_Part2() {
        super(2017, 22);
    }

    enum State {
        Infected, Weakened, Flagged, Clean
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
        Map<Coord, State> nodeStates = new HashMap<>();

        Coord current = new Coord(0, 0);
        int dir = 0; // 0 = North, 1 = East, 2 = South, 3 = West
        int numInfections = 0;

        public Cluster(String[] map) {
            int offset = map.length / 2;

            for (int i = 0; i < map.length; i++) {
                for (int j = 0; j < map[i].length(); j++) {
                    if (map[i].charAt(j) == '#') {
                        Coord node = new Coord(j - offset, i - offset);
                        nodeStates.put(node, State.Infected);
                    }
                }
            }
        }

        public State getState(Coord node) {
            // Coords not in this map are assumed to be clean
            return nodeStates.getOrDefault(node, State.Clean);
        }

        public String toString() {
            StringBuilder builder = new StringBuilder();

            int size = 5;

            builder.append("Current scanner direction: " + dir + "\n");

            for (int i = -size + 1; i < size; i++) {
                for (int j = -size + 1; j < size; j++) {
                    Coord node = new Coord(j, i);
                    builder.append(node.equals(current) ? '[' : ' ');

                    switch (getState(node)) {
                    case Clean:
                        builder.append('.');
                        break;
                    case Flagged:
                        builder.append('F');
                        break;
                    case Infected:
                        builder.append('#');
                        break;
                    case Weakened:
                        builder.append('W');
                        break;
                    default:
                        throw new AssertionError();
                    }

                    builder.append(node.equals(current) ? ']' : ' ');
                }

                builder.append("\n");
            }

            return builder.toString();
        }

        void turnLeft() {
            dir = (dir + 3) % 4;
        }

        void turnRight() {
            dir = (dir + 1) % 4;
        }

        void reverse() {
            dir = (dir + 2) % 4;
        }

        public void burst() {

            switch (getState(current)) {
            case Clean:
                turnLeft();
                nodeStates.put(current, State.Weakened);
                break;
            case Weakened:
                // no change in direction
                numInfections++;
                nodeStates.put(current, State.Infected);
                break;
            case Infected:
                turnRight();
                nodeStates.put(current, State.Flagged);
                break;
            case Flagged:
                reverse();
                nodeStates.put(current, State.Clean);
                break;
            default:
                throw new AssertionError();
            }

            moveForward();
        }

        public int getNumInfections() {
            return numInfections;
        }

        private void moveForward() throws AssertionError {
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
            default:
                throw new AssertionError();
            }
        }
    }

    @Test
    public void testCluster() throws Exception {
        String[] map = { "..#", "#..", "..." };

        Cluster cluster = new Cluster(map);

        System.out.println(cluster);

        for (int i = 0; i < 100; i++) {
            cluster.burst();
            // System.out.println(cluster);
        }

        System.out.println(cluster);
        assertEquals(26, cluster.getNumInfections());
    }

    @Test
    public void testCluster2() throws Exception {
        String[] map = { "..#", "#..", "..." };

        Cluster cluster = new Cluster(map);

        for (int i = 0; i < 10000000; i++) {
            cluster.burst();
        }

        assertEquals(2511944, cluster.getNumInfections());
    }

    @Test
    public void testClusterLarge() throws Exception {
        String[] map = getInputAsLines().toArray(n -> new String[n]);

        Cluster cluster = new Cluster(map);

        for (int i = 0; i < 10000000; i++) {
            cluster.burst();
        }

        System.out.println(cluster);
        System.out.println("Num infections: " + cluster.getNumInfections());
    }
}
