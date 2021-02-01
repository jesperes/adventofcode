package aoc2017;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day22 implements IAocIntPuzzle<String[]> {

    enum State {
        Infected, Weakened, Flagged, Clean
    }

    record Coord(int x, int y) {
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

        public void infect(Coord node) {
            numInfections++;
            nodeStates.put(node, State.Infected);
        }

        public void clean(Coord node) {
            nodeStates.remove(node);
        }

        public int getNumInfections() {
            return numInfections;
        }

        public State getState(Coord node) {
            return nodeStates.getOrDefault(node, State.Clean);
        }

        public boolean isInfected(Coord node) {
            return getState(node) == State.Infected;
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

        // Part 1
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

        // Part 2
        public void burst2() {
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

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2017, 22, "Sporifica Virus", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(5322, 2512079);
    }

    @Override
    public String[] parse(Optional<File> file) throws IOException {
        return InputUtils.asStringList(file.get()).toArray(n -> new String[n]);
    }

    @Override
    public Integer part1(String[] input) {
        Cluster cluster = new Cluster(input);

        for (int i = 0; i < 10000; i++) {
            cluster.burst();
        }

        return cluster.getNumInfections();
    }

    @Override
    public Integer part2(String[] input) {
        Cluster cluster = new Cluster(input);

        for (int i = 0; i < 10_000_000; i++) {
            cluster.burst2();
        }

        return cluster.getNumInfections();
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day22());
    }
}
