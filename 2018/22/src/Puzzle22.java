import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.junit.Test;

public class Puzzle22 {

    static class Pos {
        long x;
        long y;

        public Pos(long x, long y) {
            super();
            this.x = x;
            this.y = y;
        }

        @Override
        public int hashCode() {
            return Long.hashCode(x) ^ Long.hashCode(y);
        }

        @Override
        public boolean equals(Object obj) {
            Pos o = (Pos) obj;
            return x == o.x && y == o.y;
        }
    }

    enum Tool {
        ClimbingGear, Torch, Neither
    }

    static Map<Pos, Long> erosionLevel = new HashMap<>();
    static Map<Pos, Long> geologicIndexes = new HashMap<>();

    enum RegionType {
        Rocky('.'), Wet('='), Narrow('|');

        private char c;

        RegionType(char c) {
            this.c = c;
        }

        char getChar() {
            return this.c;
        }
    }

    static long geologicIndex(Pos pos, long depth) {
        if (pos.x == 0 && pos.y == 0) {
            return 0;
        } else if (pos.y == 0) {
            return pos.x * 16807L;
        } else if (pos.x == 0) {
            return pos.y * 48271L;
        } else {
            if (!geologicIndexes.containsKey(pos)) {
                long index = erosionLevel(new Pos(pos.x - 1, pos.y), depth)
                        * erosionLevel(new Pos(pos.x, pos.y - 1), depth);
                geologicIndexes.put(pos, index);
            }

            return geologicIndexes.get(pos);
        }
    }

    static long erosionLevel(Pos pos, long depth) {
        return (geologicIndex(pos, depth) + depth) % 20183;
    }

    static RegionType regionType(Pos pos, long depth) {
        long level = erosionLevel(pos, depth);
        return RegionType.values()[(int) (level % 3)];
    }

    public static void main(String[] args) {
        // int depth = 510;
        // Pos target = new Pos(10, 10);
        int depth = 5913;
        Pos target = new Pos(8, 701);
        Pos start = new Pos(0, 0);
        Map<Pos, RegionType> map = new HashMap<Pos, RegionType>();

        int rl = riskLevel(start, target, depth, map);
        System.out.println("Risk level: " + rl);

        /*
         * Part 2: finding shortest path with weighted edges.
         */
        findShortestPath(start, target, depth, map);
    }

    static class Edge {
        Pos from;
        Pos to;
        Tool tool;

        public Edge(Pos from, Pos to, Tool tool) {
            super();
            this.from = from;
            this.to = to;
            this.tool = tool;
        }
    }

    private static void findShortestPath(Pos start, Pos target, long depth,
            Map<Pos, RegionType> map) {

        Map<Pos, Long> distMap = new HashMap<Pos, Long>();
        Set<Pos> queue = new HashSet<>();

        distMap.put(start, 0L);
        queue.add(start);

        Tool currentTool = Tool.Torch;

        while (!queue.isEmpty()) {
            // Pick next node from queue with shortest path to source
            long shortestDist = Long.MAX_VALUE;
            Pos u = null;
            for (Pos pos : queue) {
                long dist = distMap.get(pos);
                if (dist < shortestDist) {
                    u = pos;
                    shortestDist = dist;
                }
            }

            queue.remove(u);

            for (Pos adj : adjacentTo(u)) {
                // There will be two edges added for each node,
                // one for each valid tool.
                for (Tool tool : Tool.values()) {
                    if (isValidTool(adj, tool, map)) {

                        long distToAdjacent = 1;
                        if (!tool.equals(currentTool))
                            distToAdjacent += 7;

                        distMap.put(u, shortestDist + distToAdjacent);

                    }
                }
            }
        }
    }

    private static boolean isValidTool(Pos adj, Tool tool,
            Map<Pos, RegionType> map) {
        switch (map.get(adj)) {
        case Narrow:
            return tool == Tool.Torch || tool == Tool.Neither;
        case Rocky:
            return tool == Tool.ClimbingGear || tool == Tool.Torch;
        case Wet:
            return tool == Tool.ClimbingGear || tool == Tool.Neither;
        default:
            return false;
        }
    }

    private static Set<Pos> adjacentTo(Pos u) {
        Set<Pos> set = new HashSet<>();

        for (long x = u.x - 1; x <= u.x + 1; x++) {
            for (long y = u.y - 1; y <= u.y + 1; y++) {
                if (x == y || x < 0 || y < 0)
                    continue;

                set.add(new Pos(x, y));
            }
        }

        return set;
    }

    private static int riskLevel(Pos start, Pos target, int depth,
            Map<Pos, RegionType> map) {

        int rl = 0;
        for (long x = start.x; x <= target.x; x++) {
            for (long y = start.y; y <= target.y; y++) {
                rl += regionType(new Pos(x, y), depth).ordinal();
            }
        }

        return rl;
    }

    @Test
    public void testRegionType() throws Exception {
        assertEquals(RegionType.Rocky, regionType(new Pos(0, 0), 510));
        assertEquals(RegionType.Wet, regionType(new Pos(1, 0), 510));
        assertEquals(RegionType.Rocky, regionType(new Pos(0, 1), 510));
        assertEquals(RegionType.Narrow, regionType(new Pos(1, 1), 510));
    }
}
