import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.junit.Test;

// 974 is too high

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

        @Override
        public String toString() {
            return String.format("{%d,%d}", x, y);
        }

    }

    enum Tool {
        ClimbingGear, Torch, Neither, Unset
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
        long sp = findShortestPath(start, target, depth, map);
        System.out.println("Shortest path: " + sp);
    }

    static class NodeData {
        long x;
        long y;
        Tool tool;

        public NodeData(long x, long y, Tool tool) {
            super();
            this.x = x;
            this.y = y;
            this.tool = tool;
        }

        @Override
        public String toString() {
            return String.format("{%d,%d,%s}", x, y, tool);
        }

        @Override
        public int hashCode() {
            return toString().hashCode();
        }

        @Override
        public boolean equals(Object obj) {
            return toString().equals(obj.toString());
        }
    }

    private static long findShortestPath(Pos start, Pos target, int depth,
            Map<Pos, RegionType> map) {

        Graph<NodeData> graph = new Graph<>();
        Map<String, Node<NodeData>> nodemap = new HashMap<>();

        // Create nodes. The graph has two instance of each position,
        // depending on the tool.

        for (long x = start.x; x <= target.x + 100; x++) {
            for (long y = start.y; y <= target.y + 100; y++) {
                Pos pos = new Pos(x, y);
                RegionType rt = regionType(pos, depth);
                for (Tool tool : Tool.values()) {
                    NodeData payload = new NodeData(pos.x, pos.y, tool);
                    Node<NodeData> node = new Node<>(payload);
                    graph.addNode(node);
                    nodemap.put(payload.toString(), node);
                }
            }
        }

        // Create edges. Two edges for each node, one switching tool,
        // the other keeping the same.
        for (Node<NodeData> node : graph.getNodes()) {
            NodeData payload = node.getPayload();
            Pos pos = new Pos(payload.x, payload.y);
            Tool nodetool = payload.tool;

            for (Pos adjacent : adjacentTo(pos)) {
                for (Tool tool : Tool.values()) {
                    if (isValidTool(adjacent, tool, depth)) {

                        int dist = 1;
                        if (!nodetool.equals(tool)) {
                            dist += 7;
                        }

                        NodeData nd = new NodeData(adjacent.x, adjacent.y,
                                tool);
                        Node<NodeData> destNode = nodemap.get(nd.toString());
                        node.addDestination(destNode, dist);
                    }
                }
            }
        }

        Node<NodeData> startNode = nodemap
                .get(new NodeData(start.x, start.y, Tool.Torch).toString());
        Node<NodeData> endNode = nodemap
                .get(new NodeData(target.x, target.y, Tool.Torch).toString());

        Node<NodeData> end = Dijkstra.calculateShortestPathFromSource(graph,
                startNode, endNode);

        System.out.println(end);
        return end.getDistance() + 1;
    }

    private static boolean isValidTool(Pos adj, Tool tool, long depth) {
        switch (regionType(adj, depth)) {
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

        set.add(new Pos(u.x, u.y + 1));
        set.add(new Pos(u.x + 1, u.y));
        if (u.x > 1)
            set.add(new Pos(u.x - 1, u.y));
        if (u.y > 1)
            set.add(new Pos(u.x, u.y - 1));

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
