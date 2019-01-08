import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

public class Puzzle13 {
    final static int FAV = 1364;
    // final static int FAV = 10;

    private static class Point {
        long x;
        long y;

        public Point(long x, long y) {
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
            Point o = (Point) obj;
            return o.x == x && o.y == y;
        }

        boolean isWall() {
            if (x < 0 || y < 0)
                return true;

            return Long.bitCount(
                    ((x * x) + (3 * x) + (2 * x * y) + y + (y * y) + FAV))
                    % 2 == 1;
        }

        Stream<Point> neighbors() {
            return Arrays
                    .asList(new Point(x - 1, y), new Point(x + 1, y),
                            new Point(x, y + 1), new Point(x, y - 1))
                    .stream().filter(p -> !p.isWall());
        }
    }

    private static Set<Point> searchToDepth(Point start, int depth) {
        Map<Point, Integer> visited = new HashMap<>();
        visited.put(start, 0);
        searchToDepth(start, 1, depth, visited);
        return visited.keySet();
    }

    private static void searchToDepth(Point current, int currDepth,
            int maxDepth, Map<Point, Integer> visited) {
        if (currDepth > maxDepth)
            return;

        current.neighbors().forEach(nbr -> {
            /*
             * Check if we have already seen this node, and if so at what depth.
             * If we are getting to this node by a shorter path we must revisit
             * all its neighbors to ensure that we find all paths.
             */
            int visitedAt = visited.getOrDefault(nbr, Integer.MAX_VALUE);
            if (currDepth < visitedAt) {
                visited.put(nbr, currDepth);
                searchToDepth(nbr, currDepth + 1, maxDepth, visited);
            }
        });
    }

    public static void main(String[] args) {
        Set<Point> visited = searchToDepth(new Point(1, 1), 50);

        int size = 20;
        for (int y = 0; y < size; y++) {
            for (int x = 0; x < size; x++) {

                Point point = new Point(x, y);
                if (visited.contains(point)) {
                    System.out.print("O");
                } else if (point.isWall()) {
                    System.out.print("#");
                } else {
                    System.out.print(".");
                }
            }
            System.out.println();
        }
        System.out.println(visited.size());
    }
}
