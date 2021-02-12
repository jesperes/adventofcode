package common;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * Java-implementation of the A* search algorithm.
 * 
 * https://en.wikipedia.org/wiki/A*_search_algorithm
 */
public class AStar<T> {

    public static interface IAStarCallbacks<T> {
        int heuristic(T node);

        Collection<T> neighbors(T node);

        boolean isGoal(T node);

        default int distance(T node, T neighbor) {
            return 1;
        }
    }

    private static <T> void collectPath(Map<T, T> cameFrom, T goal,
            List<T> list) {
        if (cameFrom.containsKey(goal)) {
            list.add(goal);
            collectPath(cameFrom, cameFrom.get(goal), list);
        }
    }

    public static <T> List<T> astar(Collection<T> start,
            IAStarCallbacks<T> callbacks) {
        return astar(start, callbacks::heuristic, callbacks::neighbors,
                callbacks::isGoal, callbacks::distance);
    }

    public static <T> List<T> astar(Collection<T> start, T goal,
            Function<T, Integer> heuristic,
            Function<T, Collection<T>> neighbors) {
        return astar(start, heuristic, neighbors, //
                node -> node.equals(goal), // endfun
                (node, nbr) -> 1); // distfun
    }

    /**
     * @return The shortest path to the target node. This list contains the
     *         target node, but not the start node.
     */
    public static <T> List<T> astar(Collection<T> startNodes,
            Function<T, Integer> heuristic,
            Function<T, Collection<T>> neighbors, Function<T, Boolean> isGoal,
            BiFunction<T, T, Integer> distance) {
        Map<T, Integer> gScore = new HashMap<>();
        Map<T, T> cameFrom = new HashMap<>();
        Map<T, Integer> fScore = new HashMap<>();
        TreeSet<T> openSet = new TreeSet<>(new Comparator<T>() {
            @Override
            public int compare(T o1, T o2) {
                return Integer.compare(fScore.get(o1), fScore.get(o2));
            }
        });

        for (T start : startNodes) {
            fScore.put(start, heuristic.apply(start));
            gScore.put(start, 0);
            openSet.add(start);
        }

        while (!openSet.isEmpty()) {
            var current = openSet.first();

            if (isGoal.apply(current)) {
                List<T> path = new ArrayList<>();
                collectPath(cameFrom, current, path);
                if (path.size() == 0)
                    return Collections.emptyList();

                Collections.reverse(path);
                // assertFalse(path.contains(start));
                // assertTrue(path.contains(current));
                return path;
            }

            openSet.remove(current);
            for (T nbr : neighbors.apply(current)) {
                var tentative_gScore = gScore.get(current)
                        + distance.apply(nbr, current);
                if (tentative_gScore < gScore.getOrDefault(nbr,
                        Integer.MAX_VALUE)) {
                    cameFrom.put(nbr, current);
                    gScore.put(nbr, tentative_gScore);
                    fScore.put(nbr, tentative_gScore + heuristic.apply(nbr));
                    if (!openSet.contains(nbr)) {
                        openSet.add(nbr);
                    }
                }
            }
        }
        return Collections.emptyList();
    }
}
