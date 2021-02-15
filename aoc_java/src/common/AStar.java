package common;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
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

    private static <T> T findNextPos(Set<T> openSet, Map<T, Integer> fScore) {
        int bestFScore = Integer.MAX_VALUE;
        T best = null;
        for (T pos : openSet) {
            if (fScore.get(pos) < bestFScore) {
                best = pos;
                bestFScore = fScore.get(pos);
            }
        }
        return best;
    }

    public static <T> List<T> astar(T start, IAStarCallbacks<T> callbacks) {
        return astar(start, callbacks::heuristic, callbacks::neighbors,
                callbacks::isGoal, callbacks::distance);
    }

    public static <T> List<T> astar(T start, T goal,
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
    public static <T> List<T> astar(T start, Function<T, Integer> heuristic,
            Function<T, Collection<T>> neighbors, Function<T, Boolean> isGoal,
            BiFunction<T, T, Integer> distance) {
        Set<T> openSet = new HashSet<>();
        Map<T, Integer> gScore = new HashMap<>();
        Map<T, T> cameFrom = new HashMap<>();
        Map<T, Integer> fScore = new HashMap<>();

        openSet.add(start);
        gScore.put(start, 0);
        fScore.put(start, heuristic.apply(start));

        while (!openSet.isEmpty()) {
            var current = findNextPos(openSet, fScore);

            if (isGoal.apply(current)) {
                List<T> path = new ArrayList<>();
                collectPath(cameFrom, current, path);
                Collections.reverse(path);
                assertFalse(path.contains(start));
                assertTrue(path.contains(current));
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
        throw new RuntimeException();
    }
}
