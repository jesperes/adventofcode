package common;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * 
 * @author jespe
 *
 * @param <T>
 *            The node type. Must implement {@link Comparable}. At each step in
 *            the search, the "smallest" node according to this order is
 *            considered.
 * 
 */
abstract public class AStarSearch<T extends Comparable<T>> {

    /**
     * Is this the goal node?
     * 
     * @param node
     * @return
     */
    abstract public boolean isGoal(T node);

    /**
     * Return the neighbors of the given node.
     * 
     * @param node
     * @return
     */
    abstract public Collection<T> neighbors(T node);

    /**
     * Return the distance from the current node to the given neighbor.
     * 
     * @param curr
     * @param nbr
     * @return
     */
    abstract public int distance(T curr, T nbr);

    public final List<T> search(T start) {
        Set<T> closedSet = new HashSet<>();
        TreeSet<T> openSet = new TreeSet<>();
        Map<T, T> cameFrom = new HashMap<>();
        Map<T, Integer> gScore = new HashMap<>();

        openSet.add(start);
        gScore.put(start, 0);

        while (!openSet.isEmpty()) {
            T current = openSet.pollFirst();
            if (isGoal(current)) {
                return reconstructPath(cameFrom, current);
            }

            closedSet.add(current);

            for (T nbr : neighbors(current)) {
                if (closedSet.contains(nbr))
                    continue;

                int tentativeGScore = gScore.get(current)
                        + distance(current, nbr);

                if (!openSet.contains(nbr)) // new node
                    openSet.add(nbr);
                else if (tentativeGScore >= gScore.getOrDefault(nbr,
                        Integer.MAX_VALUE))
                    continue;

                // best path
                cameFrom.put(nbr, current);
                gScore.put(nbr, tentativeGScore);
            }
        }

        return null;
    }

    private List<T> reconstructPath(Map<T, T> cameFrom, T current) {
        List<T> path = new ArrayList<>();
        while (cameFrom.containsKey(current)) {
            path.add(current);
            current = cameFrom.get(current);
        }
        return path;
    }
}
