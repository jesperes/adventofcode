package common;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;

/**
 * This is only used by 2016 day 11, but not run since that test case is
 * currently ignored.
 * 
 * @author jesperes
 *
 */
public class SearchAlgorithms {

    public interface BreadthFirstSearch<N> {
        List<N> getChildren(N node);

        boolean isSolution(N node);

        N getParent(N node);
    }

    /**
     * Breadth-first search. Assumes that the graph is a tree, i.e. any node is
     * reachable through a unique path.
     *
     * @param root
     * @param data
     * @return
     */
    public static <N> Optional<List<N>> breadthFirstSearch(N root,
            BreadthFirstSearch<N> data) {

        // The frontier along which we are searching.
        Queue<N> openSet = new LinkedList<N>();

        // Tracks the states we have already visited.
        Set<N> closedSet = new HashSet<>();

        openSet.add(root);
        while (!openSet.isEmpty()) {
            N node = openSet.remove();

            /*
             * Check if this is a solution, and if so return.
             */
            System.out.format("Checking solution... (%d left to check)%n",
                    openSet.size());
            boolean isSolution = data.isSolution(node);
            if (isSolution) {
                return Optional.of(constructSolutionList(node, data));
            }

            for (N child : data.getChildren(node)) {

                if (closedSet.contains(child)) {
                    // We have already visited this state.
                    continue;
                }

                // If we already have this state in our list of states
                // to check, do not add it again.
                if (!openSet.contains(child)) {
                    openSet.add(child);
                }
            }

            closedSet.add(node);
        }

        return Optional.empty();
    }

    private static <N> List<N> constructSolutionList(N node,
            BreadthFirstSearch<N> data) {
        LinkedList<N> list = new LinkedList<>();
        N current = node;
        list.add(current);
        while ((current = data.getParent(current)) != null) {
            list.add(0, current);
        }
        return list;
    }
}
