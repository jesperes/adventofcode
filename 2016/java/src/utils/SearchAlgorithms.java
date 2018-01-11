package utils;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;

public class SearchAlgorithms {

    public interface BreadthFirstSearch<Node> {
        List<Node> getChildren(Node node);

        boolean isSolution(Node node);

        Node getParent(Node node);
    }

    /**
     * Breadth-first search. Assumes that the graph is a tree, i.e. any node is
     * reachable through a unique path.
     * 
     * @param root
     * @param data
     * @return
     */
    public static <Node> Optional<List<Node>> breadthFirstSearch(Node root,
            BreadthFirstSearch<Node> data) {

        // The frontier along which we are searching.
        Queue<Node> openSet = new LinkedList<Node>();

        // Tracks the states we have already visited.
        Set<Node> closedSet = new HashSet<>();

        openSet.add(root);
        while (!openSet.isEmpty()) {
            Node node = openSet.remove();

            System.out.println("Picking node from queue: " + node);

            /*
             * Check if this is a solution, and if so return.
             */
            boolean isSolution = data.isSolution(node);
            if (isSolution) {
                System.out.println("Found solution: " + node);
                return Optional.of(constructSolutionList(node, data));
            }

            for (Node child : data.getChildren(node)) {

                System.out.println("Checking child: " + child);

                if (closedSet.contains(child)) {
                    // We have already visited this state.
                    System.out.println("Already visited: " + child);
                    continue;
                }

                // If we already have this state in our list of states
                // to check, do not add it again.
                if (!openSet.contains(child)) {
                    System.out
                            .println("Unchecked node, add to queue: " + child);
                    openSet.add(child);
                }
            }

            closedSet.add(node);
        }

        return Optional.empty();
    }

    private static <Node> List<Node> constructSolutionList(Node node,
            BreadthFirstSearch<Node> data) {
        LinkedList<Node> list = new LinkedList<>();
        Node current = node;
        list.add(current);
        while ((current = data.getParent(current)) != null) {
            list.add(0, current);
        }
        return list;
    }
}
