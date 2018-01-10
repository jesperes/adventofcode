package utils;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;
import java.util.function.Function;

public class SearchAlgorithms {

    @FunctionalInterface
    public interface NodeFactory<Node, Action> {
        Node createChildNode(Node parent, Action action);
    }

    /**
     * Somewhat fancy breadth-first search where child nodes are computed
     * dynamically on demand, and where moves are represented explicitly.
     * 
     * @param root
     *            The initial state to start searching from.
     * @param actionProvider
     *            Function which computes the possible set of actions available
     *            from a node.
     * @param nodeFactory
     *            Function which computes child nodes given a parent node and an
     *            action.
     * @param solutionChecker
     *            Function which checks if a given state is a solution.
     * @param parentFunction
     *            Maps nodes to their parents. This is necessary to be able to
     *            reconstruct the path to the solution node.
     * @param <Node>
     *            The node/state type. Ensure that this type has a correctly
     *            implemented equals/hashCode method.
     * @param <Action>
     *            The type used to describe actions going from one state to
     *            another (i.e. "moves" in a game search).
     * @return The list of nodes leading from the root to the solution. The
     *         first element of this list is the root node, and the last is the
     *         solution.
     */
    public static <Node, Action> Optional<List<Node>> breadthFirstSearch(
            Node root, //
            Function<Node, Collection<Action>> actionProvider,
            NodeFactory<Node, Action> nodeFactory,
            Function<Node, Boolean> solutionChecker,
            Function<Node, Node> parentFunction) {

        // The frontier along which we are searching.
        Queue<Node> openSet = new LinkedList<Node>();

        // Tracks the states we have already visited.
        Set<Node> closedSet = new HashSet<>();

        openSet.add(root);
        while (!openSet.isEmpty()) {
            Node node = openSet.remove();

            /*
             * Check if this is a solution, and if so return.
             */
            boolean isSolution = solutionChecker.apply(node);
            if (isSolution) {
                return Optional.of(constructSolutionList(node, parentFunction));
            }

            /*
             * Compute child nodes and recurse.
             */
            Collection<Action> actions = actionProvider.apply(node);
            for (Action action : actions) {
                Node child = nodeFactory.createChildNode(node, action);

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

    private static <Node> List<Node> constructSolutionList(Node node,
            Function<Node, Node> parentFunction) {
        LinkedList<Node> list = new LinkedList<>();
        Node current = node;
        list.add(current);
        while ((current = parentFunction.apply(current)) != null) {
            list.add(0, current);
        }
        return list;
    }
}
