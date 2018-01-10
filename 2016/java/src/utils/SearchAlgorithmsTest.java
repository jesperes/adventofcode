package utils;

import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import org.junit.Test;

import utils.SearchAlgorithms.NodeFactory;

public class SearchAlgorithmsTest {

    class Node {
        String s;
        Move move;
        private Node parent;

        public Node(Node parent, String s) {
            this.s = s;
            this.parent = parent;
        }

        public Node(String s) {
            this.s = s;
            this.parent = null;
        }

        @Override
        public int hashCode() {
            return s.hashCode();
        }

        @Override
        public boolean equals(Object obj) {
            return s.equals(((Node) obj).s);
        }

        @Override
        public String toString() {
            return s;
        }

    }

    abstract class Move {
        abstract public Node execute(Node node);
    }

    class AppendChar extends Move {
        private String stringToAppend;

        public AppendChar(String app) {
            this.stringToAppend = app;
        }

        @Override
        public String toString() {
            return String.format("+%s", stringToAppend);
        }

        @Override
        public Node execute(Node node) {
            Node n = new Node(node, node.s + stringToAppend);
            n.move = this;
            return n;
        }
    }

    @Test
    public void testBFS() throws Exception {

        Node a = new Node("a");

        Collection<Move> moves = Arrays.asList(new AppendChar("b"),
                new AppendChar("c"), new AppendChar("d"));

        Function<Node, Collection<Move>> actionProvider = (node) -> moves;

        NodeFactory<Node, Move> nodeFactory = (node, move) -> move
                .execute(node);

        Function<Node, Boolean> solutionChecker = (node) -> {
            System.out.println("Checking solution: " + node.s);
            return node.s.equals("abcdbcd");
        };

        Function<Node, Node> parentFunction = (node) -> node.parent;

        Optional<List<Node>> solution = SearchAlgorithms.breadthFirstSearch(a,
                actionProvider, nodeFactory, solutionChecker, parentFunction);
        assertTrue(solution.isPresent());
        System.out.println(solution.get());
        for (Node b : solution.get()) {
            System.out.format("Node %s (by move %s)%n", b, b.move);
        }
    }

}
