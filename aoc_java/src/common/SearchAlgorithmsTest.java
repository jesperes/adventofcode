package common;

import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.junit.Test;

public class SearchAlgorithmsTest {

    class Node {
        String s;
        Node parent;
        List<Node> children = new ArrayList<>();

        public Node(String string) {
            this.s = string;
        }

        @Override
        public int hashCode() {
            return s.hashCode();
        }

        @Override
        public boolean equals(Object other) {
            return ((Node) other).s.equals(s);
        }

        @Override
        public String toString() {
            return s;
        }
    }

    @Test
    public void testBFS() throws Exception {

        Node a = new Node("a");
        Node b = new Node("b");
        Node c = new Node("c");
        Node d = new Node("d");
        Node e = new Node("e");

        a.children.add(b);
        a.children.add(c);
        c.children.add(d);
        c.children.add(e);

        String toSearchFor = "d";

        Optional<List<Node>> solution = SearchAlgorithms.breadthFirstSearch(a,
                new SearchAlgorithms.BreadthFirstSearch<Node>() {

                    @Override
                    public List<Node> getChildren(Node node) {
                        for (Node child : node.children) {
                            child.parent = node;
                        }
                        return node.children;
                    }

                    @Override
                    public boolean isSolution(Node node) {
                        return node.s.equals(toSearchFor);
                    }

                    @Override
                    public Node getParent(Node node) {
                        return node.parent;
                    }
                });

        assertTrue(solution.isPresent());
        System.out.println(solution.get());
        for (Node n : solution.get()) {
            System.out.format("Node %s%n", n);
        }
    }
}
