package aoc2017;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import org.junit.Test;

import common.AocPuzzle;

/**
 * deo edw edwed wed
 * 
 * @formatter:off

	pbga (66)
	xhth (57)
	ebii (61)
	havc (66)
	ktlj (57)
	fwft (72) -> ktlj, cntj, xhth
	qoyq (66)
	padx (45) -> pbga, havc, qoyq
	tknk (41) -> ugml, padx, fwft
	jptl (61)
	ugml (68) -> gyxo, ebii, jptl
	gyxo (61)
	cntj (57)

                gyxo
              /     
         ugml - ebii
       /      \     
      |         jptl
      |        
      |         pbga
     /        /
tknk --- padx - havc
     \        \
      |         goyq
      |             
      |         ktlj
       \      /     
         fwft - cntj
              \     
                xhth

 * @formatter:on
 * 
 * @author jespe
 *
 */
public class Day07 extends AocPuzzle {

    public Day07() {
        super(2017, 7);
    }

    // @formatter:off
	private static final String[] PUZZLE_INPUT = {
			"pbga (66)",
			"xhth (57)",
			"ebii (61)",
			"havc (66)",
			"ktlj (57)",
			"fwft (72) -> ktlj, cntj, xhth",
			"qoyq (66)",
			"padx (45) -> pbga, havc, qoyq",
			"tknk (41) -> ugml, padx, fwft",
			"jptl (61)",
			"ugml (68) -> gyxo, ebii, jptl",
			"gyxo (61)",
			"cntj (57)"
	};
	// @formatter:on

    static class Node {
        String name;
        int weight;
        List<String> children = new ArrayList<>();

        public Node(String line) {
            String[] elems = line.split("[ ,\\(\\)]+");
            this.name = elems[0];
            this.weight = Integer.valueOf(elems[1]);
            for (int i = 3; i < elems.length; i++) {
                this.children.add(elems[i]);
            }
        }

        @Override
        public String toString() {
            return "Node [name=" + name + ", weight=" + weight + ", children="
                    + children + "]";
        }
    }

    Map<String, Node> nodes = new HashMap<>();
    Map<String, Node> parents = new HashMap<>();

    private Node getRootNode(String[] puzzleInput) {

        /*
         * Construct the graph using a hashmap of name -> node objects.
         */
        for (String line : puzzleInput) {
            Node node = new Node(line);
            nodes.put(node.name, node);
        }

        /*
         * Construct the reverse graph from children -> parents.
         */
        for (Entry<String, Node> entry : nodes.entrySet()) {
            for (String child : entry.getValue().children) {
                parents.put(child, nodes.get(entry.getKey()));
            }
        }

        /*
         * Take any node and follow it to the root. When the node has no parent,
         * we are at the bottom. (Assuming that the input is well-formed.)
         */
        Node node = nodes.values().iterator().next();
        while (true) {
            if (parents.containsKey(node.name)) {
                node = parents.get(node.name);
            } else {
                // Node has no parent, i.e. it must be the bottom-most one.
                return node;
            }
        }
    }

    private int getWeightOfUnbalancedProgram(String[] puzzleInput) {
        Node root = getRootNode(puzzleInput);
        return getWeightOfUnbalancedProgram(root);
    }

    private int getWeightOfUnbalancedProgram(Node node) {
        /*
         * The combined weight of all children must be the same. Given that
         * there is exactly one program which is unbalanced, what should its
         * weight be?
         */

        /*
         * Construct a map keeping track of all child weights.
         */
        Map<String, Integer> childWeights = node.children.stream()
                .map(n -> nodes.get(n)).collect(Collectors.toMap(n -> n.name,
                        n -> getWeightOfUnbalancedProgram(n)));

        /*
         * Collect the children weights into a map from weight to
         * "how many times it appears".
         */
        Map<Object, Long> grouped = node.children.stream()
                .collect(Collectors.groupingBy(name -> childWeights.get(name),
                        Collectors.counting()));

        /*
         * If this map contains more than one element, the node is unbalanced.
         */
        if (node.children.size() > 1 && grouped.size() > 1) {
            // This is the weight which is deviating from the others.
            int deviantWeight = grouped.entrySet().stream()
                    .filter(e -> e.getValue() == 1).findAny()
                    .map(e -> Integer.valueOf(e.getKey().toString())).orElse(0);

            Node deviatingNode = null;
            int normalWeight = 0;
            for (Entry<String, Integer> e : childWeights.entrySet()) {
                if (e.getValue() == deviantWeight) {
                    deviatingNode = nodes.get(e.getKey());
                } else {
                    normalWeight = e.getValue();
                }
            }

            int delta = -(deviantWeight - normalWeight);
            int adjustedWeight = deviatingNode.weight + delta;

            // Adjust the map of child weights, so that when we calculate
            // the weight below we get the right value.
            childWeights.put(deviatingNode.name,
                    childWeights.get(deviatingNode.name) + delta);

//            System.out.format(
//                    "The node %s weighing %d is unbalanced. Its correct weight should be %d%n",
//                    deviatingNode.name, deviatingNode.weight, adjustedWeight);
        }

        return node.weight + childWeights.values().stream()
                .collect(Collectors.summingInt(n -> n));
    }

    @Test
    public void testPart1_small() throws Exception {
        assertEquals("tknk", getRootNode(PUZZLE_INPUT).name);
    }

    @Test
    public void testPart2_small() {
        assertEquals(770, getWeightOfUnbalancedProgram(PUZZLE_INPUT));
    }

    @Test
    public void testPart1_full() throws Exception {
        String[] puzzleInput = getInputAsLines().toArray(n -> new String[n]);
        Node root = getRootNode(puzzleInput);
        assertEquals("xegshds", root.name);
    }

    @Test
    public void testPart2_full() throws Exception {
        String[] puzzleInput = getInputAsLines().toArray(n -> new String[n]);
        int weight = getWeightOfUnbalancedProgram(puzzleInput);
        /*
         * TODO the actual answer (299) to this part is printed by
         * getWeightOfUnbalancedProgram (see "adjustedWeight" variable).
         */
        assertEquals(510008, weight);
    }
}
