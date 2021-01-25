package aoc2017;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.stream.Collectors;

import aoc2017.Day07.Node;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;
import common2.InputUtils;

public class Day07 implements IAocPuzzle<Node, String, Integer> {

    public static class Node {
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

    class UnbalancedProgramException extends RuntimeException {
        private static final long serialVersionUID = 1L;
        int weight;

        public UnbalancedProgramException(int weight) {
            this.weight = weight;
        }
    }

    private int getWeightOfUnbalancedProgram(Node node)
            throws UnbalancedProgramException {
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
         * The puzzle require us to calculate the correct weight of the node for
         * it to balance the entire table.
         */
        if (node.children.size() > 1 && grouped.size() > 1) {
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

            throw new UnbalancedProgramException(
                    deviatingNode.weight - deviantWeight + normalWeight);
        }

        return node.weight + childWeights.values().stream()
                .collect(Collectors.summingInt(n -> n));
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2017, 7, "Recursive Circus", true);
    }

    @Override
    public AocResult<String, Integer> getExpected() {
        return AocResult.of("xegshds", 299);
    }

    @Override
    public Node parse(Optional<File> file) throws IOException {

        for (String line : InputUtils.asStringList(file.get())) {
            Node node = new Node(line);
            nodes.put(node.name, node);
        }

        for (Entry<String, Node> entry : nodes.entrySet()) {
            for (String child : entry.getValue().children) {
                parents.put(child, nodes.get(entry.getKey()));
            }
        }

        Node node = nodes.values().iterator().next();
        while (true) {
            if (parents.containsKey(node.name)) {
                node = parents.get(node.name);
            } else {
                return node;
            }
        }
    }

    @Override
    public String part1(Node input) {
        return input.name;
    }

    @Override
    public Integer part2(Node input) {
        try {
            getWeightOfUnbalancedProgram(input);
            throw new RuntimeException();
        } catch (UnbalancedProgramException e) {
            return e.weight;
        }
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day07());
    }
}
