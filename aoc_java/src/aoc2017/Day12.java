package aoc2017;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day12 implements IAocIntPuzzle<Map<Integer, Set<Integer>>> {

    private void addLink(Map<Integer, Set<Integer>> graph, int a, int b) {
        if (!graph.containsKey(a)) {
            graph.put(a, new HashSet<>());
        }

        if (!graph.containsKey(b)) {
            graph.put(b, new HashSet<>());
        }

        graph.get(a).add(b);
        graph.get(b).add(a);
    }

    private Map<Integer, Set<Integer>> computeGraph(String input) {
        Map<Integer, Set<Integer>> neighborMap = new HashMap<>();

        for (String line : input.split("[\\n\\r]+")) {
            String[] elems = line.split("[ ,]+");

            int program = Integer.valueOf(elems[0]);

            for (int i = 2; i < elems.length; i++) {
                addLink(neighborMap, program, Integer.valueOf(elems[i]));
            }
        }

        return neighborMap;
    }

    private Set<Integer> getConnectedSet(Map<Integer, Set<Integer>> graph,
            int start) {
        Set<Integer> set = new HashSet<>();
        Queue<Integer> tovisit = new LinkedList<>();

        tovisit.add(start);
        while (!tovisit.isEmpty()) {
            int node = tovisit.poll();
            set.add(node);

            for (int neighbor : graph.get(node)) {
                if (set.contains(neighbor))
                    continue;
                else {
                    tovisit.add(neighbor);
                }
            }
        }

        return set;
    }

    private int getTotalNumberOfGroups(Map<Integer, Set<Integer>> graph) {
        Map<Integer, Set<Integer>> all = new HashMap<>(graph);

        int numberOfConnectedSets = 0;

        while (!all.isEmpty()) {
            // Take any number, and construct the connected set containing it:
            int next = all.keySet().iterator().next();
            Set<Integer> connectedSet = getConnectedSet(all, next);
            numberOfConnectedSets++;

            // Remove the connected node from the set
            for (int connectedNode : connectedSet) {
                all.remove(connectedNode);
            }
        }

        return numberOfConnectedSets;
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2017, 12, "Digital Plumber", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(141, 171);
    }

    @Override
    public Map<Integer, Set<Integer>> parse(Optional<File> file)
            throws IOException {
        return computeGraph(InputUtils.asString(file.get()));
    }

    @Override
    public Integer part1(Map<Integer, Set<Integer>> input) {
        return getConnectedSet(input, 0).size();
    }

    @Override
    public Integer part2(Map<Integer, Set<Integer>> input) {
        return getTotalNumberOfGroups(input);
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day12());
    }
}
