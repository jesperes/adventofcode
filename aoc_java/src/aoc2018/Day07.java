package aoc2018;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.traverse.TopologicalOrderIterator;
import org.junit.Test;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;
import common2.InputUtils;

public class Day07
        implements IAocPuzzle<Graph<String, DefaultEdge>, String, Integer> {

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2018, 7, "The Sum of Its Parts", true);
    }

    @Override
    public AocResult<String, Integer> getExpected() {
        return AocResult.of("BFLNGIRUSJXEHKQPVTYOCZDWMA", 880);
    }

    @Override
    public Graph<String, DefaultEdge> parse(Optional<File> file)
            throws IOException {
        Graph<String, DefaultEdge> g = new DefaultDirectedGraph<>(
                DefaultEdge.class);
        InputUtils.asStringList(file.get()).stream().forEach(line -> {
            String[] elems = line.split(" ");
            String before = elems[1];
            String after = elems[7];
            g.addVertex(before);
            g.addVertex(after);
            g.addEdge(before, after);
        });
        return g;
    }

    String getTopoOrder(Graph<String, DefaultEdge> graph) {
        var iter = new TopologicalOrderIterator<>(graph,
                new Comparator<String>() {
                    @Override
                    public int compare(String o1, String o2) {
                        return o1.compareTo(o2);
                    }
                });
        StringBuilder sb = new StringBuilder();
        while (iter.hasNext()) {
            sb.append(iter.next());
        }
        return sb.toString();
    }

    @Override
    public String part1(Graph<String, DefaultEdge> graph) {
        return getTopoOrder(graph);
    }

    boolean isStepAvailable(Graph<String, DefaultEdge> graph, String step,
            Collection<String> done) {
        for (var beforeEdge : graph.incomingEdgesOf(step)) {
            if (!done.contains(graph.getEdgeSource(beforeEdge)))
                return false;
        }
        return true;
    }

    Optional<String> getNextAvailableStep(Graph<String, DefaultEdge> graph,
            Collection<String> done) {
        return graph.vertexSet().stream()
                .filter(step -> isStepAvailable(graph, step, done)).sorted()
                .findFirst();
    }

    @Override
    public Integer part2(Graph<String, DefaultEdge> graph) {
        return part2(graph, 5, 60);
    }

    int part2(Graph<String, DefaultEdge> graph, int numWorkers,
            int minStepTime) {
        List<String> done = new ArrayList<>();
        int numSteps = graph.vertexSet().size();
        int second = 0;
        String[] workerUnit = new String[numWorkers];
        int[] workerTimeLeft = new int[numWorkers];

        while (done.size() < numSteps) {
            for (int i = 0; i < numWorkers; i++) {
                if (workerTimeLeft[i] == 0) {
                    // worker has finished, pick next available step

                    String unit = workerUnit[i];
                    if (unit != null) {
                        done.add(unit);
                    }

                    var maybeUnit = getNextAvailableStep(graph, done);
                    if (maybeUnit.isEmpty()) {
                        // no steps left
                        break;
                    }
                    unit = maybeUnit.get();
                    graph.removeVertex(unit);
                    System.out
                            .println("Unit done, removing from graph: " + unit);
                    workerUnit[i] = unit;
                    workerTimeLeft[i] = minStepTime + (unit.charAt(0) - 'A')
                            + 1;

                } else {
                    workerTimeLeft[i]--;
                }
            }
            second++;
        }

        return second;
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day07());
    }

    @Test
    public void testName() throws Exception {
        String input = """
                Step C must be finished before step A can begin.
                Step C must be finished before step F can begin.
                Step A must be finished before step B can begin.
                Step A must be finished before step D can begin.
                Step B must be finished before step E can begin.
                Step D must be finished before step E can begin.
                Step F must be finished before step E can begin.
                """;

        Graph<String, DefaultEdge> graph = InputUtils.parseTestData(this,
                input);

        assertEquals(15, part2(graph, 2, 0));
    }
}
