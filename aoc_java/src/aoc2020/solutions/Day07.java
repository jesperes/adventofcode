package aoc2020.solutions;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.jgrapht.graph.DefaultWeightedEdge;
import org.jgrapht.graph.EdgeReversedGraph;
import org.jgrapht.graph.SimpleDirectedWeightedGraph;
import org.jgrapht.traverse.DepthFirstIterator;

import aoc2020.AocPuzzleInfo;
import aoc2020.AocResult;
import aoc2020.IAocPuzzle;
import aoc2020.solutions.Day07.Rules;

public class Day07 implements IAocPuzzle<Rules, Long, Long> {

    Pattern linePattern = Pattern
            .compile("^(?<bag>.*) bags contain (?<contents>.*)\\.$");

    Pattern contentPattern = Pattern
            .compile("(?<num>\\d+) (?<color>[a-z]+ [a-z]+) (bag|bags)");

    record Rules(
            SimpleDirectedWeightedGraph<String, DefaultWeightedEdge> graph) {
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(355L, 5312L);
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 7, "Handy Haversacks", true);
    }

    @Override
    public Rules parse(Optional<File> file) {
        var graph = new SimpleDirectedWeightedGraph<String, DefaultWeightedEdge>(
                DefaultWeightedEdge.class);
        var rules = new Rules(graph);

        try (Stream<String> lines = Files.lines(file.get().toPath())) {
            lines.forEach(line -> parseLine(rules, line));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        return rules;
    }

    private void parseLine(Rules rules, String line) {
        Matcher m = linePattern.matcher(line);
        if (m.matches()) {
            String bag = m.group("bag");
            String contents = m.group("contents");

            rules.graph.addVertex(bag);

            contentPattern.matcher(contents).results().forEach(result -> {
                String color = result.group(2);
                rules.graph.addVertex(color);
                DefaultWeightedEdge edge = rules.graph.addEdge(bag, color);
                rules.graph.setEdgeWeight(edge,
                        Integer.parseInt(result.group(1)));
            });
        } else {
            throw new RuntimeException();
        }
    }

    @Override
    public Long part1(Rules input) {
        var rgraph = new EdgeReversedGraph<>(input.graph());

        DepthFirstIterator<String, DefaultWeightedEdge> it = new DepthFirstIterator<>(
                rgraph, "shiny gold");

        Set<String> reachable = new HashSet<>();
        while (it.hasNext()) {
            reachable.add(it.next());
        }
        reachable.remove("shiny gold");
        return (long) reachable.size();
    }

    @Override
    public Long part2(Rules rules) {
        return contains(rules, "shiny gold");
    }

    private Long contains(Rules rules, String string) {
        var graph = rules.graph();
        long n = 0;
        for (DefaultWeightedEdge edge : graph.outgoingEdgesOf(string)) {
            var child = graph.getEdgeTarget(edge);
            n += graph.getEdgeWeight(edge) * (1 + contains(rules, child));
        }
        return n;
    }
}
