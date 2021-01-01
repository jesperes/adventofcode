package aoc2020.solutions;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
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
import org.junit.Test;

import aoc2020.AocPuzzleInfo;
import aoc2020.AocResult;
import aoc2020.IAocPuzzle;
import aoc2020.InputUtils;
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
    public Rules parse(Optional<InputStream> stream) {
        var graph = new SimpleDirectedWeightedGraph<String, DefaultWeightedEdge>(
                DefaultWeightedEdge.class);
        var rules = new Rules(graph);

        try (Stream<String> lines = InputUtils.asLines(stream.get())) {
            lines.forEach(line -> parseLine(rules, line));
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

    @Test
    public void testEx1() throws Exception {
        String input = """
                light red bags contain 1 bright white bag, 2 muted yellow bags.
                dark orange bags contain 3 bright white bags, 4 muted yellow bags.
                bright white bags contain 1 shiny gold bag.
                muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
                shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
                dark olive bags contain 3 faded blue bags, 4 dotted black bags.
                vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
                faded blue bags contain no other bags.
                dotted black bags contain no other bags.
                """;
        try (InputStream stream = new ByteArrayInputStream(input.getBytes())) {
            Rules rules = parse(Optional.of(stream));
            assertEquals(9, rules.graph().vertexSet().size());
            assertEquals(4L, (long) part1(rules));
        }
    }

    @Test
    public void testEx2() throws Exception {
        String input = """
                shiny gold bags contain 2 dark red bags.
                dark red bags contain 2 dark orange bags.
                dark orange bags contain 2 dark yellow bags.
                dark yellow bags contain 2 dark green bags.
                dark green bags contain 2 dark blue bags.
                dark blue bags contain 2 dark violet bags.
                dark violet bags contain no other bags.
                """;
        try (InputStream stream = new ByteArrayInputStream(input.getBytes())) {
            Rules rules = parse(Optional.of(stream));
            assertEquals(126L, (long) part2(rules));
        }
    }
}
