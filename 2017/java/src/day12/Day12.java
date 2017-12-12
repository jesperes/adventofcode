package day12;

import static org.junit.Assert.*;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import org.junit.Test;

import utils.Utils;

public class Day12 {

    // @formatter:off
    static String SMALL_INPUT = "0 <-> 2\n" + 
            "1 <-> 1\n" + 
            "2 <-> 0, 3, 4\n" + 
            "3 <-> 2, 4\n" + 
            "4 <-> 2, 3, 6\n" + 
            "5 <-> 6\n" + 
            "6 <-> 4, 5\n"; 
    // @formatter:on

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

    @Test
    public void testSmall() throws Exception {
        Map<Integer, Set<Integer>> graph = computeGraph(SMALL_INPUT);
        assertEquals(6, getConnectedSet(graph, 0).size());
        assertEquals(2, getTotalNumberOfGroups(graph));
    }

    @Test
    public void testLarge() throws Exception {
        String input = Utils.readFileFromClassPathAsString(getClass(),
                "day12/input.txt");
        Map<Integer, Set<Integer>> graph = computeGraph(input);
        System.out.println("[Day12] Size of program group 0: "
                + getConnectedSet(graph, 0).size());
        System.out.println("[Day12] Total number of groups: "
                + getTotalNumberOfGroups(graph));
    }
}
