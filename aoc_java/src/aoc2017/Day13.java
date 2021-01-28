package aoc2017;

import java.io.File;
import java.io.IOException;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import aoc2017.Day13.Firewall;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day13 implements IAocIntPuzzle<Map<Integer, Firewall>> {

    record Firewall(int depth, int range) {
        public boolean isAtTop(int time) {
            return time % ((range * 2) - 2) == 0;
        }

        public int getSeverity() {
            return depth * range;
        }
    }

    private int getMaxDepth(Map<Integer, Firewall> firewalls) {
        return firewalls.keySet().stream().mapToInt(n -> n).max().getAsInt();
    }

    private int getMaxRange(Map<Integer, Firewall> firewalls) {
        return firewalls.values().stream().map(fw -> fw.range).mapToInt(n -> n)
                .max().getAsInt();
    }

    private Optional<Integer> computeSeverity(Map<Integer, Firewall> firewalls,
            int maxDepth, int maxRange, int delay, boolean onlyCollisions) {

        int time = delay;
        int severity = 0;
        int collisions = 0;

        for (int i = 0; i <= maxDepth; i++) {
            if (firewalls.containsKey(i)) {
                Firewall fw = firewalls.get(i);
                if (fw.isAtTop(time)) {
                    collisions++;
                    severity += fw.getSeverity();
                    if (onlyCollisions) {
                        return Optional.of(1);
                    }
                }
            }

            // advance time
            time++;
        }

        if (collisions == 0)
            return Optional.empty();
        else
            return Optional.of(severity);
    }

    int computeDelayUntilNotCaught(Map<Integer, Firewall> firewalls,
            int maxDepth, int maxRange) {
        for (int delay = 0;; delay++) {
            if (!computeSeverity(firewalls, maxDepth, maxRange, delay, true)
                    .isPresent()) {
                return delay;
            }
        }
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2017, 13, "Packet Scanners", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(788, 3905748);
    }

    @Override
    public Map<Integer, Firewall> parse(Optional<File> file)
            throws IOException {
        return InputUtils.asStringList(file.get()).stream().map(s -> {
            String[] elems = s.split(":");
            return new Firewall(Integer.valueOf(elems[0].trim()),
                    Integer.valueOf(elems[1].trim()));
        }).collect(Collectors.toMap(fw -> fw.depth, Function.identity()));
    }

    @Override
    public Integer part1(Map<Integer, Firewall> firewalls) {
        return computeSeverity(firewalls, getMaxDepth(firewalls),
                getMaxRange(firewalls), 0, false).get();
    }

    @Override
    public Integer part2(Map<Integer, Firewall> firewalls) {
        return computeDelayUntilNotCaught(firewalls, getMaxDepth(firewalls),
                getMaxRange(firewalls));
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day13());
    }
}
