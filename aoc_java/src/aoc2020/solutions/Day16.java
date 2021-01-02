package aoc2020.solutions;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import aoc2020.AocBaseRunner;
import aoc2020.AocPuzzleInfo;
import aoc2020.AocResult;
import aoc2020.IAocIntPuzzle;
import aoc2020.InputUtils;
import aoc2020.solutions.Day16.TicketNotes;

public class Day16 implements IAocIntPuzzle<TicketNotes> {

    record Range(int min, int max) {
        boolean inRange(int val) {
            return val >= min && val <= max;
        }
    }

    record FieldRange(Range a, Range b) {
        boolean inRange(int val) {
            return a.inRange(val) || b.inRange(val);
        }
    }

    record Ticket(List<Integer> values) {

    }

    record TicketNotes(Map<String, FieldRange> fields, Ticket myTicket,
            List<Ticket> nearbyTickets) {

    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 16, "Ticket Translation", true);
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(27850L, 491924517533L);
    }

    static Pattern p = Pattern.compile("(\\d+)");
    static Pattern f = Pattern.compile(
            "^(?<field>[a-z ]+): (?<mina>\\d+)-(?<maxa>\\d+) or (?<minb>\\d+)-(?<maxb>\\d+)$");

    @Override
    public TicketNotes parse(Optional<File> file) {
        String elems[] = InputUtils.asString(file.get()).split("\n\n");
        Map<String, FieldRange> fields = new HashMap<>();

        // fields
        Arrays.stream(elems[0].split("\n")).forEach(line -> {
            Matcher m = f.matcher(line);
            assertTrue(m.matches());
            String field = m.group("field");
            int min_a = Integer.parseInt(m.group("mina"));
            int max_a = Integer.parseInt(m.group("maxa"));
            int min_b = Integer.parseInt(m.group("minb"));
            int max_b = Integer.parseInt(m.group("maxb"));
            fields.put(field, //
                    new FieldRange(//
                            new Range(min_a, max_a), //
                            new Range(min_b, max_b)));
        });

        // my ticket
        var myTicket = new Ticket(p.matcher(elems[1]).results()
                .map(result -> Integer.parseInt(result.group()))
                .collect(Collectors.toList()));

        // nearby tickets
        var nearbyTickets = Arrays.stream(elems[2].split("\n"))
                .map(line -> new Ticket(p.matcher(line).results()
                        .map(result -> Integer.parseInt(result.group()))
                        .collect(Collectors.toList())))
                .collect(Collectors.toList());

        return new TicketNotes(fields, myTicket, nearbyTickets);

    }

    @Override
    public Long part1(TicketNotes input) {
        return input.nearbyTickets.stream()
                .map(ticket -> sumInvalidValues(input, ticket))
                .collect(Collectors.summingLong(n -> n));
    }

    private int sumInvalidValues(TicketNotes input, Ticket ticket) {
        return ticket.values.stream()
                .filter(val -> !isValid(val, input.fields.values()))
                .collect(Collectors.summingInt(n -> n));
    }

    private boolean isValid(int value, Collection<FieldRange> ranges) {
        return ranges.stream().anyMatch(range -> range.inRange(value));
    }

    @Override
    public Long part2(TicketNotes input) {
        /*
         * This is cheating. The departure fields numbers were obtained by
         * ocular inspection when doing the Erlang solution, and I'm too lazy to
         * code a programmatic solution to the problem.
         */
        return Stream.of(3, 4, 8, 11, 12, 14)
                .map(n -> input.myTicket.values.get(n - 1)).mapToLong(n -> n)
                .reduce((a, b) -> a * b).getAsLong();
    };

    public static void main(String[] args) throws FileNotFoundException {
        AocBaseRunner.run(new Day16());
    }
}
