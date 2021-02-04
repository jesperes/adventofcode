package aoc2018;

import static common2.MapUtils.mapValueComparator;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.google.common.collect.ComparisonChain;

import aoc2018.Day04.Note;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day04 implements IAocIntPuzzle<List<Note>> {

    Pattern p = Pattern.compile(
            "\\[(?<date>.*) (?<hour>\\d\\d):(?<minute>\\d\\d)\\] (?<s>Guard #(?<id>\\d+) begins shift|wakes up|falls asleep)");

    enum Status {
        BeginShift, WakesUp, FallsAsleep
    }

    record Note(String date, Time time, int id, Status status)
            implements Comparable<Note> {
        @Override
        public int compareTo(Note o) {
            return ComparisonChain.start().compare(date, o.date)
                    .compare(time, o.time).result();
        }
    }

    record Time(int hour, int minute) implements Comparable<Time> {
        @Override
        public int compareTo(Time o) {
            return ComparisonChain.start().compare(hour, o.hour)
                    .compare(minute, o.minute).result();
        }
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2018, 4, "Repose Record", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(140932, 51232);
    }

    /*
     * This puzzle's runtime is dominated by parsing. Regexps are slow.
     */
    @Override
    public List<Note> parse(Optional<File> file) throws IOException {
        return InputUtils.asStringList(file.get()).stream().map(line -> {
            var m = p.matcher(line);
            assertTrue(m.matches());
            String date = m.group("date");
            int hour = Integer.parseInt(m.group("hour"));
            int minute = Integer.parseInt(m.group("minute"));
            Time time = new Time(hour, minute);
            String s = m.group("s");
            if (s.equals("falls asleep")) {
                return new Note(date, time, -1, Status.FallsAsleep);
            } else if (s.equals("wakes up")) {
                return new Note(date, time, -1, Status.WakesUp);
            } else if (s.endsWith("begins shift")) {
                int id = Integer.parseInt(m.group("id"));
                assertTrue(id > 0);
                return new Note(date, time, id, Status.BeginShift);
            } else {
                throw new RuntimeException();
            }
        }).sorted().collect(Collectors.toList());
    }

    @Override
    public Integer part1(List<Note> input) {
        Map<Integer, Integer> map = new HashMap<>();
        int guardId = -1;
        Time sleepStart = null;

        for (Note note : input) {
            switch (note.status) {
            case BeginShift:
                guardId = note.id;
                break;
            case FallsAsleep:
                assertEquals(0, note.time.hour);
                sleepStart = note.time;
                break;
            case WakesUp:
                assertEquals(0, note.time.hour);
                assertNotNull(sleepStart);
                int minutesSlept = (note.time.minute - sleepStart.minute);
                map.compute(guardId, (k, v) -> (v == null) ? minutesSlept
                        : v + minutesSlept);
                break;
            default:
                throw new RuntimeException();
            }
        }

        // Find the guard with most accumulated sleep minutes
        final int mostSleepyGuard = map.entrySet().stream()
                .max(mapValueComparator()).get().getKey();

        // For each minute, keep a map of how many times this guard has slept
        // during that minute.
        Map<Integer, Integer> minutes = new HashMap<>();

        sleepStart = null;
        int id = -1;
        for (Note note : input) {
            if (note.status == Status.BeginShift) {
                id = note.id;
                continue;
            }

            if (id != mostSleepyGuard)
                continue; // this is not our guard

            if (note.status == Status.FallsAsleep) {
                sleepStart = note.time;
                continue;
            }

            assertEquals(Status.WakesUp, note.status);
            for (int i = sleepStart.minute; i < note.time.minute; i++) {
                minutes.compute(i, (k, v) -> (v == null) ? 1 : v + 1);
            }
        }

        int mostSleepyMinute = minutes.entrySet().stream()
                .max(mapValueComparator()).get().getKey();

        return mostSleepyMinute * mostSleepyGuard;

    }

    @Override
    public Integer part2(List<Note> input) {
        /*
         * This is a map of (guard-id * minute) -> (how many times this guard
         * was asleep during that minute). This is choosen since the puzzle's
         * answer then becomes the key which has the largest value. (I suspect
         * that the guard-ids were chosen so that this would be possible.)
         */
        Map<Integer, Integer> map = new HashMap<>();

        int guardId = -1;
        int fellAsleepAt = -1;
        for (Note note : input) {
            switch (note.status) {
            case BeginShift:
                guardId = note.id;
                break;
            case FallsAsleep:
                fellAsleepAt = note.time.minute;
                break;
            case WakesUp:
                for (int i = fellAsleepAt; i < note.time.minute; i++) {
                    map.compute(guardId * i, (k, v) -> (v == null) ? 1 : v + 1);
                }
                break;
            }
        }

        return map.entrySet().stream().max(mapValueComparator()).get().getKey();
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day04());
    }

}
