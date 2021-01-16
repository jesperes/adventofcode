package common2;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

public record AocPuzzleRun<T, P1, P2> (IAocPuzzle<T, P1, P2> puzzle,
        AocResult<P1, P2> result) {

    public Map<AocResultTableField, String> toTableRow() {
        final Map<AocResultTableField, String> map = new HashMap<>();
        final var info = puzzle.getInfo();
        final var timing = result.timing();
        final var expected = puzzle.getExpected();

        map.put(AocResultTableField.Name, info.name());
        map.put(AocResultTableField.Year, String.valueOf(info.year()));
        map.put(AocResultTableField.Day, String.valueOf(info.day()));

        if (timing.isPresent()) {
            if (info.hasInputFile())
                map.put(AocResultTableField.Parsing,
                        timeToStr(timing.get().parsing()));

            map.put(AocResultTableField.Part1Time,
                    timeToStr(timing.get().part1()));
            map.put(AocResultTableField.Part2Time,
                    timeToStr(timing.get().part2()));
            map.put(AocResultTableField.TotalTime,
                    timeToStr(timing.get().total()));
        }

        if (result.p1().isPresent()) {
            map.put(AocResultTableField.Part1Result,
                    result.p1().get().toString());
            map.put(AocResultTableField.Part1Status,
                    statusBooleanToString(result.p1().equals(expected.p1())));
        }

        if (result.p2().isPresent()) {
            map.put(AocResultTableField.Part2Result,
                    result.p2().get().toString());
            map.put(AocResultTableField.Part2Status,
                    statusBooleanToString(result.p2().equals(expected.p2())));
        }

        return map;
    }

    private String statusBooleanToString(boolean status) {
        return status ? "OK" : "*** FAILED ***";
    }

    private String timeToStr(long nanosecs) {
        return String.format(Locale.ROOT, "%.3f", nanosecs / 1000000.0);
    }
}
