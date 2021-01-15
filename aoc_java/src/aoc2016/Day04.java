package aoc2016;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import aoc2016.Day04.Room;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day04 implements IAocIntPuzzle<List<Room>> {

    final String PART2 = "northpole object storage";

    record Room(String name, int sectorId, String checksum) {
    }

    Pattern p = Pattern.compile(
            "^(?<name>[a-z\\-]+)-(?<sectorid>\\d+)\\[(?<checksum>.*)\\]$");

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2016, 4, "Security Through Obscurity", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(185371, 984);
    }

    @Override
    public List<Room> parse(Optional<File> file) throws IOException {
        return InputUtils.asStringList(file.get()).stream().map(line -> {
            var m = p.matcher(line);
            assertTrue(m.matches());
            return new Room(m.group("name"),
                    Integer.parseInt(m.group("sectorid")), m.group("checksum"));
        }).collect(Collectors.toUnmodifiableList());
    }

    private static boolean isRealRoom(Room room) {
        return freqMap(room.name).entrySet().stream().sorted((e1, e2) -> {
            Integer c1 = e1.getValue();
            Integer c2 = e2.getValue();
            if (c1 == c2) {
                return e1.getKey().compareTo(e2.getKey());
            } else {
                return c2.compareTo(c1);
            }
        }).limit(5L).map(e -> String.valueOf(e.getKey()))
                .collect(Collectors.joining()).equals(room.checksum);
    }

    private static Map<Character, Integer> freqMap(String s) {
        Map<Character, Integer> map = new HashMap<>();
        for (char c : s.toCharArray()) {
            if (c == '-')
                continue;
            map.put(c, map.getOrDefault(c, 0) + 1);
        }
        return map;
    }

    private static String decrypt(Room room) {
        return room.name.chars().mapToObj(c -> (c == '-') ? " "
                : String.valueOf(
                        (char) (((c - 'a') + room.sectorId) % 26 + 'a')))
                .collect(Collectors.joining());
    }

    @Override
    public Integer part1(List<Room> input) {
        return input.stream().filter(Day04::isRealRoom)
                .collect(Collectors.summingInt(room -> room.sectorId));
    }

    @Override
    public Integer part2(List<Room> input) {
        return input.stream().filter(room -> decrypt(room).equals(PART2))
                .findFirst().get().sectorId;
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day04());
    }
}
