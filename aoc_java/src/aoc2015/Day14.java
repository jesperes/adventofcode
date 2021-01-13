package aoc2015;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import aoc2015.Day14.Reindeer;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day14 implements IAocIntPuzzle<List<Reindeer>> {

    record Reindeer(String name, int speed, int flyTime, int restTime) {
    }

    class ReindeerFlightInfo {
        final private Reindeer reindeer;
        boolean flying = true;
        int timeLeft;
        int step = 0;
        int score = 0;
        int distance = 0;

        public ReindeerFlightInfo(Reindeer reindeer) {
            this.reindeer = reindeer;
            this.distance = 0;
            this.timeLeft = reindeer.flyTime;
        }

        void nextStep() {
            step++;
            timeLeft--;

            if (flying)
                distance += reindeer.speed;

            if (timeLeft == 0) {
                flying = !flying;
                timeLeft = flying ? reindeer.flyTime : reindeer.restTime;
            }
        }
    }

//    @Test
//    public void testDay14() throws IOException {
//        List<Reindeer> reindeers = new ArrayList<>();
//
//        try (BufferedReader reader = new BufferedReader(
//                new FileReader("inputs/2015/day14.txt"))) {
//            String line;
//
//            while ((line = reader.readLine()) != null) {
//                String[] s = line.split(" ");
//                String name = s[0];
//                int speed = Integer.valueOf(s[3]);
//                int time = Integer.valueOf(s[6]);
//                int rest = Integer.valueOf(s[13]);
//                Reindeer r = new Reindeer(name, speed, time, rest);
//                reindeers.add(r);
//            }
//        }
//
//        for (int i = 0; i < 2503; i++) {
//            for (Reindeer r : reindeers) {
//                r.nextStep();
//            }
//
//            int max = reindeers.stream().mapToInt(r -> r.distance).max()
//                    .getAsInt();
//
//            for (Reindeer r : reindeers) {
//                if (r.distance == max) {
//                    r.score++;
//                }
//            }
//        }
//
//        assertEquals(2655,
//                reindeers.stream().mapToInt(r -> r.distance).max().getAsInt());
//
//        assertEquals(1059,
//                reindeers.stream().mapToInt(r -> r.score).max().getAsInt());
//    }
//
    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2015, 14, "Reindeer Olympics", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(2655, 1059);
    }

    @Override
    public List<Reindeer> parse(Optional<File> file) throws IOException {
        return InputUtils.asStringList(file.get()).stream().map(line -> {
            String[] s = line.split(" ");
            String name = s[0];
            int speed = Integer.valueOf(s[3]);
            int time = Integer.valueOf(s[6]);
            int rest = Integer.valueOf(s[13]);
            return new Reindeer(name, speed, time, rest);
        }).collect(Collectors.toUnmodifiableList());
    }

    @Override
    public Integer part1(List<Reindeer> reindeers) {
        var list = reindeers.stream().map(r -> new ReindeerFlightInfo(r))
                .collect(Collectors.toList());

        for (int i = 0; i < 2503; i++) {
            for (var r : list) {
                r.nextStep();
            }
        }

        return list.stream().mapToInt(r -> r.distance).max().getAsInt();
    }

    @Override
    public Integer part2(List<Reindeer> reindeers) {
        var list = reindeers.stream().map(r -> new ReindeerFlightInfo(r))
                .collect(Collectors.toList());

        for (int i = 0; i < 2503; i++) {
            for (var r : list) {
                r.nextStep();
            }

            int max = list.stream().mapToInt(r -> r.distance).max().getAsInt();

            for (var r : list) {
                if (r.distance == max) {
                    r.score++;
                }
            }
        }

        return list.stream().mapToInt(r -> r.score).max().getAsInt();
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day14());
    }
}
