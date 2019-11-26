package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

public class Day14 {
    class Reindeer {

        final String name;
        final int speed;
        final int flyTime;
        final int restTime;

        boolean flying = true;
        int timeLeft;
        int step = 0;
        int score = 0;
        int distance = 0;

        public Reindeer(String name, int speed, int time, int rest) {
            this.name = name;
            this.speed = speed;
            this.flyTime = time;
            this.distance = 0;
            this.restTime = rest;
            this.timeLeft = flyTime;
        }

        void nextStep() {
            step++;
            timeLeft--;

            if (flying)
                distance += speed;

            if (timeLeft == 0) {
                flying = !flying;
                timeLeft = flying ? flyTime : restTime;
            }
        }
    }

    @Test
    public void testDay14() throws IOException {
        List<Reindeer> reindeers = new ArrayList<>();

        try (BufferedReader reader = new BufferedReader(
                new FileReader("inputs/2015/day14.txt"))) {
            String line;

            while ((line = reader.readLine()) != null) {
                String[] s = line.split(" ");
                String name = s[0];
                int speed = Integer.valueOf(s[3]);
                int time = Integer.valueOf(s[6]);
                int rest = Integer.valueOf(s[13]);
                Reindeer r = new Reindeer(name, speed, time, rest);
                reindeers.add(r);
            }
        }

        for (int i = 0; i < 2503; i++) {
            for (Reindeer r : reindeers) {
                r.nextStep();
            }

            int max = reindeers.stream().mapToInt(r -> r.distance).max()
                    .getAsInt();

            for (Reindeer r : reindeers) {
                if (r.distance == max) {
                    r.score++;
                }
            }
        }

        assertEquals(2655,
                reindeers.stream().mapToInt(r -> r.distance).max().getAsInt());

        assertEquals(1059,
                reindeers.stream().mapToInt(r -> r.score).max().getAsInt());
    }
}
