package aoc2018;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Optional;

import aoc2018.Day09.Input;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocLongPuzzle;

public class Day09 implements IAocLongPuzzle<Input> {

    record Input(int players, int points) {

    }

    class Marble {
        long n;
        Marble prev; // counter-clockwise direction
        Marble next; // clockwise direction

        public Marble(long n) {
            this.n = n;
        }
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2018, 9, "Marble Mania", false);
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(423717L, 3553108197L);
    }

    @Override
    public Input parse(Optional<File> file) throws IOException {
        return new Input(419, 72164);
    }

    @Override
    public Long part1(Input input) {
        long[] score = new long[input.players];
        Marble current = new Marble(0L);
        current.prev = current;
        current.next = current;

        for (long marbleNum = 1; marbleNum < input.points; marbleNum++) {
            int playerIdx = (int) ((marbleNum - 1) % input.players);
            if (marbleNum % 23 == 0) {
                long value = marbleNum;
                Marble marble = current.prev.prev.prev.prev.prev.prev.prev;
                value += marble.n;
                current = marble.next;
                remove(marble);
                score[playerIdx] += value;
            } else {
                current = insertCW(current.next, marbleNum);
            }
        }

        return Arrays.stream(score).max().getAsLong();
    }

    @Override
    public Long part2(Input input) {
        return part1(new Input(input.players, input.points * 100));
    }

    /*
     * Helpers
     */
    Marble insertCW(Marble current, long n) {
        Marble marble = new Marble(n);
        marble.next = current.next;
        marble.prev = current;
        current.next.prev = marble;
        current.next = marble;
        return marble;
    }

    void remove(Marble marble) {
        Marble prev = marble.prev;
        Marble next = marble.next;
        prev.next = next;
        next.prev = prev;
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day09());
    }
}
