package aoc2020;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import aoc2020.solutions.Day01;
import aoc2020.solutions.Day02;
import aoc2020.solutions.Day03;
import aoc2020.solutions.Day04;
import aoc2020.solutions.Day05;
import aoc2020.solutions.Day06;
import aoc2020.solutions.Day07;
import aoc2020.solutions.Day08;
import aoc2020.solutions.Day11;
import aoc2020.solutions.Day15;

public class Aoc2020 {

    public static void main(String[] args) throws IOException {
        final List<IAocPuzzle<?, ?, ?>> puzzles = new ArrayList<>();

        // =====================================================
        puzzles.add(new Day01());
        puzzles.add(new Day02());
        puzzles.add(new Day03());
        puzzles.add(new Day04());
        puzzles.add(new Day05());
        puzzles.add(new Day06());
        puzzles.add(new Day07());
        puzzles.add(new Day08());
        puzzles.add(new Day11());
        puzzles.add(new Day15());
        // =====================================================

        final var runs = runPuzzles(puzzles);
        printTable(runs);
    }

    private static void printTable(List<AocPuzzleRun<?, ?, ?>> runs)
            throws IOException {
        final var sep = "@";
        final var cmd = new StringBuilder();

        final var columns = new AocResultTableField[] { //
                AocResultTableField.Year, //
                AocResultTableField.Day, //
                AocResultTableField.Name, //
                AocResultTableField.Parsing, //
                AocResultTableField.Part1Time, //
                AocResultTableField.Part2Time, //
                AocResultTableField.TotalTime, //
                AocResultTableField.Part1Result, //
                AocResultTableField.Part2Result, //
                AocResultTableField.Part1Status, //
                AocResultTableField.Part2Status //
        };

        // Headers
        cmd.append(Arrays.stream(columns).map(col -> col.label)
                .collect(Collectors.joining(sep)) + "\n");

        for (final AocPuzzleRun<?, ?, ?> run : runs) {
            final var map = run.toTableRow();
            cmd.append(Arrays.stream(columns)
                    .map(col -> map.getOrDefault(col, "-"))
                    .collect(Collectors.joining(sep)) + "\n");
        }

        cmd.append(Arrays.stream(columns).map(col -> {
            switch (col) {
            case Name:
                return "Total";
            case Parsing:
                return formatSumOverRuns(runs, run -> {
                    if (run.puzzle().getInfo().hasInputFile()) {
                        return run.result().timing().get().parsing();
                    } else {
                        return 0L;
                    }
                });
            case Part1Time:
                return formatSumOverRuns(runs,
                        run -> run.result().timing().get().part1());
            case Part2Time:
                return formatSumOverRuns(runs,
                        run -> run.result().timing().get().part2());
            case TotalTime:
                return formatSumOverRuns(runs,
                        run -> run.result().timing().get().total());
            default:
                return "";
            }
        }).collect(Collectors.joining(sep)) + "\n");

        final var path = Files.createTempFile("tabulate", ".txt");
        try {
            Files.writeString(path, cmd.toString());
            final var pb = new ProcessBuilder("tabulate", "-f", "fancy_grid",
                    "-s", sep, path.toString());
            pb.inheritIO();
            final var p = pb.start();
            p.waitFor();
        } catch (final InterruptedException e) {
            e.printStackTrace();
        } finally {
            Files.delete(path);
        }
    }

    private static String formatSumOverRuns(List<AocPuzzleRun<?, ?, ?>> runs,
            Function<AocPuzzleRun<?, ?, ?>, Long> mapping) {
        return String.format("%.3f secs",
                sumOverRuns(runs, mapping) / 1000000000.0);
    }

    private static long sumOverRuns(List<AocPuzzleRun<?, ?, ?>> runs,
            Function<AocPuzzleRun<?, ?, ?>, Long> mapping) {
        return runs.stream().map(mapping)
                .collect(Collectors.summingLong(n -> n));
    }

    private static List<AocPuzzleRun<?, ?, ?>> runPuzzles(
            List<IAocPuzzle<?, ?, ?>> puzzles) throws IOException {
        final List<AocPuzzleRun<?, ?, ?>> runs = new ArrayList<>();
        for (final IAocPuzzle<?, ?, ?> puzzle : puzzles) {
            runs.add(run(puzzle));
        }
        return runs;
    }

    private static <T, P1, P2> AocPuzzleRun<T, P1, P2> run(
            IAocPuzzle<T, P1, P2> puzzle) throws IOException {
        final var info = puzzle.getInfo();
        File inputFile = null;
        AocResult<P1, P2> result;

        if (info.hasInputFile()) {

            inputFile = new File(String.format("inputs/%d/input%02d.txt",
                    info.year(), info.day()));
            if (!inputFile.exists()) {
                throw new FileNotFoundException(
                        "Input file is missing: " + inputFile);
            }

            result = runWithInput(Optional.of(inputFile), puzzle);
        } else {
            result = runWithInput(Optional.empty(), puzzle);
        }

        return new AocPuzzleRun<T, P1, P2>(puzzle, result);
    }

    record Timing<T> (long elapsed, T result) {

    }

    private static <T, P1, P2> AocResult<P1, P2> runWithInput(
            Optional<File> inputFile, IAocPuzzle<T, P1, P2> puzzle)
            throws IOException {
        final var info = puzzle.getInfo();
        System.out.format("%d day %d... ", info.year(), info.day());

        final Timing<T> parse = repeatWithTiming("parsing", puzzle::parse,
                inputFile);
        final var input = parse.result;
        final Timing<P1> part1 = repeatWithTiming("part 1", puzzle::part1,
                input);
        final Timing<P2> part2 = repeatWithTiming("part 2", puzzle::part2,
                input);

        System.out.println();

        long total = parse.elapsed + part1.elapsed + part2.elapsed;
        return AocResult.of(part1.result, part2.result, new AocTiming(
                parse.elapsed, part1.elapsed, part2.elapsed, total));
    }

    /**
     * Run a function with a given argument a number of times and return the
     * result (of the first invocation) + the average elapsed time per iteration
     * (in nanoseconds).
     * 
     * To get any sort of accuracy in performance measurements in Java, it is
     * necessary to let the JIT compiler warm up first.
     */
    private static <T, R> Timing<R> repeatWithTiming(String prefix,
            Function<T, R> fun, T arg) throws IOException {
        System.out.format("\t%s ", prefix);

        long repeatFor = 1000000000; // 1s
        long maxReps = 5000; // no need to repeat more than this many times

        final var t0 = System.nanoTime();
        R result = fun.apply(arg);
        int reps = 1;
        while (reps < maxReps) {
            fun.apply(arg);
            reps++;
            long t = System.nanoTime();
            long elapsed = t - t0;
            if (elapsed > repeatFor)
                break;
        }
        final var t1 = System.nanoTime();
        var totalElapsed = t1 - t0;
        var elapsedPerCall = totalElapsed / reps;
        System.out.format("(%.3f ms) (%d reps) ", elapsedPerCall / 1000000.0,
                reps);
        System.out.flush();
        return new Timing<R>(elapsedPerCall, result);
    }

}
