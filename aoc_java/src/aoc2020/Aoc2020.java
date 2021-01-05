package aoc2020;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

import aoc2020.solutions.Day01;
import aoc2020.solutions.Day02;
import aoc2020.solutions.Day03;
import aoc2020.solutions.Day04;
import aoc2020.solutions.Day05;
import aoc2020.solutions.Day06;
import aoc2020.solutions.Day07;
import aoc2020.solutions.Day08;
import aoc2020.solutions.Day09;
import aoc2020.solutions.Day10;
import aoc2020.solutions.Day11;
import aoc2020.solutions.Day12;
import aoc2020.solutions.Day13;
import aoc2020.solutions.Day14;
import aoc2020.solutions.Day15;
import aoc2020.solutions.Day16;
import aoc2020.solutions.Day17;
import aoc2020.solutions.Day18;
import aoc2020.solutions.Day19;
import aoc2020.solutions.Day20;
import aoc2020.solutions.Day21;
import aoc2020.solutions.Day22;
import aoc2020.solutions.Day23;
import aoc2020.solutions.Day24;

public class Aoc2020 {

    public static void main(String[] args) throws IOException {
        final List<IAocPuzzle<?, ?, ?>> puzzles = List.of(new Day01(),
                new Day02(), new Day03(), new Day04(), new Day05(), new Day06(),
                new Day07(), new Day08(), new Day09(), new Day10(), new Day11(),
                new Day12(), new Day13(), new Day14(), new Day15(), new Day16(),
                new Day17(), new Day18(), new Day19(), new Day20(), new Day21(),
                new Day22(), new Day23(), new Day24());
        System.out.format("Running %d puzzles...", puzzles.size());
        final var runs = runPuzzles(puzzles);
        System.out.println();
        printTable(runs);
        writeResultsToFile(runs);
    }

    private static void writeResultsToFile(List<AocPuzzleRun<?, ?, ?>> runs) {

        var gson = new GsonBuilder().setPrettyPrinting().create();

        Properties properties = System.getProperties();
        JsonObject root = new JsonObject();
        root.addProperty("language", "java");
        root.addProperty("platform",
                properties.getProperty("java.runtime.name"));
        root.addProperty("version",
                properties.getProperty("java.runtime.version"));

        long parsetime = runs.stream()
                .map(run -> run.result().timing().get().parsing())
                .collect(Collectors.summingLong(n -> n));
        long part1time = runs.stream()
                .map(run -> run.result().timing().get().part1())
                .collect(Collectors.summingLong(n -> n));
        long part2time = runs.stream()
                .map(run -> run.result().timing().get().part2())
                .collect(Collectors.summingLong(n -> n));
        long grandTotal = parsetime + part1time + part2time;

        var totals = new JsonObject();
        totals.addProperty("parsing", parsetime);
        totals.addProperty("part1", part1time);
        totals.addProperty("part2", part2time);
        totals.addProperty("total", grandTotal);
        root.add("totals", totals);

        JsonArray array = new JsonArray();
        runs.stream().forEach(run -> {
            var obj = new JsonObject();

            IAocPuzzle<?, ?, ?> puzzle = run.puzzle();
            AocPuzzleInfo info = puzzle.getInfo();
            AocResult<?, ?> result = run.result();
            AocTiming timing = result.timing().get();
            AocResult<?, ?> expected = puzzle.getExpected();

            obj.addProperty("name", info.name());
            obj.addProperty("year", info.year());
            obj.addProperty("day", info.day());
            obj.addProperty("parsing", timing.parsing());
            obj.addProperty("total",
                    timing.parsing() + timing.part1() + timing.part2());

            var part1 = new JsonObject();
            part1.addProperty("time", timing.part1());
            part1.addProperty("result", result.p1().get().toString());
            part1.addProperty("status",
                    result.p1().get().equals(expected.p1().get()));
            obj.add("part1", part1);

            var part2 = new JsonObject();
            part2.addProperty("time", timing.part2());
            part2.addProperty("result", result.p2().get().toString());
            part2.addProperty("status",
                    result.p2().get().equals(expected.p2().get()));
            obj.add("part2", part2);

            array.add(obj);
        });

        root.add("runs", array);
        String string = gson.toJson(root);
        try {
            Path path = new File("results-java-2020.json").toPath();
            Files.writeString(path, string, Charset.defaultCharset());
            System.out.println("Wrote json to " + path);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
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
        System.out.print(".");

        Timing<T> parse = repeatWithTiming("parsing", puzzle::parse, inputFile);
        Timing<P1> part1 = repeatWithTiming("part 1", puzzle::part1,
                parse.result);
        Timing<P2> part2 = repeatWithTiming("part 2", puzzle::part2,
                parse.result);

        return AocResult.of(part1.result, part2.result,
                new AocTiming(parse.elapsed, part1.elapsed, part2.elapsed,
                        parse.elapsed + part1.elapsed + part2.elapsed));
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
        long repeatFor = 1_000_000_000; // 1s
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
        System.out.print(".");
        System.out.flush();
        return new Timing<R>(elapsedPerCall, result);
    }

}
