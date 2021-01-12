package common2;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.google.common.collect.ComparisonChain;
import com.google.common.reflect.ClassPath;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

public class AocBase2 {

    private static final int MAX_REPEATS = 1000;
    private static final int MAX_REPEAT_NS = 1_000_000_000;

    private List<IAocPuzzle<?, ?, ?>> puzzles;

    public AocBase2(Pattern p) throws Exception {
        puzzles = findPuzzles(p);
    }

    public AocBase2(String re) throws Exception {
        puzzles = findPuzzles(Pattern.compile(re));
    }

    public static void main(String[] args) throws Exception {
        Pattern p;
        if (args.length > 0) {
            p = Pattern.compile(args[0]);
        } else {
            p = Pattern.compile(".*aoc.*\\.Day.*");
        }

        new AocBase2(p).run();
    }

    private List<IAocPuzzle<?, ?, ?>> findPuzzles(Pattern p) throws Exception {
        List<IAocPuzzle<?, ?, ?>> impls = new ArrayList<>();

        var cp = ClassPath.from(AocBase2.class.getClassLoader());
        for (var classInfo : cp.getAllClasses()) {
            var name = classInfo.getName();
            if (p.matcher(name).matches()) {
                var cls = classInfo.load();

                if (IAocPuzzle.class.isAssignableFrom(cls)) {
                    Constructor<?> ctor = cls.getConstructor();
                    impls.add((IAocPuzzle<?, ?, ?>) ctor.newInstance());
                }
            }
        }

        Collections.sort(impls, new Comparator<IAocPuzzle<?, ?, ?>>() {
            @Override
            public int compare(IAocPuzzle<?, ?, ?> o1, IAocPuzzle<?, ?, ?> o2) {
                return ComparisonChain.start()
                        .compare(o1.getInfo().year(), o2.getInfo().year())
                        .compare(o1.getInfo().day(), o2.getInfo().day())
                        .result();
            }
        });

        return impls;
    }

    public void run() throws IOException {

        System.out.format("Running %d puzzles...", puzzles.size());
        final var runs = runPuzzles(puzzles);
        System.out.println();
        var properties = System.getProperties();
        System.out.println();
        System.out.println("Language: java");
        System.out.format("System version: %s (%s)",
                properties.getProperty("java.runtime.name"),
                properties.getProperty("java.runtime.version"));
        System.out.println();
        printTable(runs);
        writeResultsToFile(runs);
    }

    private void writeResultsToFile(List<AocPuzzleRun<?, ?, ?>> runs) {

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

            if (info.day() != 25) {
                var part2 = new JsonObject();
                part2.addProperty("time", timing.part2());
                part2.addProperty("result", result.p2().get().toString());
                part2.addProperty("status",
                        result.p2().get().equals(expected.p2().get()));
                obj.add("part2", part2);
            }
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

    private void printTable(List<AocPuzzleRun<?, ?, ?>> runs)
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

        cmd.append(Arrays.stream(columns).map(col -> {
            switch (col) {
            case Name:
                return "Per puzzle";
            case Parsing:
                return formatSumOverRuns(runs, run -> {
                    if (run.puzzle().getInfo().hasInputFile()) {
                        return run.result().timing().get().parsing()
                                / runs.size();
                    } else {
                        return 0L;
                    }
                });
            case Part1Time:
                return formatSumOverRuns(runs,
                        run -> run.result().timing().get().part1()
                                / runs.size());
            case Part2Time:
                return formatSumOverRuns(runs,
                        run -> run.result().timing().get().part2()
                                / runs.size());
            case TotalTime:
                return formatSumOverRuns(runs,
                        run -> run.result().timing().get().total()
                                / runs.size());
            default:
                return "";
            }
        }).collect(Collectors.joining(sep)) + "\n");

        final var path = Files.createTempFile("tabulate", ".txt");
        try {
            Files.writeString(path, cmd.toString());
            final var pb = new ProcessBuilder("tabulate", "-f", "simple", "-s",
                    sep, path.toString());
            pb.inheritIO();
            final var p = pb.start();
            p.waitFor();
        } catch (final InterruptedException e) {
            e.printStackTrace();
        } finally {
            Files.delete(path);
        }

        Files.write(Path.of("table.csv"), cmd.toString().getBytes());
    }

    private String formatSumOverRuns(List<AocPuzzleRun<?, ?, ?>> runs,
            Function<AocPuzzleRun<?, ?, ?>, Long> mapping) {
        return String.format("%.3f", sumOverRuns(runs, mapping) / 1000000.0);
    }

    private long sumOverRuns(List<AocPuzzleRun<?, ?, ?>> runs,
            Function<AocPuzzleRun<?, ?, ?>, Long> mapping) {
        return runs.stream().map(mapping)
                .collect(Collectors.summingLong(n -> n));
    }

    private List<AocPuzzleRun<?, ?, ?>> runPuzzles(
            List<IAocPuzzle<?, ?, ?>> puzzles) throws IOException {
        final List<AocPuzzleRun<?, ?, ?>> runs = new ArrayList<>();
        for (final IAocPuzzle<?, ?, ?> puzzle : puzzles) {
            runs.add(run(puzzle));
        }
        return runs;
    }

    private <T, P1, P2> AocPuzzleRun<T, P1, P2> run(
            IAocPuzzle<T, P1, P2> puzzle) throws IOException {
        final var info = puzzle.getInfo();
        System.out.format("[%d]", info.day());
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

    private <T, P1, P2> AocResult<P1, P2> runWithInput(Optional<File> inputFile,
            IAocPuzzle<T, P1, P2> puzzle) throws IOException {
        System.out.print(".");

        Timing<T> parse = repeatWithTiming("parsing", f -> {
            try {
                return puzzle.parse(f);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }, inputFile);
        Timing<P1> part1 = repeatWithTiming("part 1", puzzle::part1,
                parse.result);
        if (puzzle.getInfo().day() != 25) {
            Timing<P2> part2 = repeatWithTiming("part 2", puzzle::part2,
                    parse.result);
            return AocResult.of(part1.result, part2.result,
                    new AocTiming(parse.elapsed, part1.elapsed, part2.elapsed,
                            parse.elapsed + part1.elapsed + part2.elapsed));
        } else {
            return AocResult.of(part1.result, null, new AocTiming(parse.elapsed,
                    part1.elapsed, 0, parse.elapsed + part1.elapsed));
        }
    }

    /**
     * Run a function with a given argument a number of times and return the
     * result (of the first invocation) + the average elapsed time per iteration
     * (in nanoseconds).
     * 
     * To get any sort of accuracy in performance measurements in Java, it is
     * necessary to let the JIT compiler warm up first.
     */
    private <T, R> Timing<R> repeatWithTiming(String prefix, Function<T, R> fun,
            T arg) throws IOException {
        final var t0 = System.nanoTime();
        R result = fun.apply(arg);
        int reps = 1;
        while (reps < MAX_REPEATS) {
            fun.apply(arg);
            reps++;
            long t = System.nanoTime();
            long elapsed = t - t0;
            if (elapsed > MAX_REPEAT_NS)
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
