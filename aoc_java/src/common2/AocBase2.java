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
import java.util.Locale;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.google.common.collect.ComparisonChain;
import com.google.common.reflect.ClassPath;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

public class AocBase2 {

    private static boolean parallel = true;
    private static int WORKERS = 1;
    private static final int MAX_REPEATS = 1000;
    private static final int MAX_REPEAT_NS = 500_000_000;

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
        if (parallel)
            System.out.println("Multi-threaded.");
        else
            System.out.println("Single-threaded.");

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

        System.out.println("\nPuzzle stats:");

        for (var p : puzzles) {
            p.dumpStats();
        }

        System.out.println("\nSummaries per year:");
        var yearStats = runs.stream()
                .mapToInt(run -> run.puzzle().getInfo().year())
                .summaryStatistics();
        var firstYear = yearStats.getMin();
        var lastYear = yearStats.getMax();
        for (int year = firstYear; year <= lastYear; year++) {
            printYearStats(runs, year);
        }
    }

    private void printYearStats(List<AocPuzzleRun<?, ?, ?>> runs, int year) {
        List<AocPuzzleInfo> implemented = new ArrayList<>();
        List<AocPuzzleInfo> partial = new ArrayList<>();
        List<AocPuzzleInfo> complete = new ArrayList<>();

        for (var run : runs) {
            var puzzle = run.puzzle();
            var info = puzzle.getInfo();
            if (info.year() != year)
                continue;

            implemented.add(info);

            var day = info.day();
            var result = run.result();
            var expected = puzzle.getExpected();

            var p1_ok = expected.p1().get().equals(result.p1().get());
            if (day == 25) {
                if (p1_ok)
                    complete.add(info);
                else
                    partial.add(info);
            } else {
                var p2_ok = expected.p2().get().equals(result.p2().get());
                if (p1_ok && p2_ok) {
                    complete.add(info);
                } else {
                    partial.add(info);
                }
            }
        }

        if (complete.size() == 25) {
            System.out.println("All puzzle solutions for %d are complete!"
                    .formatted(year));
        } else {
            System.out.println(
                    "Puzzle solution status for %d: %d/%d/%d (implemented/partial/completed)"
                            .formatted(year, implemented.size(), partial.size(),
                                    complete.size()));
        }
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
            Path path = new File("results-java.json").toPath();
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

        Collections.sort(runs, new Comparator<AocPuzzleRun<?, ?, ?>>() {
            @Override
            public int compare(AocPuzzleRun<?, ?, ?> o1,
                    AocPuzzleRun<?, ?, ?> o2) {
                Long a = o1.result().timing().get().total();
                Long b = o2.result().timing().get().total();
                return a.compareTo(b);
            }
        });

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

        /*
         * Produces a nice table of the results using the "tabulate" python
         * package. Install using "sudo apt install python3-tabulate".
         */
        var csv = Path.of("table.csv");
        Files.write(csv, cmd.toString().getBytes());
        try {
            var pb = new ProcessBuilder();

            if (System.getProperty("os.name").toLowerCase().contains("win")) {
                // If windows, use "tabulate" from WSL.
                pb.command().add("wsl");
            }

            pb.command().addAll(List.of("tabulate", "-s", sep, csv.toString()));
            pb.inheritIO();
            var p = pb.start();
            p.waitFor();
        } catch (InterruptedException e) {
            e.printStackTrace();
        } catch (IOException e) {
            System.out.println("Failed to tabulate results: " + e);
        }
    }

    private String formatSumOverRuns(List<AocPuzzleRun<?, ?, ?>> runs,
            Function<AocPuzzleRun<?, ?, ?>, Long> mapping) {
        return String.format(Locale.ROOT, "%.3f",
                sumOverRuns(runs, mapping) / 1000000.0);
    }

    private long sumOverRuns(List<AocPuzzleRun<?, ?, ?>> runs,
            Function<AocPuzzleRun<?, ?, ?>, Long> mapping) {
        return runs.stream().map(mapping)
                .collect(Collectors.summingLong(n -> n));
    }

    private List<AocPuzzleRun<?, ?, ?>> runPuzzlesParallel(
            List<IAocPuzzle<?, ?, ?>> puzzles) throws IOException {
        final List<AocPuzzleRun<?, ?, ?>> runs = new ArrayList<>();
        ExecutorService exec = Executors.newFixedThreadPool(WORKERS);
        for (final IAocPuzzle<?, ?, ?> puzzle : puzzles) {
            exec.execute(() -> {
                try {
                    Thread.currentThread().setName(puzzle.getInfo().name());
                    runs.add(run(puzzle));
                    System.out.print(".");
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            });
        }
        exec.shutdown();
        try {
            if (!exec.awaitTermination(60, TimeUnit.MINUTES)) {
                throw new RuntimeException("timeout");
            }
        } catch (InterruptedException e) {
            throw new RuntimeException();
        }
        return runs;
    }

    private List<AocPuzzleRun<?, ?, ?>> runPuzzlesSingle(
            List<IAocPuzzle<?, ?, ?>> puzzles) throws IOException {
        List<AocPuzzleRun<?, ?, ?>> runs = new ArrayList<>();
        for (final IAocPuzzle<?, ?, ?> puzzle : puzzles) {
            runs.add(run(puzzle));
            System.out.print(".");
        }
        return runs;
    }

    private List<AocPuzzleRun<?, ?, ?>> runPuzzles(
            List<IAocPuzzle<?, ?, ?>> puzzles) throws IOException {
        long t0 = System.nanoTime();

        var runs = parallel ? runPuzzlesParallel(puzzles)
                : runPuzzlesSingle(puzzles);

        long t1 = System.nanoTime();

        double elapsedSecs = (t1 - t0) / 1000000000.0;

        System.out.println();
        System.out.format(Locale.ROOT, "%nTotal wall time: %.3f seconds%n",
                elapsedSecs);
        System.out.format(Locale.ROOT, "Wall time per puzzle: %.3f seconds%n",
                elapsedSecs / puzzles.size());

        double puzzleTotalTimeSecs = runs.stream()
                .mapToLong(run -> run.result().timing().get().total()).sum()
                / 1000000000.0;

        if (parallel)
            System.out.format(Locale.ROOT, "Speedup with parallel runs: %.3f%n",
                    puzzleTotalTimeSecs / elapsedSecs);

        return runs;
    }

    private <T, P1, P2> AocPuzzleRun<T, P1, P2> run(
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

    private <T, P1, P2> AocResult<P1, P2> runWithInput(Optional<File> inputFile,
            IAocPuzzle<T, P1, P2> puzzle) throws IOException {
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
        return new Timing<R>(elapsedPerCall, result);
    }
}
