package aoc2020;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Optional;

/**
 * Class for running individual puzzles.
 * 
 * @author jesperes
 *
 */
public class AocBaseRunner {
    public static void run(IAocPuzzle<?, ?, ?> puzzle)
            throws FileNotFoundException {
        AocPuzzleInfo info = puzzle.getInfo();
        if (info.hasInputFile()) {
            File inputFile = new File(String.format("inputs/%d/input%02d.txt",
                    info.year(), info.day()));
            if (!inputFile.exists()) {
                throw new FileNotFoundException(
                        "Input file is missing: " + inputFile);
            }

            runWithInput(Optional.of(inputFile), puzzle);
        } else {
            runWithInput(Optional.empty(), puzzle);
        }

    }

    private static <T, P1, P2> void runWithInput(Optional<File> file,
            IAocPuzzle<T, P1, P2> puzzle) {
        AocResult<P1, P2> expected = puzzle.getExpected();
        System.out.print("Parsing... ");
        System.out.flush();
        long t0 = System.nanoTime();
        T input = puzzle.parse(file);
        long t1 = System.nanoTime();
        System.out.format("(%.3f ms)%n", (t1 - t0) / 1_000_000.0);

        System.out.print("Running part 1... ");
        long t2 = System.nanoTime();
        P1 p1 = puzzle.part1(input);
        long t3 = System.nanoTime();
        System.out.format("(%.3f ms) ", (t3 - t2) / 1_000_000.0);
        System.out.println(
                expected.p1().get().equals(p1) ? String.format("OK (%s)", p1)
                        : String.format("FAILED (got %s, expected %s)", p1,
                                expected.p1().get()));

        System.out.print("Running part 2... ");
        long t4 = System.nanoTime();
        P2 p2 = puzzle.part2(input);
        long t5 = System.nanoTime();
        System.out.format("(%.3f ms) ", (t5 - t4) / 1_000_000.0);
        System.out.println(
                expected.p2().get().equals(p2) ? String.format("OK (%s)", p2)
                        : String.format("FAILED (got %s, expected %s)", p2,
                                expected.p2().get()));
    }
}
