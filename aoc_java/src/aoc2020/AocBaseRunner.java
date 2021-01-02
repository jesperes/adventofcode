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
        System.out.println("Parsing...");
        T input = puzzle.parse(file);
        System.out.print("Running part 1... ");
        P1 p1 = puzzle.part1(input);
        System.out.println(
                expected.p1().get().equals(p1) ? String.format("OK (%s)", p1)
                        : String.format("FAILED (got %s, expected %s)", p1,
                                expected.p1().get()));
        System.out.print("Running part 2... ");
        P2 p2 = puzzle.part2(input);
        System.out.println(
                expected.p2().get().equals(p2) ? String.format("OK (%s)", p2)
                        : String.format("FAILED (got %s, expected %s)", p2,
                                expected.p2().get()));
    }
}
