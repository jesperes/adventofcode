package common;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Base class for AoC puzzles, with some useful methods for reading input files.
 * 
 * @author jesperes
 *
 */
public abstract class AocPuzzle {
    private int year;
    private int day;

    public AocPuzzle(int year, int day) {
        this.year = year;
        this.day = day;
    }

    protected final File getInputFile() throws FileNotFoundException {
        File f = new File(String.format("inputs/%d/input%02d.txt", year, day));
        if (!f.exists()) {
            throw new FileNotFoundException(
                    "No input file for this puzzle: " + f);
        } else {
            return f;
        }
    }

    protected final BufferedReader getInputAsReader()
            throws FileNotFoundException {
        return new BufferedReader(new FileReader(getInputFile()));
    }

    protected final String getInputAsString() throws IOException {
        try (BufferedReader r = new BufferedReader(
                new FileReader(getInputFile()))) {
            return r.lines().collect(Collectors.joining("\n"));
        }
    }

    protected final char[] getInputAsCharArray() throws IOException {
        return getInputAsString().toCharArray();
    }

    protected final List<String> getInputAsLines() throws IOException {
        try (BufferedReader r = new BufferedReader(
                new FileReader(getInputFile()))) {
            return r.lines().collect(Collectors.toList());
        }
    }

    protected final Stream<String> getInputAsStream() throws IOException {
        return getInputAsReader().lines();
    }

}
