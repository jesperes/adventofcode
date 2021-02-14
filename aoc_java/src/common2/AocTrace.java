package common2;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class AocTrace {

    static final boolean TRACE = false;

    static String filename(IAocPuzzle<?, ?, ?> puzzle) {
        return String.format("%d-%d-%s.trace", puzzle.getInfo().year(),
                puzzle.getInfo().day(), puzzle.getClass().getSimpleName());
    }

    public static void init(IAocPuzzle<?, ?, ?> puzzle) {
        if (TRACE) {
            File f = new File(filename(puzzle));
            f.delete();
        }
    }

    public static void trace(IAocPuzzle<?, ?, ?> puzzle, String fmt,
            Object... args) {
        if (TRACE) {
            try (BufferedWriter w = new BufferedWriter(
                    new FileWriter(filename(puzzle), true))) {
                w.write(String.format(fmt, args));
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
