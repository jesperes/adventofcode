package common2;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import com.google.common.io.Files;

public class InputUtils {

    final static int CAPACITY = 4096;

    /**
     * Return strings from a {@link BufferedReader} a list of longs.
     * 
     * @param reader
     * @return
     * @throws IOException
     */
    public static List<Long> asLongList(File file) {
        try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
            final List<Long> list = new ArrayList<>(CAPACITY);
            String line;
            while ((line = reader.readLine()) != null) {
                list.add(Long.valueOf(line));
            }
            return list;
        } catch (IOException e) {
            throw new RuntimeException("Failed to read input", e);
        }
    }

    public static List<Integer> asIntList(File file) {
        try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
            final List<Integer> list = new ArrayList<>(CAPACITY);
            String line;
            while ((line = reader.readLine()) != null) {
                list.add(Integer.valueOf(line));
            }
            return list;
        } catch (IOException e) {
            throw new RuntimeException("Failed to read input", e);
        }
    }

    public static List<String> asStringList(File file) {
        try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
            final List<String> list = new ArrayList<>(CAPACITY);
            String line;
            while ((line = reader.readLine()) != null) {
                list.add(line);
            }
            return list;
        } catch (IOException e) {
            throw new RuntimeException("Failed to read input", e);
        }
    }

    public static <T> List<T> asStringList(File file, Function<String, T> fun) {
        try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
            final List<T> list = new ArrayList<>(CAPACITY);
            String line;
            while ((line = reader.readLine()) != null) {
                list.add(fun.apply(line));
            }
            return list;
        } catch (IOException e) {
            throw new RuntimeException("Failed to read input", e);
        }
    }

    public static String asString(File file) {
        try {
            return new String(Files.toByteArray(file)).trim();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static byte[] asByteArray(File file) {
        try {
            return Files.toByteArray(file);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static <T> T withReader(File file, Function<BufferedReader, T> fun) {
        try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
            return fun.apply(reader);
        } catch (IOException e) {
            throw new RuntimeException("Failed to read input", e);
        }
    }

    /**
     * Parse test data from a string. For use in tests.
     * 
     * @param <T>
     * @param puzzle
     * @param data
     * @return
     */
    public static <T> T parseTestData(IAocPuzzle<T, ?, ?> puzzle, String data) {
        File f = null;
        try {
            f = File.createTempFile("tmp", ".txt");
            Files.write(data.getBytes(), f);
            return puzzle.parse(Optional.of(f));
        } catch (Exception e) {
            throw new RuntimeException(e);
        } finally {
            if (f != null) {
                f.delete();
                f.deleteOnExit();
            }
        }
    }
}
