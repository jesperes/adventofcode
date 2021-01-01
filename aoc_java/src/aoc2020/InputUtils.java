package aoc2020;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

public class InputUtils {

    final static int CAPACITY = 4096;

    /**
     * Return strings from a {@link BufferedReader} a list of longs.
     * 
     * @param reader
     * @return
     * @throws IOException
     */
    public static List<Long> asLongList(InputStream stream) {
        try (BufferedReader reader = new BufferedReader(
                new InputStreamReader(stream))) {
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

    public static List<String> asStringList(InputStream stream) {
        try (BufferedReader reader = new BufferedReader(
                new InputStreamReader(stream))) {
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

    public static <T> List<T> asStringList(InputStream stream,
            Function<String, T> fun) {
        try (BufferedReader reader = new BufferedReader(
                new InputStreamReader(stream))) {
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

    public static String asString(InputStream stream) {
        final int bufferSize = 1024;
        final char[] buffer = new char[bufferSize];
        final StringBuilder out = new StringBuilder();
        Reader in = new InputStreamReader(stream, StandardCharsets.UTF_8);
        int charsRead;
        try {
            while ((charsRead = in.read(buffer, 0, buffer.length)) > 0) {
                out.append(buffer, 0, charsRead);
            }
        } catch (IOException e) {
            throw new RuntimeException("Could not read input", e);
        }
        return out.toString();
    }

}
