package aoc2020;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

public class InputUtils {

    final static int CAPACITY = 4096;

    /**
     * Return strings from a {@link BufferedReader} a list of longs.
     * 
     * @param reader
     * @return
     */
    public static List<Long> asLongList(BufferedReader reader) {
        final List<Long> list = new ArrayList<>(CAPACITY);
        String line;
        try {
            while ((line = reader.readLine()) != null) {
                list.add(Long.valueOf(line));
            }
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
        return list;
    }

    public static List<String> asStringList(BufferedReader reader) {
        final List<String> list = new ArrayList<>(CAPACITY);
        String line;
        try {
            while ((line = reader.readLine()) != null) {
                list.add(line);
            }
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
        return list;
    }

    public static <T> List<T> asStringList(BufferedReader reader,
            Function<String, T> fun) {
        final List<T> list = new ArrayList<>(CAPACITY);
        String line;
        try {
            while ((line = reader.readLine()) != null) {
                list.add(fun.apply(line));
            }
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
        return list;
    }

    public static Optional<BufferedReader> withReaderFromString(String s) {
        return Optional.of(new BufferedReader(new StringReader(s)));
    }

}
