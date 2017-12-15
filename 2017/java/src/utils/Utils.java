package utils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Utils {
    public static Stream<String> readFileFromClassPathAsLines(Class<?> cls,
            String name) throws IOException {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(
                cls.getClassLoader().getResourceAsStream(name)))) {
            // We cannot just return lines() here, since that will cause the
            // stream to be closed before we can read anything. Instead, we
            // but force creating a list, then make a stream of the list.
            return reader.lines().collect(Collectors.toList()).stream();
        }
    }

    public static String readFileFromClassPathAsString(Class<?> cls,
            String name) throws IOException {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(
                cls.getClassLoader().getResourceAsStream(name)))) {
            return reader.lines().collect(Collectors.joining("\n"));
        }
    }
}
