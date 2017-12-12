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
            return reader.lines();
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
