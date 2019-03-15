package aoc2015;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import org.junit.Test;

public class Day08 {

    int memoryChars(String line) {
        int chars = 0;
        for (int i = 1; i < line.length() - 1;) {
            String substr = line.substring(i);
            if (substr.startsWith("\\x")) {
                chars++;
                i += 4;
            } else if (substr.startsWith("\\")) {
                i += 2;
                chars++;
            } else {
                i++;
                chars++;
            }
        }
        return chars;
    }

    int literalChars(String line) {
        int chars = 2;
        for (int i = 0; i < line.length(); i++) {
            switch (line.charAt(i)) {
            case '\\':
            case '"':
                chars += 2;
                break;
            default:
                chars++;
                break;
            }
        }
        return chars;
    }

    @Test
    public void testDay08() throws IOException {
        try (BufferedReader reader = new BufferedReader(
                new FileReader("inputs/2015/day08.txt"))) {

            String line;
            int memChars = 0;
            int litChars = 0;
            int totalChars = 0;

            while ((line = reader.readLine()) != null) {
                totalChars += line.length();
                memChars += memoryChars(line);
                litChars += literalChars(line);
            }

            assertEquals(1371, totalChars - memChars);
            assertEquals(2117, litChars - totalChars);
        }
    }
}
