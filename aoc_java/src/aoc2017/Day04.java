package aoc2017;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day04 implements IAocIntPuzzle<List<String>> {

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2017, 4, "High-Entropy Passphrases", true);
    }

    @Override
    public AocResult<Integer, Integer> getExpected() {
        return AocResult.of(466, 251);
    }

    @Override
    public List<String> parse(Optional<File> file) throws IOException {
        return InputUtils.asStringList(file.get());
    }

    @Override
    public Integer part1(List<String> input) {
        return (int) input.stream().filter(this::isValid).count();
    }

    @Override
    public Integer part2(List<String> input) {
        return (int) input.stream().filter(this::isValidPart2).count();
    }

    boolean isValid(String passphrase) {
        Set<String> words = new HashSet<>();
        for (String word : passphrase.split(" ")) {
            if (words.contains(word)) {
                return false;
            } else {
                words.add(word);
            }
        }
        return true;
    }

    String createSignature(String word) {
        return Arrays.stream(word.split("")).sorted()
                .collect(Collectors.joining());
    }

    boolean isValidPart2(String passphrase) {
        Set<String> signatures = new HashSet<>();
        for (String word : passphrase.split(" ")) {
            String sign = createSignature(word);
            if (signatures.contains(sign)) {
                return false;
            } else {
                signatures.add(sign);
            }
        }

        return true;
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day04());
    }
}
