package aoc2020;

import java.io.File;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import com.google.common.base.Splitter;
import com.google.common.collect.Sets;

import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;
import common2.InputUtils;

public class Day06 implements IAocPuzzle<List<String>, Long, Long> {

    static Splitter inputSplitter = Splitter.on("\n\n");
    static Splitter groupSplitter = Splitter.on("\n");

    @Override
    public List<String> parse(Optional<File> file) {
        return inputSplitter
                .splitToList(InputUtils.asString(file.get()).trim());
    }

    @Override
    public Long part1(List<String> input) {
        return input.stream()
                .collect(Collectors.summingLong(s -> numYesAnswers(s)));
    }

    @Override
    public Long part2(List<String> input) {
        return input.stream()
                .collect(Collectors.summingLong(s -> allYesAnswers(s)));
    }

    private long numYesAnswers(String input) {
        return input.replace("\n", "").chars().distinct().count();
    }

    private long allYesAnswers(String input) {
        return StreamSupport
                .stream(groupSplitter.split(input).spliterator(), false)
                .map(s -> s.chars().boxed().collect(Collectors.toSet()))
                .reduce(Sets::intersection).get().size();
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(6680L, 3117L);
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 6, "Custom Customs", true);
    }
}
