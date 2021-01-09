package aoc2020;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;

import aoc2020.Day18Part1Parser.BinopGRPContext;
import aoc2020.Day18Part1Parser.LiteralGRPContext;
import aoc2020.Day18Part1Parser.ParenGRPContext;
import aoc2020.Day18Part2Parser.AddopGRPContext;
import aoc2020.Day18Part2Parser.MulopGRPContext;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocLongPuzzle;
import common2.InputUtils;

public class Day18 implements IAocLongPuzzle<List<String>> {

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2020, 18, "Operation Order", true);
    }

    @Override
    public AocResult<Long, Long> getExpected() {
        return AocResult.of(8298263963837L, 145575710203332L);
    }

    @Override
    public List<String> parse(Optional<File> file) {
        return InputUtils.asStringList(file.get());
    }

    @Override
    public Long part1(List<String> input) {
        return input.stream().collect(Collectors.summingLong(line -> {
            return new Day18Part1BaseVisitor<Long>() {
                @Override
                public Long visitParenGRP(ParenGRPContext ctx) {
                    return visit(ctx.expr());
                }

                @Override
                public Long visitLiteralGRP(LiteralGRPContext ctx) {
                    return Long.parseLong(ctx.getText());
                }

                @Override
                public Long visitBinopGRP(BinopGRPContext ctx) {
                    switch (ctx.binop.getText()) {
                    case "+":
                        return visit(ctx.expr(0)) + visit(ctx.expr(1));
                    case "*":
                        return visit(ctx.expr(0)) * visit(ctx.expr(1));
                    default:
                        throw new RuntimeException();
                    }
                }

            }.visit(new Day18Part1Parser(new CommonTokenStream(
                    new Day18Part1Lexer(CharStreams.fromString(line)))).expr());
        }));
    }

    @Override
    public Long part2(List<String> input) {
        return input.stream().collect(Collectors.summingLong(line -> {
            return new Day18Part2BaseVisitor<Long>() {
                @Override
                public Long visitParenGRP(
                        Day18Part2Parser.ParenGRPContext ctx) {
                    return visit(ctx.expr());
                }

                @Override
                public Long visitMulopGRP(MulopGRPContext ctx) {
                    return visit(ctx.expr(0)) * visit(ctx.expr(1));
                }

                @Override
                public Long visitAddopGRP(AddopGRPContext ctx) {
                    return visit(ctx.expr(0)) + visit(ctx.expr(1));
                }

                @Override
                public Long visitLiteralGRP(
                        Day18Part2Parser.LiteralGRPContext ctx) {
                    return Long.parseLong(ctx.getText());
                }
            }.visit(new Day18Part2Parser(new CommonTokenStream(
                    new Day18Part2Lexer(CharStreams.fromString(line)))).expr());
        }));
    }

    public static void main(String[] args) throws FileNotFoundException {
        AocBaseRunner.run(new Day18());
    }
}
