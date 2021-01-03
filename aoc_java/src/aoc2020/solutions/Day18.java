package aoc2020.solutions;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;

import aoc2020.AocBaseRunner;
import aoc2020.AocPuzzleInfo;
import aoc2020.AocResult;
import aoc2020.IAocIntPuzzle;
import aoc2020.InputUtils;
import aoc2020.solutions.Day18Part1Parser.BinopGRPContext;
import aoc2020.solutions.Day18Part1Parser.LiteralGRPContext;
import aoc2020.solutions.Day18Part1Parser.ParenGRPContext;
import aoc2020.solutions.Day18Part2Parser.AddopGRPContext;
import aoc2020.solutions.Day18Part2Parser.MulopGRPContext;

public class Day18 implements IAocIntPuzzle<List<String>> {

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
            Day18Part1Lexer lexer = new Day18Part1Lexer(
                    CharStreams.fromString(line));
            CommonTokenStream tokenStream = new CommonTokenStream(lexer);
            Day18Part1Parser parser = new Day18Part1Parser(tokenStream);
            ParseTree start = parser.expr();

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
                    String op = ctx.binop.getText();
                    long a = visit(ctx.expr(0));
                    long b = visit(ctx.expr(1));
                    switch (op) {
                    case "+":
                        return a + b;
                    case "*":
                        return a * b;
                    default:
                        throw new RuntimeException();
                    }
                }

            }.visit(start);
        }));
    }

    @Override
    public Long part2(List<String> input) {
        return input.stream().collect(Collectors.summingLong(line -> {
            Day18Part2Lexer lexer = new Day18Part2Lexer(
                    CharStreams.fromString(line));
            CommonTokenStream tokenStream = new CommonTokenStream(lexer);
            Day18Part2Parser parser = new Day18Part2Parser(tokenStream);
            ParseTree start = parser.expr();

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
            }.visit(start);
        }));
    }

    public static void main(String[] args) throws FileNotFoundException {
        AocBaseRunner.run(new Day18());
    }
}
