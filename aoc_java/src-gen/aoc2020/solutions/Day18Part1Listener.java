// Generated from Day18Part1.g4 by ANTLR 4.9
package aoc2020.solutions;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link Day18Part1Parser}.
 */
public interface Day18Part1Listener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by the {@code parenGRP}
	 * labeled alternative in {@link Day18Part1Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterParenGRP(Day18Part1Parser.ParenGRPContext ctx);
	/**
	 * Exit a parse tree produced by the {@code parenGRP}
	 * labeled alternative in {@link Day18Part1Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitParenGRP(Day18Part1Parser.ParenGRPContext ctx);
	/**
	 * Enter a parse tree produced by the {@code literalGRP}
	 * labeled alternative in {@link Day18Part1Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterLiteralGRP(Day18Part1Parser.LiteralGRPContext ctx);
	/**
	 * Exit a parse tree produced by the {@code literalGRP}
	 * labeled alternative in {@link Day18Part1Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitLiteralGRP(Day18Part1Parser.LiteralGRPContext ctx);
	/**
	 * Enter a parse tree produced by the {@code binopGRP}
	 * labeled alternative in {@link Day18Part1Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterBinopGRP(Day18Part1Parser.BinopGRPContext ctx);
	/**
	 * Exit a parse tree produced by the {@code binopGRP}
	 * labeled alternative in {@link Day18Part1Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitBinopGRP(Day18Part1Parser.BinopGRPContext ctx);
}