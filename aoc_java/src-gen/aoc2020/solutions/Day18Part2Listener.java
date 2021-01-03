// Generated from Day18Part2.g4 by ANTLR 4.9
package aoc2020.solutions;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link Day18Part2Parser}.
 */
public interface Day18Part2Listener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by the {@code parenGRP}
	 * labeled alternative in {@link Day18Part2Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterParenGRP(Day18Part2Parser.ParenGRPContext ctx);
	/**
	 * Exit a parse tree produced by the {@code parenGRP}
	 * labeled alternative in {@link Day18Part2Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitParenGRP(Day18Part2Parser.ParenGRPContext ctx);
	/**
	 * Enter a parse tree produced by the {@code mulopGRP}
	 * labeled alternative in {@link Day18Part2Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterMulopGRP(Day18Part2Parser.MulopGRPContext ctx);
	/**
	 * Exit a parse tree produced by the {@code mulopGRP}
	 * labeled alternative in {@link Day18Part2Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitMulopGRP(Day18Part2Parser.MulopGRPContext ctx);
	/**
	 * Enter a parse tree produced by the {@code addopGRP}
	 * labeled alternative in {@link Day18Part2Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterAddopGRP(Day18Part2Parser.AddopGRPContext ctx);
	/**
	 * Exit a parse tree produced by the {@code addopGRP}
	 * labeled alternative in {@link Day18Part2Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitAddopGRP(Day18Part2Parser.AddopGRPContext ctx);
	/**
	 * Enter a parse tree produced by the {@code literalGRP}
	 * labeled alternative in {@link Day18Part2Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterLiteralGRP(Day18Part2Parser.LiteralGRPContext ctx);
	/**
	 * Exit a parse tree produced by the {@code literalGRP}
	 * labeled alternative in {@link Day18Part2Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitLiteralGRP(Day18Part2Parser.LiteralGRPContext ctx);
}