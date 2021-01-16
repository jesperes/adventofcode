// Generated from Day18Part2.g4 by ANTLR 4.9
package aoc2020;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link Day18Part2Parser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface Day18Part2Visitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by the {@code parenGRP}
	 * labeled alternative in {@link Day18Part2Parser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParenGRP(Day18Part2Parser.ParenGRPContext ctx);
	/**
	 * Visit a parse tree produced by the {@code mulopGRP}
	 * labeled alternative in {@link Day18Part2Parser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMulopGRP(Day18Part2Parser.MulopGRPContext ctx);
	/**
	 * Visit a parse tree produced by the {@code addopGRP}
	 * labeled alternative in {@link Day18Part2Parser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAddopGRP(Day18Part2Parser.AddopGRPContext ctx);
	/**
	 * Visit a parse tree produced by the {@code literalGRP}
	 * labeled alternative in {@link Day18Part2Parser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLiteralGRP(Day18Part2Parser.LiteralGRPContext ctx);
}