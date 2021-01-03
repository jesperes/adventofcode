// Generated from Day18Part1.g4 by ANTLR 4.9
package aoc2020.solutions;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link Day18Part1Parser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface Day18Part1Visitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by the {@code parenGRP}
	 * labeled alternative in {@link Day18Part1Parser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParenGRP(Day18Part1Parser.ParenGRPContext ctx);
	/**
	 * Visit a parse tree produced by the {@code literalGRP}
	 * labeled alternative in {@link Day18Part1Parser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLiteralGRP(Day18Part1Parser.LiteralGRPContext ctx);
	/**
	 * Visit a parse tree produced by the {@code binopGRP}
	 * labeled alternative in {@link Day18Part1Parser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBinopGRP(Day18Part1Parser.BinopGRPContext ctx);
}