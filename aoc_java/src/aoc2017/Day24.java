package aoc2017;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import aoc2017.Day24.Component;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;;

public class Day24 implements IAocIntPuzzle<List<List<Component>>> {

	static class Component {
		final int p_in;
		final int p_out;

		public Component(String str) {
			String[] elems = str.split("/");
			this.p_in = Integer.valueOf(elems[0]);
			this.p_out = Integer.valueOf(elems[1]);
		}

		public Component(int p_in, int p_out) {
			this.p_in = p_in;
			this.p_out = p_out;
		}

		/**
		 * Flip this component if necessary to match the given number of
		 * in-pins. Returns an empty optional if the component does not match.
		 */
		public Optional<Component> maybeFlip(int in) {
			if (p_in == in)
				return Optional.of(this);
			else if (p_out == in)
				return Optional.of(new Component(p_out, p_in));
			else
				return Optional.empty();
		}

		@Override
		public String toString() {
			return String.format("%d/%d", p_in, p_out);
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + p_in;
			result = prime * result + p_out;
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			Component other = (Component) obj;
			if (p_in != other.p_in)
				return false;
			if (p_out != other.p_out)
				return false;
			return true;
		}

	}

	static int strength(List<Component> bridge) {
		int s = bridge.stream().mapToInt(c -> c.p_in + c.p_out).sum();
		// System.out.format("Length %s == %d%n", bridge, s);
		return s;
	}

	@SafeVarargs
	static final <T> List<T> ListOf(T... elems) {
		List<T> list = new LinkedList<T>();
		for (T e : elems) {
			list.add(e);
		}
		return list;
	}

	/**
	 * Compute the strongest bridge which can be formed by taking 'first' and
	 * appending elements from 'components'.
	 *
	 * @param first
	 * @param components
	 * @return
	 */
	List<List<Component>> getAllBridges(int first, List<Component> components) {

		List<List<Component>> list = new ArrayList<>();

		// Find all components matching the digit we are to start with.
		for (Component comp : components) {
			comp.maybeFlip(first).ifPresent(match -> {

				/*
				 * There is always a sub-list containing just the matching
				 * elements (in case there are no more matching elements in the
				 * the rest of the list.
				 */
				list.add(ListOf(match));

				/*
				 * Construct all sub-bridges from the list of components minus
				 * the one we've already used up in this step.
				 */
				List<Component> sublist = new LinkedList<>();
				components.forEach(c -> {
					if (!c.equals(comp))
						sublist.add(c);
				});

				List<List<Component>> allSubBridges = getAllBridges(match.p_out,
						sublist);

				/*
				 * Pre-pend 'comp' (the component we have used in this step) to
				 * all of the output lists.
				 */
				for (List<Component> o : allSubBridges) {
					o.add(0, match); // 'match' is comp, but flipped to match
					list.add(o);
				}
			});
		}

		return list;
	}

	public List<Component> getStrongestBridge(
			List<List<Component>> allBridges) {

		int maxStrength = Integer.MIN_VALUE;
		List<Component> strongest = null;

		for (List<Component> l : allBridges) {
			int strength = strength(l);
			if (strength > maxStrength) {
				strongest = l;
				maxStrength = strength;
			}
		}

		return strongest;
	}

	public List<Component> getLongestBridge(List<List<Component>> allBridges) {

		int maxLength = Integer.MIN_VALUE;
		int currentStrength = 0; // strength of currently longest bridge
		List<Component> longest = null;

		for (List<Component> l : allBridges) {
			int length = l.size();
			if (length > maxLength) {
				longest = l;
				maxLength = length;
				currentStrength = strength(l);
			} else if (length == maxLength) {
				int s = strength(l);
				if (s > currentStrength) {
					longest = l;
					maxLength = length;
					currentStrength = s;
				}
			}
		}

		return longest;
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2017, 24, "Electromagnetic Moat", true);
	}

	@Override
	public AocResult<Integer, Integer> getExpected() {
		return AocResult.of(2006, 1994);
	}

	@Override
	public List<List<Component>> parse(Optional<File> file) throws IOException {
		return getAllBridges(0, InputUtils.asStringList(file.get()).stream()
				.map(Component::new).collect(Collectors.toList()));
	}

	@Override
	public Integer part1(List<List<Component>> input) {
		return strength(getStrongestBridge(input));
	}

	@Override
	public Integer part2(List<List<Component>> input) {
		return strength(getLongestBridge(input));
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day24());
	}
}
