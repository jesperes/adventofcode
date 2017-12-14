package day13;

import static org.junit.Assert.*;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Test;

import com.sun.xml.internal.bind.v2.TODO;

import utils.Utils;

public class Day13 {

	class Firewall {
		final int depth;
		final int range;
		int scannerPos = 0;
		int scannerDir = 1;

		public Firewall(String line) {
			String[] elems = line.split(":");
			this.depth = Integer.valueOf(elems[0].trim());
			this.range = Integer.valueOf(elems[1].trim());
		}

		public int getScannerPos() {
			return scannerPos;
		}

		public int getSeverity() {
			return depth * range;
		}

		public void moveScanner() {
			scannerPos += scannerDir;
			if (scannerPos == 0 && scannerDir == -1) {
				scannerDir = 1;
			} else if (scannerPos == range - 1) {
				scannerDir = -1;
			}
		}

		@Override
		public String toString() {
			return "Firewall [depth=" + depth + ", range=" + range
					+ ", scannerPos=" + scannerPos + ", scannerDir="
					+ scannerDir + "]";
		}
	}

	private void printFirewalls(List<Firewall> input, int currentDepth) {

		int maxDepth = input.stream().mapToInt(fw -> fw.depth).max().getAsInt()
				+ 1;
		int maxRange = input.stream().mapToInt(fw -> fw.range).max().getAsInt();

		for (int range = 0; range < maxRange; range++) {
			for (int depth = 0; depth < maxDepth; depth++) {

				int finalDepth = depth;
				Optional<Firewall> firewall = input.stream()
						.filter(fw -> fw.depth == finalDepth).findAny();

				// Is there a firewall at this depth?
				if (!firewall.isPresent()) {
					if (currentDepth == depth && range == 0)
						System.out.print("(.) ");
					else if (range == 0)
						System.out.print("... ");
					else 
						System.out.print("    ");
					continue;
				}

				// Are we out of range (below) this firewall?
				if (range >= firewall.get().range) {
					System.out.print("    ");
					continue;
				}

				char left;
				char right;
				if (currentDepth == depth && range == 0) {
					left = '(';
					right = ')';
				} else {
					left = '[';
					right = ']';
				}

				if (firewall.get().scannerPos == range) {
					System.out.format("%cS%c ", left, right);
				} else {
					System.out.format("%c %c ", left, right);
				}
			}

			System.out.println();
		}
	}

	private int computeSeverity(List<Firewall> input) {
		int totalSeverity = 0;
		int currentDepth = 0;
		int picosec = 0;

		int maxDepth = input.stream().mapToInt(fw -> fw.depth).max().getAsInt();
				
		for (int depth = 0; depth <= maxDepth; depth++) {
			int finalDepth = depth;
			Optional<Firewall> firewall = input.stream()
					.filter(fw -> fw.depth == finalDepth).findAny();
			
			System.out.println("Picosecond: " + picosec++);
			printFirewalls(input, depth);

			if (!firewall.isPresent())
				continue;
			
			... TODO ...
			
			// If the current firewall's scanner is at position 0 when we
			// move into it, we are caught.
			if (firewall.get().getScannerPos() == 0) {
				totalSeverity += fw.getSeverity();
			}

			// Then all the scanners move one step.
			for (Firewall fwScanner : input) {
				fwScanner.moveScanner();
			}
		}

		return totalSeverity;
	}

	@Test
	public void testShort() throws Exception {
		// @formatter:off
		Stream<String> puzzleInput = Stream.of(
				"0: 3", 
				"1: 2", 
				"4: 4", 
				"6: 4");
		// @formatter:on

		List<Firewall> input = puzzleInput.map(s -> new Firewall(s))
				.collect(Collectors.toList());

		assertEquals(24, computeSeverity(input));
	}

	@Test
	public void testFull() throws Exception {
		Stream<String> puzzleInput = Utils
				.readFileFromClassPathAsLines(getClass(), "day13/input.txt");
	}

}
