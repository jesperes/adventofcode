package aoc2016;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import aoc2016.Day10.Instr;
import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocIntPuzzle;
import common2.InputUtils;

public class Day10 implements IAocIntPuzzle<List<Instr>> {

	enum Tag {
		bot, value, output;
	}

	record TagVal(Tag type, int num) {
		static TagVal of(Matcher m, String group) {
			String str = m.group(group);
			String[] s = str.split(" ");
			return new TagVal(Tag.valueOf(s[0]), Integer.parseInt(s[1]));
		}

		Receiver makeReceiver() {
			if (type == Tag.bot) {
				return new Bot(num);
			} else {
				return new Output(num);
			}
		}
	}

	interface Instr {
	}

	record AssignInstr(TagVal value, TagVal to) implements Instr {
	}

	record LowHighInstr(TagVal bot, TagVal low, TagVal high) implements Instr {
	}

	interface Receiver {
		void give(int n);
	}

	static class Bot implements Receiver {
		List<Integer> values = new ArrayList<>();
		int num;
		Receiver low;
		Receiver high;

		Bot(int num) {
			this.num = num;
		}

		void setLow(Receiver bot) {
			low = bot;
		}

		void setHigh(Receiver bot) {
			high = bot;
		}

		public void give(int n) {
			assertTrue(values.size() < 2);
			values.add(n);
			if (values.size() == 2) {
				Collections.sort(values);
				low.give(values.get(0));
				high.give(values.get(1));
			}
		}
	}

	static class Output implements Receiver {
		int value = -1;
		int num;

		Output(int num) {
			this.num = num;
		}

		@Override
		public void give(int n) {
			this.value = n;
		}
	}

	Pattern assignPat = Pattern.compile("(?<value>.*) goes to (?<bot>.*)");
	Pattern lowHighPat = Pattern.compile("(?<bot>.*) gives low to (?<low>.*) and high to (?<high>.*)");

	private Instr parseInstr(String line) {
		if (line.contains("value")) {
			var m = assignPat.matcher(line);
			assertTrue(m.matches());
			return new AssignInstr(TagVal.of(m, "value"), TagVal.of(m, "bot"));
		} else {
			var m = lowHighPat.matcher(line);
			assertTrue(m.matches());
			return new LowHighInstr(TagVal.of(m, "bot"), TagVal.of(m, "low"), TagVal.of(m, "high"));
		}
	}

	@Override
	public AocPuzzleInfo getInfo() {
		return new AocPuzzleInfo(2016, 10, "Balance Bots", true);
	}

	@Override
	public AocResult<Integer, Integer> getExpected() {
		return AocResult.of(161, 133163);
	}

	@Override
	public List<Instr> parse(Optional<File> file) throws IOException {
		return InputUtils.asStringList(file.get()).stream().map(this::parseInstr)
				.collect(Collectors.toUnmodifiableList());
	}

	int part2 = 0;
	int part1 = 0;

	@Override
	public Integer part1(List<Instr> input) {
		Map<TagVal, Receiver> receivers = new HashMap<>();

		for (Instr instr : input) {
			if (instr instanceof LowHighInstr lowhigh) {
				receivers.put(lowhigh.bot, lowhigh.bot.makeReceiver());
				receivers.put(lowhigh.low, lowhigh.low.makeReceiver());
				receivers.put(lowhigh.high, lowhigh.high.makeReceiver());
			}
		}

		for (Instr instr : input) {
			if (instr instanceof LowHighInstr lowhigh) {
				Receiver rec = receivers.get(lowhigh.bot);
				if (rec instanceof Bot bot) {
					bot.setLow(receivers.get(lowhigh.low));
					bot.setHigh(receivers.get(lowhigh.high));
				}
			}
		}

		for (Instr instr : input) {
			if (instr instanceof AssignInstr assignInstr) {
				var value = assignInstr.value.num;
				var receiver = receivers.get(assignInstr.to);
				receiver.give(value);
			}
		}

		for (Receiver rec : receivers.values()) {
			if (rec instanceof Bot bot) {
				if (bot.values.get(0) == 17 && bot.values.get(1) == 61) {
					part1 = bot.num;
					break;
				}
			}
		}

		Output output1 = (Output) receivers.get(new TagVal(Tag.output, 0));
		Output output2 = (Output) receivers.get(new TagVal(Tag.output, 1));
		Output output3 = (Output) receivers.get(new TagVal(Tag.output, 2));

		part2 = output1.value * output2.value * output3.value;
		return part1;
	}

	@Override
	public Integer part2(List<Instr> input) {
		return part2;
	}

	public static void main(String[] args) {
		AocBaseRunner.run(new Day10());
	}
}
