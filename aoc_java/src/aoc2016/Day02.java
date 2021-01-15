package aoc2016;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;

import common2.AocBaseRunner;
import common2.AocPuzzleInfo;
import common2.AocResult;
import common2.IAocPuzzle;
import common2.InputUtils;

public class Day02 implements IAocPuzzle<List<String>, String, String> {

    record Coord(int x, int y) {
    }

    @Override
    public AocPuzzleInfo getInfo() {
        return new AocPuzzleInfo(2016, 2, "Bathroom Security", true);
    }

    @Override
    public AocResult<String, String> getExpected() {
        return AocResult.of("56983", "8B8B1");
    }

    @Override
    public List<String> parse(Optional<File> file) throws IOException {
        return InputUtils.asStringList(file.get());
    }

    @Override
    public String part1(List<String> input) {
        return applyInstrs(input, this::move1);
    }

    @Override
    public String part2(List<String> input) {
        return applyInstrs(input, this::move2);
    }

    public static String applyInstrs(List<String> input,
            BiFunction<Character, Character, Character> fun) {
        StringBuilder s = new StringBuilder();
        char digit = '5';
        for (String instrs : input) {
            for (char c : instrs.toCharArray()) {
                digit = fun.apply(digit, c);
            }
            s.append(digit);
        }
        return s.toString();
    }

    public static void main(String[] args) {
        AocBaseRunner.run(new Day02());
    }

    static void crash() {
        throw new RuntimeException();
    }

    private char move1(char digit, char instr) {
        if (digit == '1') {
            if (instr == 'R') {
                return '2';
            } else if (instr == 'D') {
                return '4';
            }
        } else if (digit == '2') {
            if (instr == 'U') {
                return '2';
            } else if (instr == 'L') {
                return '1';
            } else if (instr == 'R') {
                return '3';
            } else if (instr == 'D') {
                return '5';
            }
        } else if (digit == '3') {
            if (instr == 'L') {
                return '2';
            } else if (instr == 'D') {
                return '6';
            }
        } else if (digit == '4') {
            if (instr == 'U') {
                return '1';
            } else if (instr == 'L') {
                return '4';
            } else if (instr == 'R') {
                return '5';
            } else if (instr == 'D') {
                return '7';
            }
        } else if (digit == '5') {
            if (instr == 'U') {
                return '2';
            } else if (instr == 'L') {
                return '4';
            } else if (instr == 'R') {
                return '6';
            } else if (instr == 'D') {
                return '8';
            }
        } else if (digit == '6') {
            if (instr == 'U') {
                return '3';
            } else if (instr == 'L') {
                return '5';
            } else if (instr == 'R') {
                return '6';
            } else if (instr == 'D') {
                return '9';
            }
        } else if (digit == '7') {
            if (instr == 'U') {
                return '4';
            } else if (instr == 'R') {
                return '8';
            }
        } else if (digit == '8') {
            if (instr == 'U') {
                return '5';
            } else if (instr == 'L') {
                return '7';
            } else if (instr == 'R') {
                return '9';
            } else if (instr == 'D') {
                return '8';
            }
        } else if (digit == '9') {
            if (instr == 'U') {
                return '6';
            } else if (instr == 'L') {
                return '8';
            }
        }

        return digit;
    }

    private char move2(char digit, char instr) {
        if (digit == '1') {
            if (instr == 'D') {
                return '3';
            }
        } else if (digit == '2') {
            if (instr == 'R') {
                return '3';
            } else if (instr == 'D') {
                return '6';
            }
        } else if (digit == '3') {
            if (instr == 'U') {
                return '1';
            } else if (instr == 'L') {
                return '2';
            } else if (instr == 'R') {
                return '4';
            } else if (instr == 'D') {
                return '7';
            }
        } else if (digit == '4') {
            if (instr == 'L') {
                return '3';
            } else if (instr == 'D') {
                return '8';
            }
        } else if (digit == '5') {
            if (instr == 'R') {
                return '6';
            }
        } else if (digit == '6') {
            if (instr == 'U') {
                return '2';
            } else if (instr == 'L') {
                return '5';
            } else if (instr == 'R') {
                return '7';
            } else if (instr == 'D') {
                return 'A';
            }
        } else if (digit == '7') {
            if (instr == 'U') {
                return '3';
            } else if (instr == 'L') {
                return '6';
            } else if (instr == 'R') {
                return '8';
            } else if (instr == 'D') {
                return 'B';
            }
        } else if (digit == '8') {
            if (instr == 'U') {
                return '4';
            } else if (instr == 'L') {
                return '7';
            } else if (instr == 'R') {
                return '9';
            } else if (instr == 'D') {
                return 'C';
            }
        } else if (digit == '9') {
            if (instr == 'L') {
                return '8';
            }
        } else if (digit == 'A') {
            if (instr == 'U') {
                return '6';
            } else if (instr == 'R') {
                return 'B';
            }
        } else if (digit == 'B') {
            if (instr == 'U') {
                return '7';
            } else if (instr == 'L') {
                return 'A';
            } else if (instr == 'R') {
                return 'C';
            } else if (instr == 'D') {
                return 'D';
            }
        } else if (digit == 'C') {
            if (instr == 'U') {
                return '8';
            } else if (instr == 'L') {
                return 'B';
            }
        } else if (digit == 'D') {
            if (instr == 'U')
                return 'B';
        }

        return digit;
    }
}
