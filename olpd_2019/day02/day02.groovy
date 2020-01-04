// Day 2: 1202 Program Alarm

def prog() {
    return new File("../inputs/input02.txt").getText()
        .split(",").collect { it -> Integer.valueOf(it.trim()) }
}

def run(List<Integer> ints, int a, int b) {
    ints[1] = a
    ints[2] = b

    def pc = 0
    while (true) {
        def op0 = ints[pc]

        if (op0 == 99)
            return ints[0]

        def op1 = ints[pc + 1]
        def op2 = ints[pc + 2]
        def op3 = ints[pc + 3]

        if (op0 == 1)
            ints[op3] = ints[op1] + ints[op2]
        else if (op0 == 2)
            ints[op3] = ints[op1] * ints[op2]
        else
            throw new Exception("Invalid op: ${op0}")

        pc += 4
    }
}

def part1()
{
    return run(prog(), 12, 2)
}

def part2()
{
    for (i = 0; i <= 99; i++) {
        for (j = 0; j <= 99; j++) {
            def p = prog()
            if (run(p, i, j) == 19690720) {
                return 100 * p[1] + p[2]
            }
        }
    }

    throw new Exception("Did not find solution")
}

assert part1() == 3654868
assert part2() == 7014
