import time
from block_timer.timer import Timer

class AocPuzzle:

    def __init__(self):
        self.started = False
        self.completed = False
        self.success = False
        self.has_parsed_input = False
        self.has_separate_parts = False
        self.p1_time = None
        self.p2_time = None
        self.p_both_time = None
        self.parse_time = None
        self.total_time = None
        self.result = None
        self.expected_result = None
        (self.year, self.day) = self.id()

    def has_method(self, name):
        hasattr(self, name) and callable(getattr(self, name))

    def id(self):
        return None

    def parse(self, io):
        return None

    def solve(self, data):
        return None

    def solve1(self, data):
        return None

    def solve2(self, data):
        return None

    def expected(self):
        return None

    def with_time(self):
        t0 = time.process_time()
        yield
        t1 = time.process_time()
        return t1 - t0

    def __str__(self):
        return ""

    def result_is_ok(self):
        return self.expected_result == self.result

    def run(self):
        self.started = True
        (year, day) = self.id()
        input_file = "inputs/input%02d.txt" % (day)
        parsed_input = None
        with open(input_file, "r") as io:
            with Timer(print_title = False) as parse_time:
                parsed_input = self.parse(io)

            self.parse_time = parse_time.elapsed

        if parsed_input is None:
            self.has_parsed_input = False
        else:
            self.has_parsed_input = True

        # Try solve() first, otherwise solve1 + solve2
        with Timer(print_title = False) as p_both_time:
            self.result = self.solve(parsed_input)
            if self.result is None:
                with Timer(print_title = False) as p1_time:
                    part1 = self.solve1(parsed_input)

                with Timer(print_title = False) as p2_time:
                    part2 = self.solve2(parsed_input)

                self.result = (part1, part2)
                self.has_separate_parts = True
                self.p1_time = p1_time.elapsed
                self.p2_time = p2_time.elapsed
            else:
                self.p1_time = None
                self.p2_time = None
                self.has_separate_parts = False

        self.p_both_time = p_both_time.elapsed
        self.expected_result = self.expected()
        self.total_time = self.p_both_time + self.parse_time
        if self.expected_result == self.result:
            self.status = "OK"
        else:
            self.status = "FAILED"
