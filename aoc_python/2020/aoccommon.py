import time
from block_timer.timer import Timer

class AocPuzzle:

    def __init__(self):
        self.started = False
        self.completed = False
        self.success = False
        self.p1_time = None
        self.p2_time = None
        self.parse_time = None
        self.total_time = None
        self.part1 = None
        self.part2 = None
        self.result = None
        self.expected_result = None
        self.parsed_input = None
        (self.year, self.day) = self.id()

    def has_method(self, name):
        hasattr(self, name) and callable(getattr(self, name))

    def id(self):
        return None

    def parse(self, io):
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

    def parse_input_file(self, input_file):
        try:
            with open(input_file, "r") as io:
                return self.parse(io)
        except IOError:
            return None

    def run(self):
        self.started = True
        (year, day) = self.id()
        input_file = "inputs/input%02d.txt" % (day)

        print(f"Running {year} day {day}... ", end="", flush=True)
        print("[PARSING] ", end="", flush=True)
        with Timer(print_title = False) as parse_time:
            self.parsed_input = self.parse_input_file(input_file)

        print("[PART1] ", end="", flush=True)

        with Timer(print_title = False) as p1_time:
            self.part1 = self.solve1(self.parsed_input)

        print("[PART2] ", end="", flush=True)
        with Timer(print_title = False) as p2_time:
            self.part2 = self.solve2(self.parsed_input)

        self.parse_time = parse_time.elapsed
        self.p1_time = p1_time.elapsed
        self.p2_time = p2_time.elapsed
        self.total_time = self.p1_time + self.p2_time + self.parse_time

        self.result = (self.part1, self.part2)
        self.expected_result = self.expected()
        if self.expected_result == self.result:
            self.status = "OK"
        else:
            self.status = "FAILED"

        print(f"[{self.status}]")
