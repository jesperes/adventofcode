import re

from aoccommon import AocPuzzle

class Day02(AocPuzzle):
    def id(self):
        return (2020, 2)

    def parse(self, io):
        return list(map(lambda x: self.parse_line(x), io.readlines()))

    # For this puzzle, parsing the input dominates the runtime,
    # because regexps.
    def parse_line(self, line):
        (a, b, c, pwd) = re.split(r'[- :\n]+', line.strip())
        return (int(a), int(b), c, pwd)

    def solve1(self, data):
        n = 0
        for line in data:
            if self.is_valid_password1(*line):
                n += 1
        return n

    def solve2(self, data):
        n = 0
        for line in data:
            if self.is_valid_password2(*line):
                n += 1
        return n

    def is_valid_password1(self, min_c, max_c, c, pwd):
        n = 0
        for x in pwd:
            if x == c:
                n += 1

            if n > max_c:
                return False

        return n >= min_c

    def is_valid_password2(self, pos_a, pos_b, c, pwd):
        a = (pwd[pos_a - 1] == c)
        b = (pwd[pos_b - 1] == c)
        return (a or b) and (not (a and b))

    def expected(self):
        return (660, 530)
