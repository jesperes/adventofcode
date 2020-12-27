import re

from aoccommon import AocPuzzle
from functools import reduce

class Day03(AocPuzzle):
    def id(self):
        return (2020, 3)

    def parse(self, io):
        return io.readlines()

    def solve1(self, data):
        return self.tobogganing(data, 3, 1)

    def solve2(self, data):
        return reduce(lambda x, y: x*y,
                      [self.tobogganing(data, dx, dy)
                       for dx, dy in [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]])

    def tobogganing(self, data, dx, dy):
        (x, y) = (0, 0)
        trees = 0
        width = len(data[0].strip())
        while y < len(data):
            if data[y][x] == "#":
                trees += 1
            x = (x + dx) % width
            y = (y + dy)

        return trees

    def expected(self):
        return (230, 9533698720)
