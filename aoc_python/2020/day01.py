from aoccommon import AocPuzzle

class Day01(AocPuzzle):
    def id(self):
        return (2020, 1)

    def parse(self, io):
        return list(map(int, io.readlines()))

    def solve1(self, data):
        return [x * y
                for x in data
                for y in data
                if (x < y) and (x + y == 2020)
            ][0]

    def solve2(self, data):
        minz = min(data)
        return [x * y * z
                for x in data
                for y in data
                if (x < y) and (x + y) < 2020 - minz
                for z in data
                if (x + y + z == 2020)
            ][0]

    def expected(self):
        return (987339, 259521570)
