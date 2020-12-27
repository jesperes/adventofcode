#!/usr/bin/env python3

import day01
import day02
import day03

from tabulate import tabulate

puzzles = [ day01.Day01(),
            day02.Day02(),
            day03.Day03() ]

def ts(t):
    ms = t * 1000
    return f"{ms:.3g} ms"

if __name__ == "__main__":
    for p in puzzles:
        p.run()

    sum_parse = sum([p.parse_time for p in puzzles])
    sum_p1 = sum([p.p1_time for p in puzzles])
    sum_p2 = sum([p.p2_time for p in puzzles])
    sum_total = sum([p.total_time for p in puzzles])

    table = [[p.year,
              p.day,
              ts(p.parse_time),
              ts(p.p1_time),
              ts(p.p2_time),
              ts(p.total_time),
              p.part1,
              p.part2,
              p.status
              ] for p in puzzles] + \
              [["Total",
                "",
                ts(sum_parse),
                ts(sum_p1),
                ts(sum_p2),
                ts(sum_total),
                "",
                "",
                ""]]

    print(tabulate(table,
                   headers = ["Year",
                              "Day",
                              "Parse time",
                              "Part 1",
                              "Part 2",
                              "Total",
                              "Answer (p1)",
                              "Answer (p2)",
                              "Status"],
                   tablefmt = "fancy_grid"))
