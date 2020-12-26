#!/usr/bin/env python3

import day01
import day02
import day03

from tabulate import tabulate

def isalambda(v):
  LAMBDA = lambda:0
  return isinstance(v, type(LAMBDA)) and v.__name__ == LAMBDA.__name__

puzzles = [ day01.Day01(),
            day02.Day02(),
            day03.Day03() ]

def ts(t):
    ms = t * 1000
    if ms < 1:
        us = t * 1000000
        return f"{us:g} us"
    else:
        return f"{ms:g} ms"

if __name__ == "__main__":
    for p in puzzles:
        p.run()

    Table = [[p.year,
              p.day,
              ts(p.parse_time),
              ts(p.p1_time),
              ts(p.p2_time),
              ts(p.total_time),
              p.part1,
              p.part2,
              p.status
              ] for p in puzzles]

    print(tabulate(Table,
                   headers = ["Year", "Day", "Parse time", "Part 1", "Part 2", "Total", "Answer (p1)", "Answer (p2)", "Status"],
                   tablefmt = "fancy_grid"))
