#!/usr/bin/env python3

import day01

puzzles = [ day01.Day01() ]

def ts(t):
    return f"{t*1000:6.3g} ms"

if __name__ == "__main__":
    for p in puzzles:
        p.run()
        (year, day) = p.id()
        if p.result_is_ok():
            resultstr = "OK!"
        else:
            resultstr = f"Failed! {p.expected_result} != {p.result}"

        if p.has_separate_parts:
            print(f"{year} {day}: {ts(p.parse_time)} (parsing) {ts(p.p1_time)} (part1) {ts(p.p2_time)} (part2) {p.result} {resultstr}")
        else:
            print(f"{year} {day}: {ts(p.parse_time)} (parsing) {ts(p.p_both_time)} {p.result} {resultstr}")
