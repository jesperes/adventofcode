#!/usr/bin/env python3

import day01
import day02

def isalambda(v):
  LAMBDA = lambda:0
  return isinstance(v, type(LAMBDA)) and v.__name__ == LAMBDA.__name__

puzzles = [ day01.Day01(),
            day02.Day02() ]

columns = {"year"        : ("Year",         "%d",     6),
           "day"         : ("Day",          "%d",     3),
           "parse_time"  : ("Parsing",      lambda f: ts(f),  12),
           "p1_time"     : ("Part 1",       lambda f: ts(f),  12),
           "p2_time"     : ("Part 2",       lambda f: ts(f),  12),
           "p_both_time" : ("Part 1 & 2",   lambda f: ts(f),  12),
           "result"      : ("Result",       "%10.10s %10.10s", 21),
           "status"      : ("Status",       "%s",    10)
           }

def header():
    return "+" + "+".join(map(lambda col: (columns[col][2] * "-"), columns)) + "+"

def field(p, name, fmt, width):
    if isalambda(fmt):
        s = fmt(getattr(p, name))
    else:
        s = fmt % getattr(p, name)

    return "%*.*s" % (width, width, s)

def label(col, columns):
    width = columns[col][2]
    return "%*.*s" % (width, width, columns[col][0])

def line(p):
    return "|" + "|".join(map(lambda col: field(p, col, columns[col][1], columns[col][2]), columns)) + "|"

def column_labels():
    return "|" + "|".join(map(lambda col: label(col, columns), columns)) + "|"

def ts(t):
    return f"{t*1000:8.3g} ms"

if __name__ == "__main__":
    print(header())
    print(column_labels())
    print(header())
    for p in puzzles:
        p.run()
        print(line(p))
    print(header())
