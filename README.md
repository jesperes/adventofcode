# Advent of code

[![Build Status](https://travis-ci.org/jesperes/adventofcode.svg?branch=master)](https://travis-ci.org/jesperes/adventofcode)

These are my solutions to the AdventOfCode (https://adventofcode.com).

## Erlang

Erlang solutions are collected in a rebar3 application `aoc_erlang`, and
can be run by

```
$ cd aoc_erlang
$ rebar3 eunit [--suite aocYYYY_dayDD]
```

## Java

Java solutions are in a Eclipse-project in `aoc_java`, and can be run using Maven:

```
$ cd aoc_java
$ mvn test
```

## C

C solutions are built with CMake:

```
$ cd aoc_c
$ mkdir build && cmake ..
$ cd build && make && ctest
```

## Status

E:   Erlang
J:   Java
C:   C
+:   C++

| Day | 2015 | 2016 | 2017 | 2018 | 2019 |
|-----|------|------|------|------|------|
| 1   | EJ   | EJ   | E    | EJC  | EJ   |
| 2   | EJ   | E    | E    | EJ   | EJ   |
| 3   | EJ   | E    | E    | E    | EJ   |
| 4   | EJ   | E    | EJ   | E    | EJ   |
| 5   | EJ   | E    | EJ   | EJ   | EJ   |
| 6   | EJC  | E    | J    | E    | E    |
| 7   | EJ   | E    | J    | E    | E    |
| 8   | EJ   | E    | J    | E    | E    |
| 9   | EJ   | EJ   |      | E    | E    |
| 10  | EJ   | E    | J    | E    | E    |
| 11  | EJ   |      | J    | E    | E    |
| 12  | EJ   | E    | J    | E    |      |
| 13  | EJ   | E    | J    | EJ   |      |
| 14  | EJ   | EJC  | J    | EJ   |      |
| 15  | EJ   | E    | J    | EJ   |      |
| 16  | EJ   | E    | J    | E    |      |
| 17  | EJ   | E    | J    | E    |      |
| 18  | EJ   | E    | J    | E    |      |
| 19  | EJ   | EJ   | J    | E    |      |
| 20  | EJ   | E    | J    | E    |      |
| 21  | EJ   | EJ   | J    | E C  |      |
| 22  | EJ   | E    | J    | EJ   |      |
| 23  | EJ   | EJ   |      | E    |      |
| 24  | EJ   | E    | J    | EJ   |      |
| 25  | EJ   | E    | J    | EJ   |      |
