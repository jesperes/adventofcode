#!/bin/bash

AOC_C_SRC=../../aoc_c/src
CFLAGS="-g -O3 -Wall -Werror"
clang-format -i day07.c
gcc -c ${CFLAGS} -I${AOC_C_SRC} ${AOC_C_SRC}/intcode.c -o intcode.o
gcc -c ${CFLAGS} -I${AOC_C_SRC} day07.c -o day07.o
gcc day07.o intcode.o -o day07
./day07 ../inputs/input07.txt
