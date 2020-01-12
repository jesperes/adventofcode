#!/bin/bash
set -e
clang-format -i *.cpp
g++ -g -O3 -Wall -Werror day08.cpp -o day08
./day08 ../inputs/input08.txt
