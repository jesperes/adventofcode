#!/bin/bash

PART1=$(./intcode.sh ../inputs/input05.txt 1 | tail -n 1)
PART2=$(./intcode.sh ../inputs/input05.txt 5)

[ $PART1 = 16348437 ] || exit 1
[ $PART2 = 6959377 ] || exit 1
