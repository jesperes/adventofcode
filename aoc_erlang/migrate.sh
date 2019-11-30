#!/bin/bash

set -x
set -e

YEAR=$1; shift
DAY=$1; shift

SRC=$HOME/dev/adventofcode
DEST=$HOME/dev/adventofcode/aoc_erlang/apps/aoc_erlang

day2=$(printf "%02d" $DAY)
cp -v $SRC/$YEAR/$DAY/puzzle$DAY.erl $DEST/src/aoc${YEAR}_day${day2}.erl

INPUT=$SRC/$YEAR/$DAY/input.txt
if [ -f $INPUT ]; then
    cp -v $INPUT $DEST/priv/inputs/$YEAR/input${day2}.txt
fi
