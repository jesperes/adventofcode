#!/bin/bash

set -x
set -e

YEAR=$1; shift
DAY=$1; shift

SRC=$HOME/dev/adventofcode
DEST=$HOME/dev/adventofcode/aoc_erlang/apps/aoc_erlang

cp -v $SRC/$YEAR/$DAY/puzzle$DAY.erl $DEST/src/aoc${YEAR}_day${DAY}.erl

INPUT=$SRC/$YEAR/$DAY/input.txt

if [ -f $INPUT ]; then
    cp -v $INPUT $DEST/priv/inputs/$YEAR/input$DAY.txt
fi
