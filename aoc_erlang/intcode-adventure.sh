#!/bin/bash
exec erl -noshell -pa ./_build/default/lib/aoc_erlang/ebin -s intcode execute_tty ./apps/aoc_erlang/priv/inputs/2019/input25.txt
