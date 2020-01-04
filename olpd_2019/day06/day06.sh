#!/bin/bash

erlc day06.erl
erl -noshell -eval 'ok = day06:test(), erlang:halt(0).' > output.txt
