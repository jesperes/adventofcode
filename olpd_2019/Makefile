ALL=\
	day01-awk \
	day02-groovy \
	day04-clpfd \
	day05-bash

all: $(ALL)
.PHONY: $(ALL)
.SILENT:

TIME=time --format='$@ \t%E'

day01-awk:
	cd day01 && $(TIME) awk -f day01.awk < ../inputs/input01.txt

day02-groovy:
	cd day02 && $(TIME) groovy day02.groovy 2>&1 | grep -v "^WARNING:"

day04-clpfd:
	cd day04 && $(TIME) swipl day04.pl

day05-bash:
	cd day05 && $(TIME) ./day05.sh
