ALL=\
	day01-awk \
	day02-groovy \
	day03-java \
	day04-clpfd \
	day05-bash \
	day06-erlang

all: $(ALL)
.PHONY: $(ALL)
.SILENT:

TIME=time --format='$@ \t%E'

day01-awk:
	cd day01 && $(TIME) awk -f day01.awk < ../inputs/input01.txt

day02-groovy:
	cd day02 && $(TIME) groovy day02.groovy 2>&1 | grep -v "^WARNING:"

day03-java:
	cd day03 && $(TIME) ./day03.sh

day04-clpfd:
	cd day04 && $(TIME) swipl day04.pl

day05-bash:
	cd day05 && $(TIME) ./day05.sh

day06-erlang:
	cd day06 && $(TIME) ./day06.sh
