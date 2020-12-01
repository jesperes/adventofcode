ALL=\
	day01-awk \
	day02-groovy \
	day03-java \
	day04-clpfd \
	day05-bash \
	day06-erlang \
	day07-c \
	day08-c++ \
	day09-ruby \
	day10-js

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

day07-c:
	cd day07 && $(TIME) ./day07.sh

day08-c++:
	cd day08 && $(TIME) ./day08.sh

day09-ruby:
	cd day09 && $(TIME) ./day09.rb --verbose=silent

day10-js:
	cd day10 && $(TIME) node ./day10.js

day11-perl:
	cd day11 && $(TIME) ./day11.perl
