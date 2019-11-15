BUILD_TYPE=Release

ifeq (output-sync, $(filter output-sync, $(.FEATURES)))
MAKEFLAGS += --output-sync=none
endif

all:
	mkdir -p build/${BUILD_TYPE}
	(cd build/${BUILD_TYPE} && cmake ../.. && cmake --build .)
	time ./build/${BUILD_TYPE}/aoc2016
