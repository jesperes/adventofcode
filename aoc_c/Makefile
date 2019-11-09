BUILD_TYPE=Release

all:
	mkdir -p build/${BUILD_TYPE}
	(cd build/${BUILD_TYPE} && cmake ../.. && cmake --build .)
	time ./build/${BUILD_TYPE}/aoc2016
