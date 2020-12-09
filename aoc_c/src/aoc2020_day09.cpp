#include <iostream>
#include <fstream>
#include <vector>
#include <limits>
#include <chrono>



int main(int argc, char **argv)
{
  using namespace std::chrono;
  high_resolution_clock::time_point t1 = high_resolution_clock::now();

  int preamble = std::stoi(argv[1]);
  std::vector<int64_t> input;
  std::ifstream infile(argv[2]);
  int a;
  while (infile >> a) {
    input.push_back(a);
  }

  int64_t sol1;

  for (size_t i = 0; i < (input.size() - preamble - 1); i++) {
    int64_t next = input[i + preamble];
    bool found = false;

    for (int x = 0; x < preamble && !found; x++) {
      for (int y = 0; y < preamble && !found; y++) {
        int64_t a = input[i + x];
        int64_t b = input[i + y];
        if (x > y)
          continue;

        if (a + b == next) {
          found = true;
        }
      }
    }

    if (!found) {
      std::cout << next << std::endl;
      sol1 = next;
    }
  }

  bool found = false;
  int64_t maxint = std::numeric_limits<int64_t>::max();

  for (size_t len = 2; len < input.size() && !found; len++) {
    for (size_t i = 0; i < input.size() - len && !found; i++) {
      int64_t min = maxint, max = 0, sum = 0;

      for (size_t j = i; (j < i + len) && !found; j++) {
        int64_t a = input[j];
        sum += a;
        if (a < min) min = a;
        if (a > max) max = a;
      }

      if (sum == sol1) {
        std::cout << (min + max) << std::endl;
        found = true;
      }
    }
  }

  high_resolution_clock::time_point t2 = high_resolution_clock::now();
  duration<double> time_span = duration_cast<duration<double>>(t2 - t1);

  std::cout << time_span.count() << " seconds" << std::endl;
}
