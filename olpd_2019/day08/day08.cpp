#include <cassert>
#include <climits>
#include <fstream>
#include <iostream>
#include <sstream>

// Image is 25x6 pixels, and 100 layers thick.

int main(int argc, char **argv) {
  int image[100][25][6];
  assert(argc == 2);
  std::fstream fin(argv[1], std::fstream::in);
  for (int layer = 0; layer < 100; layer++) {
    for (int y = 0; y < 6; y++) {
      for (int x = 0; x < 25; x++) {
        if (fin.eof())
          break;

        char c;
        fin >> c;
        image[layer][x][y] = c - '0';
      }
    }
  }

  // =====[ PART 1 ]======================================================

  int zeroes = INT_MAX;
  int minlayer = 0;
  for (int layer = 0; layer < 100; layer++) {
    int num_zeroes = 0;
    for (int y = 0; y < 6; y++) {
      for (int x = 0; x < 25; x++) {
        if (image[layer][x][y] == 0)
          num_zeroes++;
      }
    }
    if (num_zeroes < zeroes) {
      minlayer = layer;
      zeroes = num_zeroes;
    }
  }

  // Count the number of 1's and 2's on that layer.
  int ones = 0, twos = 0;
  for (int y = 0; y < 6; y++) {
    for (int x = 0; x < 25; x++) {
      switch (image[minlayer][x][y]) {
      case 1:
        ones++;
        break;
      case 2:
        twos++;
        break;
      }
    }
  }

  assert(1703 == ones * twos);

  // =====[ PART 2 ]======================================================

  std::stringstream str;

  for (int y = 0; y < 6; y++) {
    for (int x = 0; x < 25; x++) {
      int pixel = -1;

      for (int layer = 0; layer < 100; layer++) {
        int c = image[layer][x][y];
        if (c == 2) { // transparent
          continue;
        } else {
          pixel = c;
          break;
        }
      }

      if (pixel == 1) {
        str << "\u2588"; // black
      } else {
        str << "\u2591"; // light shade
      }
    }

    str << std::endl;
  }

  //  std::cout << str.str();

  assert(str.str() == "█░░█░░██░░░██░░████░████░\n"
                      "█░░█░█░░█░█░░█░█░░░░█░░░░\n"
                      "████░█░░░░█░░░░███░░███░░\n"
                      "█░░█░█░░░░█░██░█░░░░█░░░░\n"
                      "█░░█░█░░█░█░░█░█░░░░█░░░░\n"
                      "█░░█░░██░░░███░█░░░░████░\n");
}
