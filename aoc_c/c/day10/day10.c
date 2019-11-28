#include <assert.h>
#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Reverse [start+len] subarray of circular buf.
void reverse(uint8_t buf[], int buflen, int start, int len) {
  // printf("len = %d, buflen = %d\n", len, buflen);
  assert(len <= buflen);

#if 0
  printf("Reversing substring of length %d starting at %d: [ ", len, start);
  for (int i = 0; i < buflen; i++) {
    printf("%d ", buf[i]);
  }
  printf("] -> ");
#endif

  for (int i = 0; i < len / 2; i++) {
    int a = (start + i) % buflen;
    int b = (start + len - 1 - i) % buflen;
    // printf("Swapping indexes %d and %d\n", a, b);
    int tmp = buf[a];
    buf[a] = buf[b];
    buf[b] = tmp;
  }

#if 0
  printf("[ ");
  for (int i = 0; i < buflen; i++) {
    printf("%d ", buf[i]);
  }
  printf("]\n");
#endif
}

int pos = 0;
int skip = 0;

/*
 * input - the circular input list of length 'inputlen'
 * lengths - the list of lengths to use
 */
int hashFunc(uint8_t input[], int inputlen, uint8_t lengths[], int nlen) {

  // Iterate over the inputs lengths
  for (int i = 0; i < nlen; i++) {
    // For each length in the "lengths" array we are to do one reversal.
    reverse(input, inputlen, pos, lengths[i]);

    // Move the current position forward
    pos = (pos + lengths[i] + skip) % inputlen;

    // Increment the skip length
    skip++;

#if 0
    printf("After step %d [ ", i);
    for (int i = 0; i < inputlen; i++) {
      if (i == pos) {
        printf("[%d] ", input[i]);
      } else {
        printf("%d ", input[i]);
      }
    }
    printf("]\n");
#endif
  }

  return input[0] * input[1];
}

void assertArrayEquals(uint8_t array[], int len, uint8_t exp[]) {
  for (int i = 0; i < len; i++) {
    assert(array[i] == exp[i]);
  }
}

uint8_t *create_lengths_array(const char *str) {
  int lengths_len = strlen(str) + 5;
  uint8_t *lengths = (uint8_t *)malloc(lengths_len);
  memcpy(lengths, str, strlen(str));
  int i = strlen(str);
  lengths[i++] = 17;
  lengths[i++] = 31;
  lengths[i++] = 73;
  lengths[i++] = 47;
  lengths[i++] = 23;

  return lengths;
}

char *to_hex(uint8_t hash[16]) {
  char *hex = malloc(33);
  for (int i = 0; i < 16; i++) {
    sprintf(hex + i * 2, "%02x", hash[i]);
  }
  hex[32] = 0;
  return hex;
}

char *hashFunc2(const char *input) {
  // The ring we are rotating
  uint8_t ring[256];
  for (int i = 0; i < 256; i++) {
    ring[i] = i;
  }

  // The lengths to use
  uint8_t *lengths = create_lengths_array(input);
  int lengths_len = strlen(input) + 5;
  pos = 0;
  skip = 0;

  for (int i = 0; i < 64; i++) {
    hashFunc(ring, 256, lengths, lengths_len);
  }

  // Construct the dense hash
  uint8_t dense[16];
  for (int i = 0; i < 16; i++) {
    uint8_t x = 0;

    for (int j = 0; j < 16; j++) {
      x ^= ring[i * 16 + j];
    }

    dense[i] = x;
  }

  char *hex = to_hex(dense);
  printf("hash(\"%s\") = %s\n", input, hex);
  return hex;
}

int main(int argc, char **argv) {
  (void)&argv;
  (void)&argc;

  // Test cases for the list reversal code
  {
    uint8_t arr[] = {1, 2, 3, 4};
    uint8_t exp[] = {4, 3, 2, 1};
    reverse(arr, 4, 0, 4);
    assertArrayEquals(arr, 4, exp);
  }
  {
    uint8_t arr[] = {1, 2, 3, 4};
    uint8_t exp[] = {3, 2, 1, 4};
    reverse(arr, 4, 2, 3);
    assertArrayEquals(arr, 4, exp);
  }
  {
    uint8_t arr[] = {1, 2, 3, 4, 5, 6};
    uint8_t exp[] = {6, 5, 3, 4, 2, 1};
    reverse(arr, 6, 4, 4);
    assertArrayEquals(arr, 6, exp);
  }

  {
    // The short test
    uint8_t input[] = {0, 1, 2, 3, 4};
    uint8_t lengths[] = {3, 4, 1, 5};
    pos = 0;
    skip = 0;
    int result = hashFunc(input, 5, lengths, 4);
    assert(result == 12);
  }

  {
    uint8_t hex[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
    char *hexstr = to_hex(hex);
    printf("hexstr = %s\n", hexstr);
    free(hexstr);
  }

  {
    // The full test
    uint8_t input[256];
    for (int i = 0; i < 256; i++) {
      input[i] = i;
    }
    uint8_t lengths[] = {130, 126, 1,   11,  140, 2,   255, 207,
                         18,  254, 246, 164, 29,  104, 0,   224};
    skip = 0;
    pos = 0;
    int result =
        hashFunc(input, 256, lengths, sizeof(lengths) / sizeof(uint8_t));
    assert(result == 38628);
    printf("[Day10]: Result = %d\n", result);
  }

  // Part 2
  assert(strcmp("a2582a3a0e66e6e86e3812dcb672a272", hashFunc2("")) == 0);
  assert(strcmp("33efeb34ea91902bb2f59c9920caa6cd", hashFunc2("AoC 2017")) ==
         0);
  assert(strcmp("3efbe78a8d82f29979031a4aa0b16a9d", hashFunc2("1,2,3")) == 0);
  assert(strcmp("63960835bcdc130f0b66d7ff4f6a5a8e", hashFunc2("1,2,4")) == 0);

  assert(
      strcmp("e1462100a34221a7f0906da15c1c979a",
             hashFunc2(
                 "130,126,1,11,140,2,255,207,18,254,246,164,29,104,0,224")) ==
      0);
}
