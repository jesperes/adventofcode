#include "aoc2016.h"
#include "md5.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

unsigned char *md5(char *in) {
  MD5_CTX ctx;
  unsigned char digest[16];
  unsigned char *buf = malloc(33);
  MD5_Init(&ctx);
  MD5_Update(&ctx, in, strlen(in));
  MD5_Final(digest, &ctx);
  for (int i = 0; i < 16; i++) {
    sprintf((char *)&buf[i * 2], "%02x", digest[i]);
  }
  return buf;
}

#define MAX_HASHES 100000
#define NTH_KEY 64
// #define INPUT "abc"
#define INPUT "ahsbgdzn"

char *hashes[MAX_HASHES];

char *md5_cached(int i) {
  if (hashes[i] == NULL) {
    MD5_CTX ctx;
    unsigned char digest[16];
    char buf[64];

    hashes[i] = malloc(33);

    sprintf(buf, "%s%d", INPUT, i);

    MD5_Init(&ctx);
    MD5_Update(&ctx, buf, strlen(buf));
    MD5_Final(digest, &ctx);

    for (int j = 0; j < 16; j++) {
      sprintf((char *)&hashes[i][j * 2], "%02x", digest[j]);
    }

    hashes[i][32] = 0;
  }

  return hashes[i];
}

// If hash has a three letter sequence, return that letter, otherwise
// return -1;
char has3(char *hash) {
  for (int i = 0; i < strlen(hash) - 2; i++) {
    char c = hash[i];
    if (hash[i + 1] == c && hash[i + 2] == c) {
      return hash[i];
    }
  }
  return -1;
}

// If hash has a 5-letter sequence of c, return true, otherwise return false.
bool has5(char *hash, char c) {
  for (int i = 0; i < strlen(hash) - 4; i++) {
    if (hash[i] == c && hash[i + 1] == c && hash[i + 2] == c &&
        hash[i + 3] == c && hash[i + 4] == c) {
      return true;
    }
  }
  return false;
}

int_solution_t aoc2016_day14() {

  for (int i = 0; i < MAX_HASHES; i++) {
    hashes[i] = NULL;
  }

  int keys_found = 0;
  int nth_key = -1;

  for (int i = 0; keys_found < NTH_KEY; i++) {
    char *hash = md5_cached(i);
    char c3 = has3(hash);
    if (c3 != -1) {
      // check if any of the next 1000 hashes contain
      // a 5-letter sequence of
      for (int j = i + 1; j <= i + 1000; j++) {
        char *hash5 = md5_cached(j);
        if (has5(hash5, c3)) {
          keys_found++;
          nth_key = i;
          break;
        }
      }
    }
  }

  int_solution_t sol;

  sol.part1 = nth_key;
  sol.part2 = -1;

  return sol;
}
