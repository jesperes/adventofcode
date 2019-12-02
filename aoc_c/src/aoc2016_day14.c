#include "md5.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define MAX_HASHES 100000
#define NTH_KEY 64
#define TEST_INPUT "abc"
#define INPUT "ahsbgdzn"

char *hashes[MAX_HASHES];

void reinit_cache() {
  for (int i = 0; i < MAX_HASHES; i++) {
    if (hashes[i] != NULL) {
      free(hashes[i]);
    }

    hashes[i] = NULL;
  }
}

#define TO_HEXB(X) (char)(((X) <= 9) ? ((X) + 48) : ((X) + 87))

void md5_hexdigest(char *input, char *outbuf) {
  MD5_CTX ctx;
  unsigned char digest[16];

  MD5_Init(&ctx);
  MD5_Update(&ctx, input, strlen(input));
  MD5_Final(digest, &ctx);

  for (int j = 0; j < 16; j++) {
    outbuf[j * 2] = TO_HEXB((digest[j] & 0xf0) >> 4);
    outbuf[j * 2 + 1] = TO_HEXB(digest[j] & 0x0f);
  }

  outbuf[32] = 0;
}

char *md5_cached(char *input, int i) {
  if (hashes[i] == NULL) {
    char buf[64];
    sprintf(buf, "%s%d", input, i);
    hashes[i] = malloc(33);
    md5_hexdigest(buf, hashes[i]);
  }

  return hashes[i];
}

char *md5_cached_part2(char *input, int i) {
  if (hashes[i] == NULL) {
    char buf[64];
    sprintf(buf, "%s%d", input, i);
    hashes[i] = malloc(33);
    md5_hexdigest(buf, hashes[i]);

    for (int j = 0; j < 2016; j++) {
      md5_hexdigest(hashes[i], hashes[i]);
    }
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

int main() {

  /*
   * Part 1
   */

  // Just a sanity check that our md5 implementation works.
  assert(strcmp(md5_cached("abc", 0),
                "577571be4de9dcce85a041ba0410f29f") == 0);

  reinit_cache();

  int keys_found = 0;
  int nth_key = -1;

  for (int i = 0; keys_found < NTH_KEY; i++) {
    char *hash = md5_cached(INPUT, i);
    char c3 = has3(hash);
    if (c3 != -1) {
      // check if any of the next 1000 hashes contain
      // a 5-letter sequence of
      for (int j = i + 1; j <= i + 1000; j++) {
        char *hash5 = md5_cached(INPUT, j);
        if (has5(hash5, c3)) {
          keys_found++;
          nth_key = i;
          break;
        }
      }
    }
  }

  assert(nth_key == 23890);

  /*
   * Part 2
   */

  reinit_cache();


  assert(strcmp(md5_cached_part2(TEST_INPUT, 0),
                "a107ff634856bb300138cac6568c0f24") == 0);

  reinit_cache();
  keys_found = 0;
  nth_key = -1;

  for (int i = 0; keys_found < NTH_KEY; i++) {
    char *hash = md5_cached_part2(INPUT, i);
    char c3 = has3(hash);
    if (c3 != -1) {

      // check if any of the next 1000 hashes contain
      // a 5-letter sequence of
      for (int j = i + 1; j <= i + 1000; j++) {
        char *hash5 = md5_cached_part2(INPUT, j);
        if (has5(hash5, c3)) {
          keys_found++;
          nth_key = i;
          break;
        }
      }
    }
  }

  assert(nth_key == 22696);

  return 0;
}
