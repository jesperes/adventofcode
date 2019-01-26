#include <stdio.h>
#include <stdlib.h>

#define MAX_FREQ 2000000
#define MAX_INPUT 2000
#define MAX_LINE 256

int main(int argc, char **argv) {
  int seen_freqs[MAX_FREQ];
  int freqlist[MAX_INPUT];
  int numfreqs = 0;
  char buf[MAX_LINE];
  
  FILE *f = fopen(argv[1], "r");
  int i = 0;
  while (!feof(f))
  {
    char *s = fgets(buf, 256, f);
    if (s == NULL)
      break;
    freqlist[i++] = atoi(s);
    numfreqs++;
  }
  
  int freq = 0;
  while (1) {
    for (int i = 0; i < numfreqs; i++) {
      // freq can be negative, so offset all frequencies in the array
      // by MAX_FREQ / 2.
      int idx = freq + (MAX_FREQ / 2);
      seen_freqs[idx]++;
      if (seen_freqs[idx] >= 2) {
        printf("First duplicate frequency: %d\n", freq);
        return 0;
      }
      
      freq += freqlist[i];
    } 
  }
}
