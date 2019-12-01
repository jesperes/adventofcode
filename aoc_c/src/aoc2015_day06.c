#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/*
toggle 461,550 through 564,900
turn off 370,39 through 425,839
turn off 464,858 through 833,915
turn off 812,389 through 865,874
*/

struct Point {
  int x, y;
};

#define TOGGLE 1
#define TURN_ON 2
#define TURN_OFF 3

struct State {
  int on;
  int brightness;
};

struct Instr {
  int cmd;
  struct Point from;
  struct Point to;
};

int scan_command(FILE *f, struct Instr *instr)
{
  char buf[256];
  char *res = fgets(buf, 256, f);
  if (feof(f) || res == NULL)
    return 0;

  if (strncmp("toggle", buf, 6) == 0) {
    instr->cmd = TOGGLE;
    sscanf(buf, "toggle %d,%d through %d,%d\n",
           &(instr->from.x),
           &(instr->from.y),
           &(instr->to.x),
           &(instr->to.y));
  } else if (strncmp("turn on", buf, 7) == 0) {
    instr->cmd = TURN_ON;
    sscanf(buf, "turn on %d,%d through %d,%d\n",
           &(instr->from.x),
           &(instr->from.y),
           &(instr->to.x),
           &(instr->to.y));
  } else if (strncmp("turn off", buf, 6) == 0) {
    instr->cmd = TURN_OFF;
    sscanf(buf, "turn off %d,%d through %d,%d\n",
           &(instr->from.x),
           &(instr->from.y),
           &(instr->to.x),
           &(instr->to.y));
  } else {
    assert(0);
  }

  return 1;
}


int main(int argc, char **argv)
{
  FILE *f = fopen(argv[1], "r");
  if (f == NULL) {
    perror("fopen");
    exit(1);
  }

  struct Instr instr;
  struct State state[1000][1000] = {0};

  while (scan_command(f, &instr)) {
    for (int x = instr.from.x; x <= instr.to.x; x++) {
      for (int y = instr.from.y; y <= instr.to.y; y++) {
        switch (instr.cmd) {
        case TOGGLE:
          state[x][y].on = !state[x][y].on;
          state[x][y].brightness += 2;
          break;
        case TURN_ON:
          state[x][y].on = 1;
          state[x][y].brightness++;
          break;
        case TURN_OFF:
          state[x][y].on = 0;
          if (state[x][y].brightness >= 1) {
            state[x][y].brightness--;
          }
          break;
        }
      }
    }
  }

  int lights_on = 0;
  int total_brightness = 0;
  for (int x = 0; x <= 999; x++) {
    for (int y = 0; y <= 999; y++) {
      if (state[x][y].on)
        lights_on++;

      total_brightness += state[x][y].brightness;
    }
  }

  assert(lights_on == 543903);
  assert(total_brightness == 14687245);
}
