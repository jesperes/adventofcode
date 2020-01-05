#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include <inttypes.h>
#include <stdio.h>

#include "intcode.h"

/* Addressing modes */
#define MODE_POS 0
#define MODE_IMM 1
#define MODE_REL 2

#define DEFAULT_OUTPUT_SIZE 10

int64_t read(int64_t *prog, int64_t op, int m, int relbase)
{
  if (m == MODE_POS) {
    return prog[op];
  } else if (m == MODE_IMM) {
    return op;
  } else if (m == MODE_REL) {
    return prog[op + relbase];
  } else {
    assert(false);
  }
}

void write(int64_t *prog, int64_t op, int m, int64_t val, int relbase)
{
  if (m == MODE_POS) {
    prog[op] = val;
  } else if (m == MODE_REL) {
    prog[op + relbase] = val;
  } else {
    assert(false);
  }
}

void intcode_init(intcode_t *p)
{
  memset(p, 0, sizeof(intcode_t));
}

// Read an intcode program from file.
void intcode_init_from_file(intcode_t *p, char *filename)
{
  intcode_init(p);

  FILE *f = fopen(filename, "r");
  assert(f != NULL);

  char buf[20];
  memset(buf, 0, sizeof(buf));
  char *q = buf;

  int progsize = 64; // Size of the intcode program (nr. of ints)
  int i = 0; // index into the intcode program's array of ints
  p->prog = (int64_t *)malloc(progsize * sizeof(int64_t));

  while (true) {
    int c = getc(f);

    // Reallocate program buffer when needed
    if (i >= progsize) {
      progsize *= 2;
      p->prog = (int64_t *)realloc(p->prog, progsize * sizeof(int64_t));
    }

    if (c == ',' || c == EOF) {
      p->prog[i++] = atoll(buf);
      buf[0] = 0;
      q = buf;
      memset(buf, 0, sizeof(buf));
      if (c == EOF)
        break;
    } else {
      *q++ = c;
    }
  }

  progsize *= 2;
  p->prog = (int64_t *)realloc(p->prog, progsize * sizeof(int64_t));

  // Zero out rest of program.
  for (int j = i; j < progsize; j++) {
    p->prog[j] = 0;
  }

  p->progsize = progsize;
}

void intcode_execute(intcode_t *p)
{
  while (true) {
    int64_t pc = p->pc;
    int relbase = p->relbase;
    int64_t *prog = p->prog;

    // Read opcode
    int64_t op0 = prog[pc] % 100;

#if 0
    printf("pc = %" PRId64
           ", opcode = %" PRId64
           ", prog[pc] = %" PRId64 "\n", pc, op0, prog[pc]);
#endif

    // Store last opcode for OP_END, OP_INPUT, and OP_OUTPUT
    // instructions.
    p->last_opcode = op0;

    // Read opcode modifiers
    int64_t m1 = (prog[pc] / 100) % 10;
    int64_t m2 = (prog[pc] / 1000) % 10;
    int64_t m3 = (prog[pc] / 10000) % 10;

    // Read operands
    int64_t op1 = prog[pc + 1];
    int64_t op2 = prog[pc + 2];
    int64_t op3 = prog[pc + 3];

    switch (op0) {
    case OP_ADD: {
      int64_t a = read(prog, op1, m1, relbase);
      int64_t b = read(prog, op2, m2, relbase);
      write(prog, op3, m3, a + b, relbase);
      p->pc += 4;
      break;
    }
    case OP_MUL: {
      int64_t a = read(prog, op1, m1, relbase);
      int64_t b = read(prog, op2, m2, relbase);
      write(prog, op3, m3, a * b, relbase);
      p->pc += 4;
      break;
    }
    case OP_JUMP_IF_TRUE: {
      if (read(prog, op1, m1, relbase)) {
        p->pc = read(prog, op2, m2, relbase);
      } else {
        p->pc += 3;
      }
      break;
    }
    case OP_JUMP_IF_FALSE: {
      if (read(prog, op1, m1, relbase)) {
        p->pc += 3;
      } else {
        p->pc = read(prog, op2, m2, relbase);
      }
      break;
    }
    case OP_LESS_THAN: {
      int64_t a = read(prog, op1, m1, relbase);
      int64_t b = read(prog, op2, m2, relbase);
      write(prog, op3, m3, (a < b) ? 1 : 0, relbase);
      p->pc += 4;
      break;
    }
    case OP_EQUALS: {
      int64_t a = read(prog, op1, m1, relbase);
      int64_t b = read(prog, op2, m2, relbase);
      write(prog, op3, m3, (a == b) ? 1 : 0, relbase);
      p->pc += 4;
      break;
    }
    case OP_ADJ_RELBASE: {
      p->relbase += read(prog, op1, m1, relbase);
      p->pc += 2;
      break;
    }
    case OP_INPUT: {
      if (!p->input_pending) {
        // Return to user to handle input instruction.
        p->input_pending = true;
        return;
      } else {
        // We have received input from user, write it to
        // memory and continue.
        write(prog, op1, m1, p->input, relbase);
        p->input_pending = false;
        p->pc += 2;
        break;
      }
    }
    case OP_OUTPUT: {
      if (!p->output_pending){
        // Return to user to handle output instruction.
        p->output = read(prog, op1, m1, relbase);
        p->output_pending = true;
        return;
      } else {
        // User has handled output instruction, continue.
        p->output_pending = false;
        p->pc += 2;
        break;
      }
    }
    case OP_END: {
      return;
    }
    }
  }
}

#ifdef INTCODE_ASCII_MAIN

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

/*
 * Intcode computer hooked up to an ascii-capable terminal.
 */
int main(int argc, char **argv)
{
  assert(argc == 2);

  // The intcode program state
  intcode_t intcode;
  intcode_init_from_file(&intcode, argv[1]);

  while (true) {
    intcode_execute(&intcode);
    // printf("Intcode exited with opcode %d\n", intcode.last_opcode);
    switch (intcode.last_opcode) {
    case OP_END: {
      return 0;
    }
    case OP_INPUT: {
      intcode.input = getc(stdin);
      break;
    }
    case OP_OUTPUT: {
      // putchar(intcode.output);
      break;
    }
    }
  }
}
#endif

#ifdef INTCODE_MAIN
int main(int argc, char **argv)
{
  assert(argc >= 2);
  int i = 2;

  // The intcode program state
  intcode_t intcode;
  intcode_init_from_file(&intcode, argv[1]);

  // intcode.prog[1] = 12;
  // intcode.prog[2] = 2;

  for (int j = 0; j < intcode.progsize; j++) {
    printf("%" PRId64 ",", intcode.prog[j]);
  }
  printf("\n");

  while (true) {
    intcode_execute(&intcode);

#if 0
    printf("Execute function returned with opcode %d\n", intcode.last_opcode);
    printf("Input pending: %d\n", intcode.input_pending);
    printf("Output pending: %d\n", intcode.output_pending);
#endif
    switch (intcode.last_opcode) {
    case OP_END: {
      printf("Program exiting, prog[0] = %" PRId64 "\n",
             intcode.prog[0]);
      return 0;
    }
    case OP_INPUT: {
      assert(i < argc);
      intcode.input = atol(argv[i++]);
      printf("INPUT = %" PRId64 "\n", intcode.input);
      break;
    }
    case OP_OUTPUT: {
      printf("%" PRId64 "\n", intcode.output);
      break;
    }
    }
  }
}
#endif
