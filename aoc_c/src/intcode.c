#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <assert.h>

#include "intcode.h"

/* Opcode definitions */
#define OP_ADD 1
#define OP_MUL 2
#define OP_INPUT 3
#define OP_OUTPUT 4
#define OP_JUMP_IF_TRUE 5
#define OP_JUMP_IF_FALSE 6
#define OP_LESS_THAN 7
#define OP_EQUALS 8
#define OP_ADJ_RELBASE 9
#define OP_END 99

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
};

/*
 * Execute the given intcode program.
 *
 * prog    The program, as an array of 64-bit ints. This
 *         should be malloc:ed, as it will be realloc:ed as needed.
 * progc   Size of the allocated buffer.
 * input   Array of inputs.
 * inputc  Size of input array.
 *
 * Returns a malloc:ed array of outputs.
 */
void intcode_execute(intcode_t *p)
{
  int pc = 0;
  size_t relbase = 0;
  int64_t *prog = p->prog;

  while (true) {
    // Read opcode
    int64_t op0 = prog[pc] % 100;

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
      pc += 4;
      break;
    }
    case OP_MUL: {
      int64_t a = read(prog, op1, m1, relbase);
      int64_t b = read(prog, op2, m2, relbase);
      write(prog, op3, m3, a * b, relbase);
      pc += 4;
      break;
    }
    case OP_JUMP_IF_TRUE: {
      if (read(prog, op1, m1, relbase)) {
        pc = read(prog, op2, m2, relbase);
      } else {
        pc += 3;
      }
      break;
    }
    case OP_JUMP_IF_FALSE: {
      if (read(prog, op1, m1, relbase)) {
        pc += 3;
      } else {
        pc = read(prog, op2, m2, relbase);
      }
      break;
    }
    case OP_LESS_THAN: {
      int64_t a = read(prog, op1, m1, relbase);
      int64_t b = read(prog, op2, m2, relbase);
      write(prog, op3, m3, (a < b) ? 1 : 0, relbase);
      pc += 4;
      break;
    }
    case OP_EQUALS: {
      int64_t a = read(prog, op1, m1, relbase);
      int64_t b = read(prog, op2, m2, relbase);
      write(prog, op3, m3, (a == b) ? 1 : 0, relbase);
      pc += 4;
      break;
    }
    case OP_ADJ_RELBASE: {
      relbase += read(prog, op1, m1, relbase);
      pc += 2;
      break;
    }
    case OP_INPUT: {
      break;
    }
    case OP_OUTPUT: {
      break;
    }
    case OP_END: {
      break;
    }
    }
  }
}
