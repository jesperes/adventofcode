#ifndef _INTCODE_H
#define _INTCODE_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

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

/*
 * Intcode state
 */
typedef struct {
  int pc;                       /* Program counter */
  int relbase;                  /* For relative parameter modes */

  int64_t *prog;                /* Intcode program */
  int progsize;

  int last_opcode;

  bool input_pending;
  int64_t input;

  bool output_pending;
  int64_t output;

} intcode_t;

void intcode_init(intcode_t *p);

/*
 * Execute intcode program. The function returns when it encounters a
 * OP_END, OP_INPUT, or OP_OUTPUT instruction. The instruction can be
 * found in the "last_opcode" field of the intcode_t struct.
 *
 * OP_END: The program has terminated.
 *
 * OP_INPUT: The program is waiting for input. Write the input value
 *           to the "input" field.
 *
 * OP_OUTPUT: The program has produced output. Output value is stored
 *            in the "output" field.
 *
 * To continue executing, call intcode_execute() again.
 */
void intcode_execute(intcode_t *p);

void intcode_init_from_file(intcode_t *p, const char *filename);

#endif
