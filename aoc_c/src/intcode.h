#ifndef _INTCODE_H
#define _INTCODE_H

#include <stdint.h>

/*
 * Intcode state
 */
typedef struct {
  int pc;
  int relbase;
  int64_t *prog;
  size_t progc;
  int64_t *input;
  size_t inputc;
  int64_t *output;
  size_t outputc;
} intcode_t;

typedef enum
  {
   StatusInput,
   StatusOutput,
  } IntcodeStatusCode;

typedef struct {
  IntcodeStatusCode status;
  int64_t output;
} intcode_status_t;

void intcode_init(intcode_t *p);
void intcode_execute(intcode_t *p);
void intcode_deinit(intcode_t *p);

#endif
