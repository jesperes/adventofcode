#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
int main(int argc, char **argv) {
int64_t freq = -1;
void *labels[] = {
  &&label_0,
  &&label_1,
  &&label_2,
  &&label_3,
  &&label_4,
  &&label_5,
  &&label_6,
  &&label_7,
  &&label_8,
  &&label_9,
  &&label_10,
  &&label_11,
  &&label_12,
  &&label_13,
  &&label_14,
  &&label_15,
  &&label_16,
  &&label_17,
  &&label_18,
  &&label_19,
  &&label_20,
  &&label_21,
  &&label_22,
  &&label_23,
  &&label_24,
  &&label_25,
  &&label_26,
  &&label_27,
  &&label_28,
  &&label_29,
  &&label_30,
  &&label_31,
  &&label_32,
  &&label_33,
  &&label_34,
  &&label_35,
  &&label_36,
  &&label_37,
  &&label_38,
  &&label_39,
  &&label_40,
};
  int64_t i = 0;
  int64_t a = 0;
  int64_t p = 0;
  int64_t b = 0;
  int64_t f = 0;

label_0:
  i = 31;

label_1:
  a = 1;

label_2:
  p *= 17;

label_3:
  if (p > 0) goto *labels[3 + (p)];

label_4:
  a *= 2;

label_5:
  i += -1;

label_6:
  if (i > 0) goto *labels[6 + (-2)];

label_7:
  a += -1;

label_8:
  i = 127;

label_9:
  p = 826;

label_10:
  p *= 8505;

label_11:
  p = p % a;

label_12:
  p *= 129749;

label_13:
  p += 12345;

label_14:
  p = p % a;

label_15:
  b = p;

label_16:
  b = b % 10000;

label_17:
  freq = b;

label_18:
  i += -1;

label_19:
  if (i > 0) goto *labels[19 + (-9)];

label_20:
  if (a > 0) goto *labels[20 + (3)];

label_21:
  if (b != 0) {
    printf("rcv = %ld\n", freq);
    exit((int)freq);
  }

label_22:
  if (b > 0) goto *labels[22 + (-1)];

label_23:
  f = 0;

label_24:
  i = 126;

label_25:
  if (a != 0) {
    printf("rcv = %ld\n", freq);
    exit((int)freq);
  }

label_26:
  if (b != 0) {
    printf("rcv = %ld\n", freq);
    exit((int)freq);
  }

label_27:
  p = a;

label_28:
  p *= -1;

label_29:
  p += b;

label_30:
  if (p > 0) goto *labels[30 + (4)];

label_31:
  freq = a;

label_32:
  a = b;

label_33:
  if (1 > 0) goto *labels[33 + (3)];

label_34:
  freq = b;

label_35:
  f = 1;

label_36:
  i += -1;

label_37:
  if (i > 0) goto *labels[37 + (-11)];

label_38:
  freq = a;

label_39:
  if (f > 0) goto *labels[39 + (-16)];

label_40:
  if (a > 0) goto *labels[40 + (-19)];
  return 0;
}
