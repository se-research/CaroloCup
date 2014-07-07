/*
 * bsxfun.c
 *
 * Code generation for function 'bsxfun'
 *
 * C source code generated on: Tue Jan 28 02:26:12 2014
 *
 */

/* Include files */
#include "rt_nonfinite.h"
#include "nnRoadAngleCalc.h"
#include "nnRoadSizeCalc.h"
#include "bsxfun.h"

/* Function Definitions */
void b_bsxfun(const double a[9], const double b[3], double c[9])
{
  int ak;
  int ck;
  int k;
  double cv[3];
  ak = 0;
  for (ck = 0; ck < 8; ck += 3) {
    for (k = 0; k < 3; k++) {
      cv[k] = a[ak + k] * b[k];
    }

    for (k = 0; k < 3; k++) {
      c[ck + k] = cv[k];
    }

    ak += 3;
  }
}

void bsxfun(const double a[3], const double b[3], double c[9])
{
  int ak;
  int ck;
  int k;
  double cv[3];
  ak = 0;
  for (ck = 0; ck < 8; ck += 3) {
    for (k = 0; k < 3; k++) {
      cv[k] = a[ak] - b[k];
    }

    for (k = 0; k < 3; k++) {
      c[ck + k] = cv[k];
    }

    ak++;
  }
}

void c_bsxfun(const double a[9], double b, double c[9])
{
  int ak;
  int ck;
  int k;
  double cv[3];
  ak = 0;
  for (ck = 0; ck < 8; ck += 3) {
    for (k = 0; k < 3; k++) {
      cv[k] = a[ak + k] + b;
    }

    for (k = 0; k < 3; k++) {
      c[ck + k] = cv[k];
    }

    ak += 3;
  }
}

void d_bsxfun(const double a[3], double b, double c[3])
{
  int ak;
  int ck;
  ak = 0;
  for (ck = 0; ck < 3; ck++) {
    c[ck] = a[ak] - b;
    ak++;
  }
}

void e_bsxfun(const double a[3], double b, double c[3])
{
  int ak;
  int ck;
  ak = 0;
  for (ck = 0; ck < 3; ck++) {
    c[ck] = a[ak] / b;
    ak++;
  }
}

void f_bsxfun(const double a[3], double b, double c[3])
{
  int ak;
  int ck;
  ak = 0;
  for (ck = 0; ck < 3; ck++) {
    c[ck] = a[ak] + b;
    ak++;
  }
}

/* End of code generation (bsxfun.c) */
