/*
 * calculations.h
 *
 *  Created on: 10 jan 2012
 *      Author: benjamin
 */

#ifndef CALCULATIONS_H_
#define CALCULATIONS_H_

typedef struct
{
	double a;
	double b;
	double c;
	double d;
} matrix_2x2;

typedef struct
{
	double a;
	double b;
} vector_2x1;

double calc_point_point_distance(double x1, double y1, double x2, double y2);
matrix_2x2 inverse2x2matrix(matrix_2x2 matrix);
vector_2x1 add2x1vector(vector_2x1 A, vector_2x1 B);
vector_2x1 mul2x2matrix2x1vector(matrix_2x2 A, vector_2x1 B);
matrix_2x2 add2x2matrix(matrix_2x2 A, matrix_2x2 B);
matrix_2x2 mul2x2matrix(matrix_2x2 A, matrix_2x2 B);

#endif /* CALCULATIONS_H_ */
