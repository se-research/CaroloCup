/*
 * calculations.c
 *
 *  Created on: 10 jan 2012
 *      Author: benjamin
 */

#include "calculations.h"
#include "uart_ft.h"
#include <math.h>

double calc_point_point_distance(double x1, double y1, double x2, double y2) {
	return sqrt((x2-x1) * (x2-x1) + (y2-y1)*(y2-y1));
}

matrix_2x2 inverse2x2matrix(matrix_2x2 matrix)
{
	if(matrix.b*matrix.d == matrix.a*matrix.c)
	{
		uart_ft_print_string("Attempt to divide by zero");
		return matrix;
	}

	double scalar = 1/(matrix.b*matrix.d-matrix.a*matrix.c);
	double temp = matrix.a;

	matrix.a = scalar*matrix.d;
	matrix.d = scalar*temp;
	matrix.c = scalar*(-matrix.c);
	matrix.b = scalar*(-matrix.b);

	return matrix;
}

matrix_2x2 mul2x2matrix(matrix_2x2 A, matrix_2x2 B)
{
	matrix_2x2 C;

	C.a = A.a*B.a+A.b*B.c;
	C.b = A.a*B.b+A.b*B.d;
	C.c = A.c*B.a+A.d*B.c;
	C.d = A.c*B.b+A.d*B.d;

	return C;
}

matrix_2x2 add2x2matrix(matrix_2x2 A, matrix_2x2 B)
{
	matrix_2x2 C;
	C.a = A.a+B.a;
	C.b = A.b+B.b;
	C.c = A.c+B.c;
	C.d = A.d+B.d;

	return C;
}

vector_2x1 mul2x2matrix2x1vector(matrix_2x2 A, vector_2x1 B)
{
	vector_2x1 C;
	C.a = A.a*B.a+A.b*B.b;
	C.b = A.c*B.a+A.d*B.b;

	return C;
}

vector_2x1 add2x1vector(vector_2x1 A, vector_2x1 B)
{
	vector_2x1 C;
	C.a = A.a+B.a;
	C.b = A.b+B.b;

	return C;
}
