#ifndef POLYNOMIAL_H
#define POLYNOMIAL_H
/*
#include "3rdParty/libdb-4.6.21/dlib/optimization.h"
#include <dlib/matrix.h>
#include <iostream>
#include <vector>

using namespace std;
using namespace dlib;

// ----------------------------------------------------------------------------------------

typedef matrix<double,2,1> point_vector;
typedef matrix<double,3,1> parameter_vector;
typedef matrix<double,1,0> row_vector;
typedef matrix<double,0,1> column_vector;

class point_dist_model {
private:
	double xp, yp;
	int deg;
	parameter_vector params;
public:
	point_dist_model(parameter_vector &params, point_vector& pt) {
		this->params = params;
		deg = params.size()-1;
		xp = pt(0);
		yp = pt(1);
	}
	double operator() (double x) const {
		return pow(x-xp,2) + pow(polynomial(x, params)-yp,2);
	}

};

// ----------------------------------------------------------------------------------------

double polynomial(const double x, const parameter_vector& params);
double residual(const std::pair<double, double>& data, const parameter_vector& params);
parameter_vector find_polynomial(std::vector<std::pair<double, double> > data_samples);
double get_min_distance(std::vector<std::pair<double, double> > data_samples, point_vector pt, double& x);
double get_min_distance(parameter_vector& params, point_vector pt, double& x);
*/
#endif
