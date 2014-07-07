#include "Polynomial.h"
/*
double polynomial(
	    const double x,
	    const parameter_vector& params) {
	int deg = params.size()-1;
	double ret = 0;
	for (int i=0; i<deg+1; i++) {
		ret += params(i)*pow(x,i);
	}
	return ret;
}

// This function is the "residual" for a least squares problem.   It takes an input/output
// pair and compares it to the output of our model and returns the amount of error.  The idea
// is to find the set of parameters which makes the residual small on all the data pairs.
double residual (
    const std::pair<double, double>& data,
    const parameter_vector& params) {
    return polynomial(data.first, params) - data.second;
}

parameter_vector find_polynomial(std::vector<std::pair<double, double> > data_samples) {
	parameter_vector params;
	params = 1;
	// Find the polynomial x = p(y) that minimizes the squared error
	solve_least_squares_lm(objective_delta_stop_strategy(1e-7).be_verbose(),
			residual,
	        derivative(residual),
	        data_samples,
	        params);
	return params;
}

double get_min_distance(std::vector<std::pair<double, double> > data_samples,
		point_vector pt, double& x) {
	parameter_vector params;
	params = find_polynomial(data_samples);
	point_dist_model pdm(params, pt);
	double min_dist = find_min_single_variable(pdm, x, 0, 10);
	return min_dist;
}

double get_min_distance(parameter_vector& params, point_vector pt, double& x) {
	point_dist_model pdm(params, pt);
	double min_dist = find_min_single_variable(pdm, x, 0, 10);
	return min_dist;
}
*/
