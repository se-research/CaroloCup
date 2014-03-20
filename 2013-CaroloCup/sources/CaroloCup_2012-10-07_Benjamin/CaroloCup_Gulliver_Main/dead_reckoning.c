/*
 * dead_reckoning.c
 *
 *  Created on: 29 dec 2011
 *      Author: benjamin
 */

#include "dead_reckoning.h"
#include "mc_driver.h"
#include "nine_dof.h"
#include "uart_ft.h"
#include <stdio.h>
#include <math.h>
#include <avr/pgmspace.h>
#include <util/delay.h>

/*
 * Variables
 */
double dr_compass_offset = 0;
double tot_dist_traveled = 0;
double abs_dist_traveled = 0;

double dr_get_steering_angle() {
	return ((double) mc_get_current_servo_pos() - (double)MC_SERVO_CENTER)
			* ((2.0*DR_MC_MAX_SERVO_RAD)
					/ ((double)MC_SERVO_LEFT - (double)MC_SERVO_RIGHT));
}

unsigned char dr_get_servo_pos(double steering_angle) {
	return (unsigned char)(steering_angle/((2.0*DR_MC_MAX_SERVO_RAD)
					/ ((double)MC_SERVO_LEFT - (double)MC_SERVO_RIGHT)) + (double)MC_SERVO_CENTER);
}

/*
 * Get distance traveled since last call of this function.
 *
 *
 * Returns:
 * Distance traveled in mm
 */
double dr_get_travel_distance() {
	const double dist = (double)mc_get_tacho(1) * MC_METER_TACHO_PULSE * 1000.0;
	tot_dist_traveled += dist;
	abs_dist_traveled += fabs(dist);
	return dist;
}

double dr_get_total_travel_distance() {
	return tot_dist_traveled;
}

double dr_get_abs_travel_distance() {
	return abs_dist_traveled;
}

void dr_update_position_angle(double *pos_x, double *pos_y, double *angle) {
	const double steering_angle = dr_get_steering_angle();
	const double travel_distance = dr_get_travel_distance();

	if (travel_distance == 0) {
		return;
	}

	// Avoid division by zero
	if (fabs(steering_angle) < 0.001) {
		*pos_x += cos(*angle) * travel_distance;
		*pos_y += sin(*angle) * travel_distance;
	} else {
		const double turn_rad_rear = DR_AXIS_DISTANCE / tan(steering_angle);
		double turn_rad_front = sqrt(
				DR_AXIS_DISTANCE * DR_AXIS_DISTANCE
						+ turn_rad_rear * turn_rad_rear);

		if (turn_rad_rear < 0) {
			turn_rad_front = -turn_rad_front;
		}
		const double angle_diff = (travel_distance * 2.0) / (turn_rad_rear + turn_rad_front);

		*pos_x += turn_rad_rear * (sin(*angle + angle_diff) - sin(*angle));
		*pos_y += turn_rad_rear * (cos(*angle - angle_diff) - cos(*angle));
		*angle += angle_diff;

		while (*angle > 2.0 * M_PI) {
			*angle -= 2.0 * M_PI;
		}

		while (*angle < 0) {
			*angle += 2.0 * M_PI;
		}

//		dr_compass_corr_angle(angle);
	}
}

double deltaDist()
{
	static double dist = 0;
	double deltadist = dr_get_total_travel_distance()-dist;
	dist = dr_get_total_travel_distance();
	return deltadist;
}

void dr_compass_reset_offset() {
	unsigned int tmp;

	if (nine_dof_read_compass(&tmp)) {
		dr_compass_offset = (double)tmp * (M_PI / 180.0);
	}
}

void dr_compass_corr_angle(double *angle) {
	unsigned int tmp;
	if (!nine_dof_read_compass(&tmp)) {
		return;
	}

	double comp_angle = (double)tmp * (M_PI / 180.0) - dr_compass_offset;

	if (comp_angle < 0) {
		comp_angle += 2.0 * M_PI;
	}

	if (fabs(*angle - comp_angle) > M_PI) {
		if (*angle < comp_angle) {
			*angle += 2.0 * M_PI;
		} else {
			comp_angle += 2.0 * M_PI;
		}
	}

	*angle = *angle * (1 - DR_COMPASS_WEIGHT) + comp_angle * DR_COMPASS_WEIGHT;

	if (*angle < 0) {
		*angle += 2.0 * M_PI;
	} else if (*angle > 2.0 * M_PI) {
		*angle -= 2.0 * M_PI;
	}
}





