/*
	Copyright 2012 Benjamin Vedder	benjamin@vedder.se

	This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
    */

/*
 * dead_reckoning.c
 *
 *  Created on: 29 dec 2011
 *      Author: benjamin
 */

#include "dead_reckoning.h"
#include "drive.h"
#include "mcpwm.h"
#include <stdio.h>
#include <math.h>

//  Private variables
static float tot_dist_traveled = 0;
static float abs_dist_traveled = 0;

// Global variables
volatile float dr_x_pos = 5.0;
volatile float dr_y_pos = 5.0;
volatile float dr_angle = 0.0;

// Private functions
static float get_travel_distance();

float dr_get_steering_angle() {
	return drive_get_current_steering_position() * DR_MC_MAX_SERVO_RAD;
}

float dr_get_servo_pos(float steering_angle) {
	return steering_angle / DR_MC_MAX_SERVO_RAD;
}

/*
 * Get distance traveled since last call of this function.
 *
 * Returns:
 * Distance traveled in mm
 */
static float get_travel_distance() {
	const float dist = (float)mcpwm_get_tachometer_value(1) * DR_METER_TACHO_PULSE * 1000.0;
	tot_dist_traveled += dist;
	abs_dist_traveled += fabsf(dist);
	return dist;
}

float dr_get_total_travel_distance() {
	return tot_dist_traveled;
}

float dr_get_abs_total_travel_distance() {
	return abs_dist_traveled;
}

void dr_reset_travel_counters() {
	tot_dist_traveled = 0;
	abs_dist_traveled = 0;
}

void dr_update_position_angle(float *pos_x, float *pos_y, float *angle) {
	const float steering_angle = dr_get_steering_angle();
	const float travel_distance = get_travel_distance();

	if (travel_distance == 0) {
		return;
	}

	// Avoid division by zero
	if (fabsf(steering_angle) < 0.0001) {
		*pos_x += cosf(*angle) * travel_distance;
		*pos_y += sinf(*angle) * travel_distance;
	} else {
		const float turn_rad_rear = DR_AXIS_DISTANCE / tanf(steering_angle);
		float turn_rad_front = sqrtf(
				DR_AXIS_DISTANCE * DR_AXIS_DISTANCE
						+ turn_rad_rear * turn_rad_rear);

		if (turn_rad_rear < 0) {
			turn_rad_front = -turn_rad_front;
		}
		const float angle_diff = (travel_distance * 2.0) / (turn_rad_rear + turn_rad_front);

		*pos_x += turn_rad_rear * (sinf(*angle + angle_diff) - sinf(*angle));
		*pos_y += turn_rad_rear * (cosf(*angle - angle_diff) - cosf(*angle));
		*angle += angle_diff;

		while (*angle > 2.0 * M_PI) {
			*angle -= 2.0 * M_PI;
		}

		while (*angle < 0) {
			*angle += 2.0 * M_PI;
		}
	}
}

/*
 * Call this function periodically to update the position
 * and angle
 */
void dr_update_func() {
	dr_update_position_angle((float*)&dr_x_pos, (float*)&dr_y_pos, (float*)&dr_angle);
}
