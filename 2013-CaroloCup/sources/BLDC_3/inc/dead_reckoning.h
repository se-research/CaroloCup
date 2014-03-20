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
 * dead_reckoning.h
 *
 *  Created on: 29 dec 2011
 *      Author: benjamin
 */

#ifndef DEAD_RECKONING_H_
#define DEAD_RECKONING_H_

#include "main.h"

/*
 * Paremeters
 */
/*
 * Calculated as:
 * arctan(axist_distance / turn_radius_at_maximum_steering_angle)
 */
#define DR_MC_MAX_SERVO_RAD		0.60
#define DR_AXIS_DISTANCE		260.0
#define DR_METER_TACHO_PULSE	(0.007)	// Meters per tachometer pulse

/*
 * Global variables
 */
volatile float dr_x_pos;
volatile float dr_y_pos;
volatile float dr_angle;

/*
 * Functions
 */
float dr_get_steering_angle();
void dr_update_position_angle(float *pos_x, float *pos_y, float *angle);
float dr_get_servo_pos(float steering_angle);
float dr_get_total_travel_distance();
float dr_get_abs_total_travel_distance();
void dr_reset_travel_counters();
void dr_update_func();

#endif /* DEAD_RECKONING_H_ */
