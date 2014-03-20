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
 * drive.c
 *
 *  Created on: 3 nov 2012
 *      Author: benjamin
 */

#include "drive.h"
#include "servo.h"
#include "mcpwm.h"

#include <math.h>
#include <stdint.h>

/*
 * mps:
 * How many meters/second to set the speed controller to.
 * A negative value means go reverse.
 *
 * steering:
 * The servo position to use. Range: -1.0 ... 0.0 ,,, 1.0
 * 0.0: Center
 * -1.0: Full right
 * 1.0: Full left
 *
 */
void drive_go(float mps, float steering) {

	// Check ranges
	if (fabsf(mps) < DRIVE_MIN_MPS) {
		mps = 0;
	} else if (fabsf(mps) > DRIVE_MAX_MPS) {
		if (mps > 0.0) {
			mps = DRIVE_MAX_MPS;
		} else {
			mps = -DRIVE_MAX_MPS;
		}
	}

	if (steering > 1.0) {
		steering = 1.0;
	} else if (steering < -1.0) {
		steering = -1.0;
	}

	float rpm = (mps * DRIVE_GEAR_RATIO * 60)
			/ (DRIVE_WHEEL_DIAMETER * M_PI);

	signed short steering_value = (signed short) roundf(
			(steering * (float) DRIVE_STEERING_DELTA));

//	mcpwm_use_pid(1);
//	mcpwm_set_pid_speed(rpm);

// TODO: Fix this!
	mcpwm_use_pid(0);
	mcpwm_set_duty(mps / 10.0);

#if DRIVE_INVERT_STEERING
	servos[0].pos = -steering_value;
#else
	servos[0].pos = steering_value;
#endif
}

float drive_get_current_speed() {
	return (mcpwm_get_rpm() / 60 / DRIVE_GEAR_RATIO) * DRIVE_WHEEL_DIAMETER
			* M_PI;
}

float drive_get_current_steering_position() {
#if DRIVE_INVERT_STEERING
	float steering = (float)(-servos[0].pos) / DRIVE_STEERING_DELTA;
#else
	float steering = (float)servos[0].pos / DRIVE_STEERING_DELTA;
#endif

	if (steering > 1.0) {
		steering = 1.0;
	} else if (steering < -1.0) {
		steering = -1.0;
	}

	return steering;
}
