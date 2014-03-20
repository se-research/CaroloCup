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
 * drive.h
 *
 *  Created on: 3 nov 2012
 *      Author: benjamin
 */

#ifndef DRIVE_H_
#define DRIVE_H_

// Parameters
#define DRIVE_STEERING_DELTA		110		// Servo limit in each direction
#define DRIVE_MIN_MPS				0.5
#define DRIVE_MAX_MPS				5.0
#define DRIVE_GEAR_RATIO			4.7
#define DRIVE_WHEEL_DIAMETER		0.065
#define DRIVE_INVERT_STEERING		1

// Functions
void drive_go(float mps, float steering);
float drive_get_current_speed();
float drive_get_current_steering_position();

#endif /* DRIVE_H_ */
