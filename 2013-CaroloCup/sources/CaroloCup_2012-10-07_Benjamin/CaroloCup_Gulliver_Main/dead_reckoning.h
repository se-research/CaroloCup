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
#define DR_MC_MAX_SERVO_RAD		0.40
#define DR_DEV_PER_DIST			0.015
#define DR_AXIS_DISTANCE		330.0
#define DR_RCM_POS_WEIGHT		0.3
#define DR_RCM_ANGLE_WEIGHT		0.0001
#define DR_RCM_MAX_CORR			100.0
#define DR_COMPASS_WEIGHT		0.1

// RCM antenna offset according to the center of rear wheels
#define RCM_OFS_ANGLE	0.19739*M_PI
#define RCM_OFS_DIST	357.0


/*
 * Functions
 */
double dr_get_steering_angle();
double dr_get_travel_distance();
void dr_update_position_angle(double *pos_x, double *pos_y, double *angle);
void dr_compass_reset_offset();
void dr_compass_corr_angle(double *angle);
unsigned char dr_get_servo_pos(double steering_angle);
double dr_get_total_travel_distance();
double dr_get_abs_travel_distance();
double deltaDist();

#endif /* DEAD_RECKONING_H_ */
