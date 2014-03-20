/*
 * autopilot.h
 *
 *  Created on: 2 feb 2013
 *      Author: benjamin
 */

#ifndef AUTOPILOT_H_
#define AUTOPILOT_H_

// RoutePoint struct
typedef struct {
	float x;
	float y;
	float speed;
} RoutePoint;

// Functions
void ap_run();
void ap_clear();
void ap_set_running(int running);
void ap_add_point(RoutePoint *p);
void ap_get_steering_angle_to_point(float current_x,
		float current_y,
		float current_angle,
		float goal_x,
		float goal_y,
		float *steering_angle,
		float *distance);

#endif /* AUTOPILOT_H_ */
