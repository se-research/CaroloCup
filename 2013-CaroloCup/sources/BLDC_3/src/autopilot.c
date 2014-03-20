/*
 * autopilot.c
 *
 *  Created on: 2 feb 2013
 *      Author: benjamin
 */

#include "autopilot.h"
#include "dead_reckoning.h"
#include "drive.h"
#include <math.h>

#define ROUTE_LEN			200
#define DIST_TRES			100

static volatile RoutePoint path[ROUTE_LEN];
static volatile int point_prev = 0;
static volatile int point_dest = 0;
static volatile int is_running = 0;

// Private functions
static float calc_point_point_distance(float x1, float y1, float x2, float y2);

void ap_get_steering_angle_to_point(float current_x,
		float current_y,
		float current_angle,
		float goal_x,
		float goal_y,
		float *steering_angle,
		float *distance) {

	const float dist = sqrt((goal_x-current_x)*(goal_x-current_x) + (goal_y-current_y)*(goal_y-current_y));
	*distance = dist;
	const float gamma=current_angle - atan2f((goal_y-current_y), (goal_x-current_x));
	const float dx = dist * cosf(gamma);
	const float dy = dist * sinf(gamma);

	if (dy == 0.0) {
		*steering_angle = 0;
		return;
	}

	float circle_radius = -(dx * dx + dy * dy) / (2.0 * dy);
	*steering_angle = atan(DR_AXIS_DISTANCE / circle_radius);
}

void ap_add_point(RoutePoint *p) {
	point_dest++;
	if (point_dest >= ROUTE_LEN) {
		point_dest = 0;
	}

	path[point_dest].x = p->x;
	path[point_dest].y = p->y;
	path[point_dest].speed = p->speed;
}

void ap_clear() {
	point_prev = 0;
	point_dest = 0;
}

void ap_set_running(int running) {
	is_running = running;
}

void ap_run() {
	if (point_prev == point_dest || !is_running) {
		return;
	}

	const float pos_x = dr_x_pos;
	const float pos_y = dr_y_pos;
	const float angle = dr_angle;
	float distance;
	float steering_angle;

	// Pointer to previous point
	RoutePoint *p_prev = (RoutePoint*)&path[point_prev];

	// Point in front of previous point
	int point_next = point_prev + 1;
	if (point_next >= ROUTE_LEN) {
		point_next = 0;
	}
	RoutePoint *p_next = (RoutePoint*)&path[point_next];

	// Compute steering angle to that point
	ap_get_steering_angle_to_point(pos_x, pos_y, angle, p_next->x,
			p_next->y, &steering_angle, &distance);

	// Compute speed
	const float dist_previous = calc_point_point_distance(pos_x, pos_y,
			p_prev->x, p_prev->y);

	const float fract_prev = distance / (dist_previous + distance);

	float speed = (p_prev->speed * fract_prev)
						+ (p_next->speed * (1.0 - fract_prev));

	float servo_pos = dr_get_servo_pos(steering_angle);

	if (distance < DIST_TRES) {
		// Close enough to next point... Increment prevoius point
		point_prev++;
		if (point_prev >= ROUTE_LEN) {
			point_prev = 0;
		}
	} else {
		// Drive with calculated steering angle
		drive_go(speed, servo_pos);
	}

	if (point_prev == point_dest) {
		drive_go(0, servo_pos);
	}
}

static float calc_point_point_distance(float x1, float y1, float x2, float y2) {
	return sqrtf((x2-x1) * (x2-x1) + (y2-y1)*(y2-y1));
}
