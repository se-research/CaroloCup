/*
 * autopilot.c
 *
 *  Created on: 10 jan 2012
 *      Author: benjamin
 */

#include "autopilot.h"
#include "uart_rcm.h"
#include "dead_reckoning.h"
#include "mc_driver.h"
#include "calculations.h"
#include "rf_comm.h"
#include "map.h"

#include <avr/pgmspace.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

/*
 * Variables
 */
unsigned int ap_current_route;
unsigned int ap_current_point;
double ap_record_offset;
unsigned int ap_record_current_point;
volatile unsigned char ap_override_speed;
volatile double ap_override_speed_val;
unsigned char ap_change_lane;
unsigned int ap_change_lane_from;
unsigned int ap_change_lane_to;
unsigned char ap_change_lane_dest_route;
RoutePoint ap_previous_point;
AreaPoint ap_areas[AP_AREA_NUM];
unsigned char ap_point_progress;

void ap_init() {
//	for (unsigned char i = 0;i < AP_ROUTE_NUM;i++) {
//		ap_route_points[i] = 0;
//	}
//
//	for (unsigned char i = 0;i < AP_AREA_NUM;i++) {
//		memset(&ap_areas[i], 0, sizeof(AreaPoint));
//	}

	ap_current_point = 0;
	ap_record_offset = 0;
	ap_current_route = 0;
	ap_record_current_point = 0;
	ap_change_lane = 0;
	ap_change_lane_from = 0;
	ap_change_lane_to = 0;
	ap_change_lane_dest_route = 0;
	ap_previous_point.speed = 0.7;
	ap_previous_point.x = 0;
	ap_previous_point.y = 0;
	ap_point_progress = 0;
	ap_override_speed = 0;
	ap_override_speed_val = 0.0;

	/*
	 * Hard-code map for now
	 */
//	ap_route_points[0] = 31;
//	double center_x=6800;
//	double center_y=8800;
//	double radius = 3500;
//	for(unsigned char i=0;i<ap_route_points[0];i++)
//	{
//		ap_route[0][i].x=center_x+radius*cos((double)i*2.0*M_PI/(double)ap_route_points[0]);
//		ap_route[0][i].y=center_y+radius*sin((double)i*2.0*M_PI/(double)ap_route_points[0]);
//		ap_route[0][i].speed=0.8;
//	}
//
//	ap_route_points[1] = 31;
//	center_x=6000;
//	center_y=5000;
//	radius = 3500;
//	for(unsigned char i=0;i<ap_route_points[1];i++)
//	{
//		ap_route[1][i].x=center_x+radius*cos((double)i*2.0*M_PI/(double)ap_route_points[1]);
//		ap_route[1][i].y=center_y+radius*sin((double)i*2.0*M_PI/(double)ap_route_points[1]);
//		ap_route[1][i].speed=0.8;
//	}
//
//	// Add area
//	ap_add_area(0, 4700, 10000, 1500);
//	ap_add_area(1, 10100, 4800, 1500);
}

/*
 * Get the number of valid points in the current route
 */
unsigned int ap_get_route_points(unsigned char route) {
	return ap_route_points[route];
}

unsigned int ap_get_current_point() {
	return ap_current_point;
}

unsigned char ap_get_current_route() {
	return ap_current_route;
}

void ap_set_current_route(unsigned char route) {
	if (route < AP_ROUTE_NUM) {
		ap_current_route = route;
	}
}

void ap_clear_route(int route) {
	if (route < AP_ROUTE_NUM) {
		ap_route_points[route] = 0;
	}
}

void ap_clear_all_routes() {
	for (unsigned char i = 0;i < AP_ROUTE_NUM;i++) {
		ap_route_points[i] = 0;
	}
}

void ap_change_lane_next(unsigned char dest_route, unsigned int from, unsigned int to) {
	ap_change_lane_dest_route = dest_route;
	ap_change_lane_from = from;
	ap_change_lane_to = to;
	ap_change_lane = 1;
}

/*
 * Try to automatically change lane...
 */
void ap_change_lane_auto(unsigned char dest_route) {
	if (dest_route >= AP_ROUTE_NUM || dest_route == ap_current_route) {
		return;
	}

	// Find closest point
	int closest_index = 0;
	double dist = 0;
	double closest_dist = 99999999999.0;
	for (unsigned char i = 0;i < ap_route_points[dest_route];i++) {
		 dist = calc_point_point_distance(dr_x_pos, dr_y_pos, ap_route[dest_route][i].x,
					ap_route[dest_route][i].y);
		 if (dist < closest_dist) {
			 closest_dist = dist;
			 closest_index = i;
		 }
	}

	// Update previous point
	ap_previous_point = ap_route[ap_current_route][ap_current_point];

	// Take point after closest point
	closest_index++;
	if (closest_index >= ap_route_points[dest_route]) {
		closest_index = 0;
	}

	// Take another point after closest point
	closest_index++;
	if (closest_index >= ap_route_points[dest_route]) {
		closest_index = 0;
	}

	ap_current_route = dest_route;
	ap_current_point = closest_index;
}

/*
 * Get a pointer to the current route
 */
RoutePoint* ap_get_route(unsigned char route) {
	if (route < AP_ROUTE_NUM) {
		return (RoutePoint*)&ap_route[route];
	} else {
		return 0;
	}
}

void ap_reset() {
	ap_current_point = 0;
}

void ap_set_route_point(RoutePoint point, int route, int index) {
	if (index < AP_ROUTE_LEN && route < AP_ROUTE_NUM) {
		if (index == 0) {
			ap_route_points[route] = 1;
		} else if (ap_route_points[route] <= index) {
			ap_route_points[route] = index + 1;
		}

		ap_route[route][index] = point;
	}
}

signed char ap_get_steering_angle_to_point(double current_x,
										double current_y,
										double current_angle,
										double goal_x,
										double goal_y,
										double *steering_angle,
										double *distance) {
	const double D=sqrt((goal_x-current_x)*(goal_x-current_x) + (goal_y-current_y)*(goal_y-current_y));
	*distance = D;
	const double gamma=current_angle - atan2((goal_y-current_y), (goal_x-current_x));
	const double dx = D * cos(gamma);
	const double dy = D * sin(gamma);

	if (dy == 0.0) {
		*steering_angle = 0;
		return 0;
	}

	double circle_radius = -(dx * dx + dy * dy) / (2.0 * dy);

	/*
	 * Add correction if arc is much longer than total distance
	 */
	double angle_correction = 1.03 + (pow((D / 1000.0), 2.1) * 0.05);
	if (angle_correction > 5.0) {
		angle_correction = 5.0;
	}

	*steering_angle = atan(DR_AXIS_DISTANCE / circle_radius) * angle_correction;

	return 0;
}

void ap_run(double pos_x, double pos_y, double angle) {
	static double closest_distance = 99999999999.0;
	double distance, steering_angle;
	unsigned char servo_pos;
	static unsigned char max_steering = 0;

	distance = calc_point_point_distance(pos_x, pos_y, ap_route[ap_current_route][ap_current_point].x,
			ap_route[ap_current_route][ap_current_point].y);

	if (distance < AP_DISTANCE_TRES || (max_steering > 25 && distance < 2000.0)) {
		max_steering = 0;
		// Update previous point
		ap_previous_point = ap_route[ap_current_route][ap_current_point];

		ap_current_point++;
		if (ap_current_point >= ap_route_points[ap_current_route]) {
			ap_current_point = 0;
		}
		closest_distance = 99999999999.0;

		// Change lane here
		if (ap_change_lane && ap_change_lane_from == ap_current_point &&
				ap_current_route != ap_change_lane_dest_route) {
			// Update previous point
			ap_previous_point = ap_route[ap_current_route][ap_current_point];

			ap_current_route = ap_change_lane_dest_route;
			ap_current_point = ap_change_lane_to;
			ap_change_lane = 0;
		}
	}

	if (ap_get_steering_angle_to_point(pos_x, pos_y, angle, ap_route[ap_current_route][ap_current_point].x,
			ap_route[ap_current_route][ap_current_point].y, &steering_angle, &distance) == 0) {
		if (steering_angle >= DR_MC_MAX_SERVO_RAD) {
			servo_pos = MC_SERVO_LEFT;
			max_steering++;
		} else if (steering_angle <= -DR_MC_MAX_SERVO_RAD) {
			servo_pos = MC_SERVO_RIGHT;
			max_steering++;
		} else {
			servo_pos = dr_get_servo_pos(steering_angle);
			max_steering = 0;
		}

		if (distance < closest_distance) {
			closest_distance = distance;
		}

		const double dist_previous = calc_point_point_distance(pos_x, pos_y,
				ap_previous_point.x, ap_previous_point.y);
		const double fract_prev = distance / (dist_previous + distance);

		ap_point_progress = (unsigned char)((1.0 - fract_prev) * 100.0);

		double speed;

		if (ap_override_speed) {
			speed = ap_override_speed_val;
		} else {
			speed = (ap_previous_point.speed * fract_prev) +
					(ap_route[ap_current_route][ap_current_point].speed * (1.0 - fract_prev));
		}

		mc_set_servo_position(servo_pos, 0);
		mc_set_speed(speed);
	} else {
		return;
	}
}

void ap_speed_override_on() {
	ap_override_speed = 1;
}

void ap_speed_override_off() {
	ap_override_speed = 0;
}

void ap_speed_override(double speed) {
	ap_override_speed_val = speed;
}

void ap_set_current_route_point(double x, double y, double speed) {
	ap_route[ap_current_route][ap_record_current_point].x = x;
	ap_route[ap_current_route][ap_record_current_point].y = y;
	ap_route[ap_current_route][ap_record_current_point].speed = speed;
}

void ap_reset_record_route() {
	ap_record_offset = dr_get_abs_travel_distance();
	ap_record_current_point = 0;
}

void ap_run_record_route() {
	if ((dr_get_abs_travel_distance() - ap_record_offset) >= AP_RECORD_POINT_DIST) {
		ap_record_offset += AP_RECORD_POINT_DIST;
		double speed;

		ap_route[ap_current_route][ap_record_current_point].x = dr_x_pos;
		ap_route[ap_current_route][ap_record_current_point].y = dr_y_pos;

		if (mc_get_speed(&speed)) {
			if (speed > AP_RECORD_MAX_SPEED) {
				speed = AP_RECORD_MAX_SPEED;
			} else if (speed < -AP_RECORD_MAX_SPEED) {
				speed = -AP_RECORD_MAX_SPEED;
			}
			ap_route[ap_current_route][ap_record_current_point].speed = speed;
		} else {
			ap_route[ap_current_route][ap_record_current_point].speed = AP_RECORD_DEFAULT_SPEED;
		}

		if (ap_record_current_point < (AP_ROUTE_LEN - 1)) {
			ap_record_current_point++;
		}

		ap_route_points[ap_current_route] = ap_record_current_point;
	}
}

unsigned char ap_get_current_area() {
	unsigned char retval = 250;
	double dist;

	for (unsigned char i = 0;i < AP_AREA_NUM;i++) {
		if (ap_areas[i].radius == 0) {
			continue;
		}

		dist = calc_point_point_distance(dr_x_pos, dr_y_pos, ap_areas[i].x, ap_areas[i].y);

		if (dist < ap_areas[i].radius) {
			retval = i;
			break;
		}
	}

	return retval;
}

void ap_add_area(unsigned char index, double x, double y, double radius) {
	if (index >= AP_AREA_NUM) {
		return;
	}

	ap_areas[index].x = x;
	ap_areas[index].y = y;
	ap_areas[index].radius = radius;
}

unsigned char ap_get_point_progress() {
	return ap_point_progress;
}

AreaPoint ap_get_area(unsigned char index) {
	AreaPoint res = {0, 0, 0};

	if (index < AP_AREA_NUM) {
		res = ap_areas[index];
	}

	return res;
}

void ap_clear_areas() {
	for (unsigned char i = 0;i < AP_AREA_NUM;i++) {
		ap_areas[i].radius = 0;
	}
}
