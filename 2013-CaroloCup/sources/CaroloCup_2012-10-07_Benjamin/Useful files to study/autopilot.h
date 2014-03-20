/*
 * autopilot.h
 *
 *  Created on: 10 jan 2012
 *      Author: benjamin
 */
#ifndef AUTOPILOT_H_
#define AUTOPILOT_H_

#include "map.h"

/*
 * Parameters
 */
#define AP_DISTANCE_TRES		300.0
#define AP_RECORD_POINT_DIST	800.0
#define AP_RECORD_DEFAULT_SPEED	0.5
#define AP_RECORD_MAX_SPEED		3.2

/*
 * Functions
 */
void ap_init();
void ap_reset();
unsigned int ap_get_route_points(unsigned char route);
unsigned int ap_get_current_point();
RoutePoint* ap_get_route(unsigned char route);
unsigned char ap_get_current_route();
void ap_set_current_route(unsigned char route);
void ap_set_route_point(RoutePoint point, int route, int index);
void ap_clear_route(int route);
void ap_clear_all_routes();
void ap_change_lane_next(unsigned char dest_route, unsigned int from, unsigned int to);
void ap_change_lane_auto(unsigned char dest_route);
signed char ap_get_steering_angle_to_point(double current_x,
										double current_y,
										double current_angle,
										double goal_x,
										double goal_y,
										double *steering_angle,
										double *distance);
void ap_run(double pos_x, double pos_y, double angle);
void ap_set_current_route_point(double x, double y, double speed);
void ap_reset_record_route();
void ap_run_record_route();
void ap_speed_override_on();
void ap_speed_override_off();
void ap_speed_override(double speed);
unsigned char ap_get_current_area();
void ap_add_area(unsigned char index, double x, double y, double radius);
AreaPoint ap_get_area(unsigned char index);
unsigned char ap_get_point_progress();
void ap_clear_areas();

#endif /* AUTOPILOT_H_ */
