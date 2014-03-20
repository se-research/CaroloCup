/*
 * mc_driver.h
 *
 *  Created on: 13 dec 2011
 *      Author: benjamin
 */

#ifndef MC_DRIVER_H_
#define MC_DRIVER_H_

/*
 * defines
 */
#define MC_PWM_MAX				3600
#define MC_SERVO_OFFSET			0//20
#define MC_SERVO_LEFT			185
#define MC_SERVO_RIGHT			41
#define MC_SERVO_CENTER			113
//#define MC_SERVO_LEFT			70
//#define MC_SERVO_RIGHT		214
//#define MC_SERVO_CENTER		142
#define MC_METER_TACHO_PULSE	((1.0 / 441.75) /* * 1.05*/)	// Meters per tachometer pulse

/*
 * MC Value struct
 */
typedef struct {
	double v_batt;
	double v_log;
	double temp;
	double speed;	// Speed in meters/second
} MC_VALUES;

/*
 * Functions
 */
void mc_set_motor_power(unsigned int power_in, unsigned char direction, unsigned char isPid);
void mc_set_servo_position(unsigned char pos, unsigned char speed);
unsigned char mc_get_values(unsigned char* buffer);
signed long mc_get_tacho(unsigned char reset_tacho);
unsigned char mc_get_current_servo_pos();
void mc_set_speed(double speed);
unsigned char mc_get_values_struct(MC_VALUES *values);
void mc_set_override(unsigned char override);
unsigned char mc_get_rpm(double *rpm);
unsigned char mc_get_speed(double *speed);
void mc_set_speed_no_store(double speed);
double mc_get_last_speed();

#endif /* MC_DRIVER_H_ */
