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
 * mc_pwm.h
 *
 *  Created on: 13 okt 2012
 *      Author: benjamin
 */

#ifndef MCPWM_H_
#define MCPWM_H_

typedef enum {
   MC_STATE_OFF = 0,
   MC_STATE_DETECTING,
   MC_STATE_STARTING,
   MC_STATE_RUNNING
} mc_state;

typedef enum {
	PWM_MODE_NONSYNCHRONOUS_LOSW = 0,
	PWM_MODE_NONSYNCHRONOUS_HISW,
	PWM_MODE_SYNCHRONOUS,
	PWM_MODE_BIPOLAR
} mc_pwm_mode;

// Functions
void mcpwm_init();
void mcpwm_set_duty(float dutyCycle);
int mcpwm_get_comm_step();
float mcpwm_get_duty_cycle();
float mcpwm_get_rpm();
void mcpwm_use_pid(int use_pid);
void mcpwm_set_pid_speed(float rpm);
float mcpwm_get_kv();
float mcpwm_get_kv_filtered();
int mcpwm_get_tachometer_value(int reset);
float mcpwm_get_tot_current_filtered();
void mcpwm_set_detect();
int mcpwm_get_detect_top();
mc_state mcpwm_get_state();
signed int mcpwm_read_hall_phase();

// Interrupt handlers
void mcpwm_time_int_handler();
void mcpwm_comm_int_handler();
void mcpwm_adc_inj_int_handler();
void mcpwm_adc_int_handler();

// External variables
extern volatile uint16_t ADC_Value[];
extern volatile int ADC_curr_norm_value[];

/*
 * Parameters
 */
#define MCPWM_SWITCH_FREQUENCY			40000	// Switching frequency in HZ
#define MCPWM_DEAD_TIME_CYCLES			80		// Dead time
#define MCPWM_PWM_MODE					PWM_MODE_SYNCHRONOUS // Default PWM mode
#define MCPWM_MIN_DUTY_CYCLE			0.03	// Minimum duty cycle
#define MCPWM_MAX_DUTY_CYCLE			0.94	// Maximum duty cycle
#define MCPWM_AVG_COM_RPM				6		// Number of commutations to average RPM over
#define MCPWM_NUM_POLES					2		// Motor pole number (for RPM calculation)
#define MCPWM_HALL_SENSOR_ORDER			5		// Order in which hall sensors are connected

// Sensorless settings
#define MCPWM_IS_SENSORLESS				1		// Use sensorless commutation
#define MCPWM_MAX_COMM_START_DIFF		10		// The lower the number, the more picky the the closed loop detector
#define MCPWM_MIN_CLOSED_RPM			500		// Switch to open loop below this RPM
#define MCPWM_MAX_CLOSED_RPM			120000	// Switch to open loop above this RPM
#define MCPWM_START_COMM_TIME_MS		15		// Commutation time during startup in msecs
#define MCPWM_START_DUTY_CYCLE			0.3		// Startup duty cycle
#define MCPWM_MIN_START_STEPS			1		// Minimum steps to run in open loop
#define MCPWM_CLOSED_STARTPWM_COMMS		5		// Run at least this many commutations in closed loop with start duty cycle
#define MCPWM_CYCLE_INT_LIMIT			110.0	// Flux integrator limit
#define MCPWM_VZERO_FACT				1.0		// Virtual zero adjustment

// PID parameters
#define MCPWM_PID_TIME_K				0.001	// Pid controller sample time in seconds
#define MCPWM_PID_KP					0.0002	// Proportional gain
#define MCPWM_PID_KI					0.010	// Integral gain
#define MCPWM_PID_KD					0.0		// Derivative gain
#define MCPWM_PID_MIN_RPM				1000.0	// Minimum allowed RPM

// Misc settings
#define MCPWM_ADC_CHANNELS				12

/*
 * Outrunner parameters:
 * MCPWM_USE_BIPOLAR_PWM: 0
 * MCPWM_START_COMM_TIME_MS: 6
 * MCPWM_START_DUTY_CYCLE. 0.2
 * MCPWM_CYCLE_INT_LIMIT: 90
 * MCPWM_PID_KP: 0.0002
 * MCPWM_PID_KI: 0.010
 * MCPWM_VZERO_FACT: 1.0
 */

/*
 * Coreless inrunner parameters:
 * MCPWM_USE_BIPOLAR_PWM: 0
 * MCPWM_START_COMM_TIME_MS: 8
 * MCPWM_START_DUTY_CYCLE. 0.2
 * MCPWM_CYCLE_INT_LIMIT: 90
 * MCPWM_PID_KP: 0.0002
 * MCPWM_PID_KI: 0.010
 * MCPWM_VZERO_FACT: 0.9
 */

/*
 * Carolo Cup parameters:
 * MCPWM_USE_BIPOLAR_PWM: 1
 * MCPWM_START_COMM_TIME_MS: 15
 * MCPWM_START_DUTY_CYCLE. 0.2
 * MCPWM_CYCLE_INT_LIMIT: 80
 * MCPWM_PID_KP: 0.0004
 * MCPWM_PID_KI: 0.020
 * MCPWM_VZERO_FACT: 1.0
 */

#endif /* MC_PWM_H_ */
