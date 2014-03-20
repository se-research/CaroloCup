/*
	Copyright 2009-2010 Benjamin Vedder	vedder87@gmail.com
	
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
 * servo.h
 *
 *  Created on: 2009-apr-25
 *      Author: Benjamin
 *
 *
 * Changelog:
 * 2011-12-08
 * - Renamed functions
 * - Fixed pulse issue when pulses are close together
 *
 * 2011-07-17
 * - Changed some variables to volatile.
 * - Added another command.
 *
 * 2010-11-13
 * - Calculate the servo timings independent of F_CPU.
 * - Cleanup
 *
 */

#ifndef SERVO_H_
#define SERVO_H_

#include <avr/io.h>
#include <stdlib.h>
#include <avr/interrupt.h>
#include <avr/pgmspace.h>

// Change these parameters
#define SERVOS_NUM	2		// Number of servos to use

#define S_PRESCALE		8L
/*
 * Servo timing (in uS)
 *
 * The default signal to drive servos looks like this:
 *
 * ----1000us----|----1000us----|---20000us----
 * _____________________________
 *               |______________|_____...______
 *
 * -S_STARTPULSE-|--S_PULSELEN--|--S_COOLDOWN--
 *
 * And the default parameters are the following
 * #define S_STARTPULSE		1000L
 * #define S_PULSELEN		1000L
 * #define S_COOLDOWN		20000L
 *
 * You can experiment with these to make your servo move further.
 * For mg995 these can be WAY out of spec.
 *
 * Note that S_PULSELEN is not accurate at all for low F_CPU. However,
 * it will be rounded up to the nearest possible value (hence the strange
 * calculation below)
 *
 */
#define S_STARTPULSE	1000L
#define S_PULSELEN		1000L
#define S_COOLDOWN		15000L

/*
 * Dynamic servo parameters
 * Calculated from F_CPU
 */
#define SERVO_START_OFFSET		(F_CPU / ((1000000L * S_PRESCALE) / S_STARTPULSE))
#define SERVO_CPU_FACTOR		((F_CPU + (((1000000L * S_PRESCALE) / S_PULSELEN) * 256L) - 1L) / (((1000000L * S_PRESCALE) / S_PULSELEN) * 256L)) // Round up
#define SERVO_COOLDOWN_FACTOR	(F_CPU / ((1000000L * S_PRESCALE) / S_COOLDOWN))

/*
 * Compile with commands to mode servos with a specified speed
 * to s specified position interrupt driven. Enabling this will
 * use some extra ram and a few bytes of flash.
 */
#define USE_COMMANDS 1

/*
 * Calculate how many clock cycles it takes to generate PWM.
 */
#define TEST_CYCLE_TIME 0

#if TEST_CYCLE_TIME
extern volatile unsigned int restart_cnt;
extern volatile unsigned int interrupt_cnt;

#define get_restart_cycles()	(restart_cnt)
#define get_interrupt_cycles()	(interrupt_cnt)
#endif

// Some servo speed defines
// TODO

typedef struct {
	volatile unsigned char pos;
	volatile uint8_t *port;
	volatile uint8_t *dir;
	volatile uint8_t pin;
} SERVO;

#if USE_COMMANDS
typedef struct {
	volatile signed char active;
	volatile unsigned char pos;
	volatile unsigned char speed;
	volatile unsigned char last;
} SERVO_CMD;

extern volatile signed char cmd_seq_running;
extern volatile unsigned int cmd_ptr;
extern volatile PGM_P cmd_seq;

/*
 * The number of servo cycles to wait for each time unit in the wait command.
 *
 * The wait time can be calculated with:
 * (S_STARTPULSE + S_PULSELEN + S_COOLDOWN) * CMD_WAIT_FACTOR
 *
 */
#define CMD_WAIT_FACTOR		1

/*
 * Servo commands.
 */

/*
 * Move servo to given position with given speed.
 *
 * Param 1: Servo.
 * Param 2: Position.
 * Param 3: Speed. 0 for max speed.
 */
#define CMD_MOVE_SERVO		0

/*
 * Wait for a while. See configuration for more info.
 *
 * Param 1: Time to wait
 */
#define CMD_WAIT			1

/*
 * Wait for servo to be ready.
 *
 * Param 1: Servo to wait for.
 */
#define CMD_WAIT_SERVO		2

/*
 * Wait for all servo commands to get ready.
 */
#define CMD_WAIT_ALL_SERVOS	3

/*
 * Stop servo driver.
 */
#define CMD_STOP_DRIVER		4

/*
 * End of command.
 */
#define CMD_STOP_CMDS		5

/*
 * Restart this command sequence
 */
#define CMD_RESTART			6
#endif

extern volatile SERVO servos[SERVOS_NUM];

void servo_init(void);
void servo_stop_driver(void);
unsigned char servo_driver_is_active();

#if USE_COMMANDS
void servo_move(unsigned char servo, unsigned char position, unsigned char speed);
void servo_run_cmds(PGM_P cmds);
void servo_stop_cmds(void);
#endif

#endif /* SERVO_H_ */
