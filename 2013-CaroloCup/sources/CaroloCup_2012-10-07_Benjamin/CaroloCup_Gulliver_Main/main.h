/*
 * main.h
 *
 *  Created on: 8 mar 2011
 *      Author: benjamin
 */

#ifndef MAIN_H_
#define MAIN_H_


/*
 * Variables
 */
extern volatile signed int mv_batt;
extern volatile double dr_x_pos;
extern volatile double dr_y_pos;
extern volatile double dr_angle;
extern volatile double dr_sigma;
extern volatile unsigned long clk;

/*
 * Localization
 */
#define MAIN_DEFAULT_X	1000.0
#define MAIN_DEFAULT_Y	1000.0

/*
 * IR
 */
#define IR_FWD		0x18
#define IR_REV		0x1C
#define IR_LEFT		0x19
#define IR_RIGHT	0x1B

/*
 * LEDs
 */
#define LED1_ON		PORTC_OUTSET = _BV(0);
#define LED1_OFF	PORTC_OUTCLR = _BV(0);
#define LED1_TGL	PORTC_OUTTGL = _BV(0);
#define LED1_IS_ON	(PORTC_IN & _BV(0))

#define LED2_ON		PORTC_OUTSET = _BV(1);
#define LED2_OFF	PORTC_OUTCLR = _BV(1);
#define LED2_TGL	PORTC_OUTTGL = _BV(1);
#define LED2_IS_ON	(PORTC_IN & _BV(1))

/*
 * Network byte order macros
 */
#define htons(A) ((((unsigned short)(A) & 0xff00) >> 8) | \
		(((unsigned short)(A) & 0x00ff) << 8))

#define htonl(A) ((((unsigned long)(A) & 0xff000000) >> 24) | \
		(((unsigned long)(A) & 0x00ff0000) >> 8)  | \
		(((unsigned long)(A) & 0x0000ff00) << 8)  | \
		(((unsigned long)(A) & 0x000000ff) << 24))

#define ntohs  htons
#define ntohl  htonl

/*
 * Other macros
 */
#define max( a, b ) ( ((a) > (b)) ? (a) : (b) )
#define min( a, b ) ( ((a) < (b)) ? (a) : (b) )

// Number of distance sensors connected to the sensor board.
#define DISTANCE_SENSORS_NUM	2

// Sensor distances in centimeters
#define SENS_FAR_DIST		85
#define SENS_CLOSE_DIST		25

// Maximum speed in m/s
#define MC_MAX_SPEED 2.0

/*
 * Functions
 */
void set_mc_timeout(unsigned int ms);
void updateClk(unsigned long newclock);

#endif /* MAIN_H_ */
