/*
 * ir_jvc.c
 *
 *  Created on: 25 dec 2009
 *      Author: benjamin
 *
 * JVC ir protocol implemented using interrupts on int0 or int1.
 *
 * TODO: implement repeat counting with redundancy.
 *
 */

#include "ir.h"

#ifdef	IR_JVC_PROTOCOL

#include <avr/io.h>
#include <avr/interrupt.h>

/*
 * NEC Timing (in us)
 */
#define STARTBIT_LEN	8400L
#define ZERO_LEN		1050L
#define ONE_LEN			2100L
#define CMD_SPACE_LEN	4200L
#define PERIODE_MIN_LEN	20000L
#define PERIODE_MAX_LEN	70000L

/*
 * Timer constants
 */
#define MAX_ERROR		40	// Maximum timing error in timer ticks
#define TIMER_PRESCALE	64

#define STARTBIT_CNT	((STARTBIT_LEN * (F_CPU / 1000000L)) / TIMER_PRESCALE)	// 1050
#define ZERO_CNT		((ZERO_LEN * (F_CPU / 1000000L)) / TIMER_PRESCALE)		// 131.25
#define ONE_CNT			((ONE_LEN * (F_CPU / 1000000L)) / TIMER_PRESCALE)		// 262.5
#define CMD_SPACE_CNT	((CMD_SPACE_LEN * (F_CPU / 1000000L)) / TIMER_PRESCALE)	// 525
#define PERIODE_MIN_CNT	((PERIODE_MIN_LEN * (F_CPU / 1000000L)) / TIMER_PRESCALE)	// ...
#define PERIODE_MAX_CNT	((PERIODE_MAX_LEN * (F_CPU / 1000000L)) / TIMER_PRESCALE)	// ...

/*
 * Interrupt stuff
 */
#define	IR_STARTBIT		0
#define IR_SPACE		1
#define	IR_ADDR			2
#define	IR_CMD			3

/*
 * Edges
 */
#define	FALLING		0
#define	RISING		1

/*
 * Variables
 */
static volatile signed char status;
static volatile signed char edge;
static volatile signed char just_received;
static volatile unsigned char buffer_read, buffer_write;
static volatile unsigned char repeats;
static volatile unsigned char timer_overflows;

/*
 * Debug variables
 */
#if IR_DBG_EN
volatile unsigned int 			cnt_start,
								cnt_space,
								cnt_addr,
								cnt_cmd;
#endif

/*
 * Buffer with received data
 */
IRDATA buffer[IR_BUFFER_SIZE];

void ir_init(void) {

	/*
	 * Init ariables
	 */
	status = IR_STARTBIT;
	edge = FALLING;
	buffer_write = 0;
	buffer_read = 0;
	repeats = 0;
	timer_overflows = 0;

#if IR_DBG_EN
	cnt_start = 0;
	cnt_space = 0;
	cnt_addr = 0;
	cnt_cmd = 0;
#endif

	/*
	 * Init Timer0
	 */
	IR_CNT_PRESCALE_SET();
	IR_CNT = 0;
	IR_CNT_OVF_EN();

	/*
	 * Init INT0
	 *
	 * Falling edge interrupt at init, and interrupt enabled ofc.
	 */
	IR_INT_EN();		// Enable int0
	IR_FALLING_INT();	// Falling edge

}

signed char ir_has_next(void) {
	if (buffer_read == buffer_write) {
		return 0;
	} else {
		return 1;
	}
}

void ir_get_next(IRDATA *data) {

	while(buffer_read == buffer_write);

	data->address_low = buffer[buffer_read].address_low;
	data->command = buffer[buffer_read].command;

	if (buffer_read < IR_BUFFER_SIZE - 1) {
		buffer_read++;
	} else {
		buffer_read = 0;
	}

}

unsigned char ir_get_repeats(void) {
	return repeats;
}

ISR (IR_PIN_ISR) {
	static unsigned char index, addr, cmd;

	volatile signed int cnt = (signed int)IR_CNT + (signed int)(((unsigned int)timer_overflows) << 8);	// Save count
	IR_CNT = 0;
	timer_overflows = 0;

	switch (status) {
	case IR_STARTBIT:
		if (edge == FALLING) {

			if (just_received && cnt < PERIODE_MAX_CNT) {

				if (cnt > PERIODE_MIN_CNT) {
					repeats++;
				}

			} else {

				IR_RISING_INT();
				edge = RISING;

			}

		} else {

			status = IR_STARTBIT;
			IR_FALLING_INT();
			edge = FALLING;
			just_received = 0;

#if IR_DBG_EN
			cnt_start = cnt;
#endif

			if ((cnt - MAX_ERROR) < STARTBIT_CNT && (cnt + MAX_ERROR) > STARTBIT_CNT) {
				status = IR_SPACE;
			}
		}
		break;

	case IR_SPACE:

#if IR_DBG_EN
		cnt_space = cnt;
#endif

		if ((cnt - MAX_ERROR) < CMD_SPACE_CNT && (cnt + MAX_ERROR) > CMD_SPACE_CNT) {
			/*
			 * OK, so far so good. Next up is the address.
			 */
			status = IR_ADDR;

			index = 0;
			addr = 0;
			cmd = 0;

		}

		else {
			/*
			 * ERROR!
			 */
			status = IR_STARTBIT;
			just_received = 0;
		}

		break;

	case IR_ADDR:

#if IR_DBG_EN
		cnt_addr = cnt;
#endif

		if ((cnt - MAX_ERROR) < ONE_CNT && (cnt + MAX_ERROR) > ONE_CNT) {
			addr |= _BV(index);
		} else if ((cnt - MAX_ERROR) < ZERO_CNT && (cnt + MAX_ERROR) > ZERO_CNT) {
			// Bits initialized to zero already...
		} else {
			/*
			 * FAIL!
			 */
			status = IR_STARTBIT;
			just_received = 0;
			return;
		}

		index++;

		if (index == 8) {
			status = IR_CMD;
			index = 0;
		}

		break;

	case IR_CMD:

#if IR_DBG_EN
		cnt_cmd = cnt;
#endif

		if ((cnt - MAX_ERROR) < ONE_CNT && (cnt + MAX_ERROR) > ONE_CNT) {
			cmd |= _BV(index);
		} else if ((cnt - MAX_ERROR) < ZERO_CNT && (cnt + MAX_ERROR) > ZERO_CNT) {
			// Bits initialized to zero already...
		} else {
			/*
			 * FAIL!
			 */
			status = IR_STARTBIT;
			just_received = 0;
			return;
		}

		index++;

		if (index == 8) {
			/*
			 * No error as far as we can tell. Put address and command into ringbuffer.
			 */
			buffer[buffer_write].address_low = addr;
			buffer[buffer_write].command = cmd;

			if (buffer_write < IR_BUFFER_SIZE - 1) {
				buffer_write++;
			} else {
				buffer_write = 0;
			}

			repeats = 0;
			just_received = 1;

			status = IR_STARTBIT;
		}

		break;

	}
}

ISR (IR_CNT_ISR) {

	timer_overflows++;

	if (timer_overflows > 50) {
		status = IR_STARTBIT;
		just_received = 0;
	}

}

#endif





