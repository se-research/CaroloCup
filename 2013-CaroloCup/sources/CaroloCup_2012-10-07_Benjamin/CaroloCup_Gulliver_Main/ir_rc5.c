/*
 * ir_rc5.c
 *
 *  Created on: 26 dec 2009
 *      Author: benjamin
 */

#include "ir.h"

#ifdef	IR_RC5_PROTOCOL

#include <avr/io.h>
#include <avr/interrupt.h>


/*
 * Protocol definitions
 */
#define ADDR_LEN	5
#define CMD_LEN		6

/*
 * Timings in us
 */
#define HALF_BIT_LEN	889L
#define BIT_LEN			1778L

/*
 * Timer constants
 */
#define MAX_ERROR		20	// Maximum timing error in timer ticks
#define TIMER_PRESCALE	64

#define HALF_BIT_CNT	((HALF_BIT_LEN * (F_CPU / 1000000L)) / TIMER_PRESCALE)	//111.12 @8MHz
#define BIT_CNT			((BIT_LEN * (F_CPU / 1000000L)) / TIMER_PRESCALE)		//222.25 @8MHz

/*
 * Interrupt stuff
 */
#define	IR_STARTBIT1	0
#define	IR_STARTBIT2	1
#define	IR_TOGGLEBIT	2
#define	IR_ADDR			3
#define	IR_CMD			4

/*
 * Variables
 */
static volatile signed char status;
static volatile signed char toggle;
static volatile unsigned char repeats;
static volatile unsigned char buffer_read, buffer_write;
static volatile unsigned char timer_overflows;

/*
 * Buffer with received data
 */
IRDATA buffer[IR_BUFFER_SIZE];

/*
 * Debug
 */

#if IR_DBG_EN
volatile unsigned int 			cnt_start2,
								cnt_toggle,
								cnt_addr,
								cnt_cmd;
#endif


void ir_init(void) {

	/*
	 * Variables
	 */
	status = IR_STARTBIT1;
	buffer_read = 0;
	buffer_write = 0;
	toggle = 0;

#if IR_DBG_EN
	cnt_start2 = 0;
	cnt_toggle = 0;
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
	 */
	IR_INT_EN();		// Enable int0
	IR_ANY_EDGE_INT();	// Interrupt on any logical change

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

	static unsigned char index = 0, addr = 0, cmd = 0, toggle_last = 0;
	static signed int cnt = 0;

	cnt += (signed int)IR_CNT + (signed int)(((unsigned int)timer_overflows) << 8);	// Save count
	IR_CNT = 0;
	timer_overflows = 0;

	switch (status) {

	case IR_STARTBIT1:

		if (IR_EDGE_LOW()) {
			status = IR_STARTBIT2;
			cnt = 0;
		} else {
			status = IR_STARTBIT1;
		}

		break;

	case IR_STARTBIT2:

#if IR_DBG_EN
		cnt_start2 = cnt;
#endif

		if ((cnt - MAX_ERROR) < BIT_CNT && (cnt + MAX_ERROR) > BIT_CNT) {
			status = IR_TOGGLEBIT;
			cnt = 0;

		} else if ((cnt - MAX_ERROR) < HALF_BIT_CNT && (cnt + MAX_ERROR) > HALF_BIT_CNT) {
			cnt = HALF_BIT_CNT;	// Synchronize..
			return;
		} else {
			status = IR_STARTBIT1;
			return;
		}

		break;

	case IR_TOGGLEBIT:

#if IR_DBG_EN
		cnt_toggle = cnt;
#endif

		if ((cnt - MAX_ERROR) < BIT_CNT && (cnt + MAX_ERROR) > BIT_CNT) {

			toggle_last = toggle;

			if (IR_EDGE_LOW()) {
				toggle = 1;
			} else {
				toggle = 0;
			}

			index = 0;
			addr = 0;
			cmd = 0;

			cnt = 0;
			status = IR_ADDR;

		} else if ((cnt - MAX_ERROR) < HALF_BIT_CNT && (cnt + MAX_ERROR) > HALF_BIT_CNT) {
			cnt = HALF_BIT_CNT;	// Synchronize..
			return;
		} else {
			status = IR_STARTBIT1;
			return;
		}

		break;

	case IR_ADDR:

#if IR_DBG_EN
		cnt_addr = cnt;
#endif

		if ((cnt - MAX_ERROR) < BIT_CNT && (cnt + MAX_ERROR) > BIT_CNT) {

			if (IR_EDGE_LOW()) {
				addr |= _BV(ADDR_LEN - index - 1);
			} else {

			}

			index++;
			if (index == ADDR_LEN) {
				index = 0;
				status = IR_CMD;
			}

			cnt = 0;

		} else if ((cnt - MAX_ERROR) < HALF_BIT_CNT && (cnt + MAX_ERROR) > HALF_BIT_CNT) {
			cnt = HALF_BIT_CNT;	// Synchronize..
			return;
		} else {
			status = IR_STARTBIT1;
			return;
		}

		break;

	case IR_CMD:

		if ((cnt - MAX_ERROR) < BIT_CNT && (cnt + MAX_ERROR) > BIT_CNT) {

#if IR_DBG_EN
		cnt_cmd = cnt;
#endif

			if (IR_EDGE_LOW()) {
				cmd |= _BV(CMD_LEN - index - 1);
			} else {

			}

			index++;
			if (index == CMD_LEN) {

				if (toggle == toggle_last) {

					repeats++;

				} else {

					buffer[buffer_write].address_low = addr;
					buffer[buffer_write].command = cmd;

					if (buffer_write < IR_BUFFER_SIZE - 1) {
						buffer_write++;
					} else {
						buffer_write = 0;
					}

					repeats = 0;
				}



				status = IR_STARTBIT1;
			}

			cnt = 0;

		} else if ((cnt - MAX_ERROR) < HALF_BIT_CNT && (cnt + MAX_ERROR) > HALF_BIT_CNT) {
			cnt = HALF_BIT_CNT;	// Synchronize..
			return;
		} else {
			status = IR_STARTBIT1;
			return;
		}

		break;

	}

}


ISR (IR_CNT_ISR) {
	timer_overflows++;

	if (timer_overflows > 50) {
		status = IR_STARTBIT1;
	}

}

#endif



