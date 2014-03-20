/*
 * ir_nec.c
 *
 *  Created on: 12 aug 2010
 *      Author: benjamin
 *
 *
 * NEC IR protocol implemented using interrupts on INT0  or INT1 pin.
 *
 */

#include "ir.h"

#ifdef	IR_ATLAS_PROTOCOL

#include <avr/io.h>
#include <avr/interrupt.h>

/*
 * Atlas AC Timing (in us)
 */
#define STARTBIT_LEN	4250L
#define ZERO_LEN		1100L
#define ONE_LEN			2150L
#define SPACE_LEN		4450L
#define REP_BREAK_LEN	5700L
#define PERIODE_LEN		180000L

/*
 * Timer constants
 */
#define MAX_ERROR		50	// Maximum timing error in timer ticks
#define TIMER_PRESCALE	64

#define STARTBIT_CNT	((STARTBIT_LEN * (F_CPU / 1000000L)) / TIMER_PRESCALE)
#define ZERO_CNT		((ZERO_LEN * (F_CPU / 1000000L)) / TIMER_PRESCALE)
#define ONE_CNT			((ONE_LEN * (F_CPU / 1000000L)) / TIMER_PRESCALE)
#define SPACE_CNT		((SPACE_LEN * (F_CPU / 1000000L)) / TIMER_PRESCALE)
#define REP_BREAK_CNT	((REP_BREAK_LEN * (F_CPU / 1000000L)) / TIMER_PRESCALE)
#define PERIODE_CNT		((PERIODE_LEN * (F_CPU / 1000000L)) / TIMER_PRESCALE)

/*
 * Interrupt stuff
 */
#define	IR_STARTBIT			0
#define IR_SPACE			1
#define	IR_DATA				2
#define IR_BREAK			3
#define	IR_REP_STARTBIT		4
#define IR_REP_SPACE		5
#define	IR_REP_DATA			6

/*
 * Edges
 */
#define	FALLING		0
#define	RISING		1

/*
 * Variables
 */
volatile signed char status;
volatile signed char edge;
volatile unsigned char buffer_read, buffer_write;
volatile unsigned char timer_overflows;

/*
 * Debug variables
 */
volatile unsigned int 		cnt_start,
							cnt_space,
							cnt_data1,
							cnt_break,
							cnt_rep_start,
							cnt_rep_space,
							cnt_data2;

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
	timer_overflows = 0;

#if IR_DBG_EN
	cnt_start = 0;
	cnt_space = 0;
	cnt_data1 = 0;
	cnt_break = 0;
	cnt_rep_start = 0;
	cnt_rep_space = 0;
	cnt_data2 = 0;
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

	unsigned char i;

	for (i = 0;i < IR_PACKET_LEN;i++ ) {
		data->data[i] = buffer[buffer_read].data[i];
	}

	if (buffer_read < IR_BUFFER_SIZE - 1) {
		buffer_read++;
	} else {
		buffer_read = 0;
	}

}

ISR (IR_PIN_ISR) {
	static unsigned char index, index_byte, data1[IR_PACKET_LEN], data2[IR_PACKET_LEN];

	volatile signed int cnt = (signed int)IR_CNT + (signed int)(((unsigned int)timer_overflows) << 8);	// Save count
	IR_CNT = 0;
	timer_overflows = 0;

	switch (status) {

	case IR_STARTBIT:
		if (edge == FALLING) {
			IR_RISING_INT();
			edge = RISING;
		} else {
			IR_FALLING_INT();
			edge = FALLING;

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

		if ((cnt - MAX_ERROR) < SPACE_CNT && (cnt + MAX_ERROR) > SPACE_CNT) {
			/*
			 * OK, so far so good. Next up is the address.
			 */
			status = IR_DATA;

			for (index = 0;index < IR_PACKET_LEN;index++) {
				data1[index] = 0;
				data2[index] = 0;
			}

			index = 7;
			index_byte = 0;

		}

		else {
			status = IR_STARTBIT;
		}

		break;

	case IR_DATA:

#if IR_DBG_EN
		cnt_data1 = cnt;
#endif

		if ((cnt - MAX_ERROR) < ONE_CNT && (cnt + MAX_ERROR) > ONE_CNT) {
			data1[index_byte] |= _BV(index);
		} else if ((cnt - MAX_ERROR) < ZERO_CNT && (cnt + MAX_ERROR) > ZERO_CNT) {
			// Bits initialized to zero already...
		} else {
			/*
			 * FAIL!
			 */
			status = IR_STARTBIT;
			return;
		}

		index--;

		if (index == 255) {
			index = 7;
			index_byte++;

			if (index_byte == IR_PACKET_LEN) {
				index_byte = 0;
				status = IR_BREAK;
			}
		}

		break;

	case IR_BREAK:

#if IR_DBG_EN
		cnt_break = cnt;
#endif

		if ((cnt - MAX_ERROR) < REP_BREAK_CNT && (cnt + MAX_ERROR) > REP_BREAK_CNT) {
			/*
			 * OK, so far so good. Next up is the inverted data.
			 */
			status = IR_REP_STARTBIT;
			IR_RISING_INT();
			edge = RISING;

			index = 7;
			index_byte = 0;

		}

		else {
			status = IR_STARTBIT;
		}

		break;

	case IR_REP_STARTBIT:

		IR_FALLING_INT();
		edge = FALLING;

#if IR_DBG_EN
		cnt_rep_start = cnt;
#endif

		if ((cnt - MAX_ERROR) < STARTBIT_CNT && (cnt + MAX_ERROR) > STARTBIT_CNT) {
			status = IR_REP_SPACE;
		} else {
			status = IR_STARTBIT;
		}
		break;


	case IR_REP_SPACE:

#if IR_DBG_EN
		cnt_rep_space = cnt;
#endif

		if ((cnt - MAX_ERROR) < SPACE_CNT && (cnt + MAX_ERROR) > SPACE_CNT) {
			status = IR_REP_DATA;
		} else {
			status = IR_STARTBIT;
		}

		break;

	case IR_REP_DATA:

#if IR_DBG_EN
		cnt_data2 = cnt;
#endif

		if ((cnt - MAX_ERROR) < ONE_CNT && (cnt + MAX_ERROR) > ONE_CNT) {
			data2[index_byte] |= _BV(index);
		} else if ((cnt - MAX_ERROR) < ZERO_CNT && (cnt + MAX_ERROR) > ZERO_CNT) {
			// Bits initialized to zero already...
		} else {
			/*
			 * FAIL!
			 */
			status = IR_STARTBIT;
			return;
		}

		index--;

		if (index == 255) {
			index = 7;
			index_byte++;

			if (index_byte == IR_PACKET_LEN) {
				status = IR_STARTBIT;

				/*
				 * We have the data. Now lets check it.
				 */
				for (index = 0;index < IR_PACKET_LEN;index++) {
					if ((data1[index] ^ data2[index]) != 0xFF) {
						return;
					}
				}

				/*
				 * Received data seems to be valid. Add it to the buffer.
				 */
				for (index = 0;index < IR_PACKET_LEN;index++) {

					buffer[buffer_write].data[index] = data1[index];

				}

				if (buffer_write < IR_BUFFER_SIZE - 1) {
					buffer_write++;
				} else {
					buffer_write = 0;
				}

			}
		}

		break;

	}
}

ISR (IR_CNT_ISR) {

	timer_overflows++;

	if (timer_overflows > 200) {
		status = IR_STARTBIT;
		IR_FALLING_INT();
		edge = FALLING;
	}

}

#endif



