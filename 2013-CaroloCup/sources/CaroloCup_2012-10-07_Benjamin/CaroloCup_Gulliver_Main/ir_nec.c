/*
 * ir_nec.c
 *
 *  Created on: 25 dec 2009
 *      Author: benjamin
 *
 *
 * NEC IR protocol implemented using interrupts on INT0  or INT1 pin.
 *
 */

#include "ir.h"

#ifdef	IR_NEC_PROTOCOL

#include <avr/io.h>
#include <avr/interrupt.h>

/*
 * NEC Timing (in us)
 */
#define STARTBIT_LEN	9000L
#define ZERO_LEN		1125L
#define ONE_LEN			2250L
#define CMD_SPACE_LEN	4500L
#define REP_SPACE_LEN	2250L
#define REP_BREAK_LEN	107750L
#define PERIODE_LEN		110000L

/*
 * Timer constants
 */
#define MAX_ERROR		((F_CPU * 8) / 1000000)	// Maximum timing error in timer ticks
#define TIMER_PRESCALE	64

#define STARTBIT_CNT	((STARTBIT_LEN * (F_CPU / 1000000L)) / TIMER_PRESCALE)	// 1125
#define ZERO_CNT		((ZERO_LEN * (F_CPU / 1000000L)) / TIMER_PRESCALE)		// 140.625
#define ONE_CNT			((ONE_LEN * (F_CPU / 1000000L)) / TIMER_PRESCALE)		// 281.25
#define CMD_SPACE_CNT	((CMD_SPACE_LEN * (F_CPU / 1000000L)) / TIMER_PRESCALE)	// 562.5
#define REP_SPACE_CNT	((REP_SPACE_LEN * (F_CPU / 1000000L)) / TIMER_PRESCALE)	// 281.25
#define REP_BREAK_CNT	((REP_BREAK_LEN * (F_CPU / 1000000L)) / TIMER_PRESCALE)	// 13468.75
#define PERIODE_CNT		((PERIODE_LEN * (F_CPU / 1000000L)) / TIMER_PRESCALE)	// 13750

/*
 * Macros for 8 MHz.
 */
//#define STARTBIT_CNT	1125
//#define ZERO_CNT		141
//#define ONE_CNT		281
//#define CMD_SPACE_CNT	563
//#define REP_SPACE_CNT	281
//#define REP_BREAK_CNT	13469
//#define PERIODE_CNT	13750

/*
 * Interrupt stuff
 */
#define	IR_STARTBIT		0
#define IR_SPACE		1
#define	IR_ADDR1		2
#define	IR_ADDR2		3
#define	IR_CMD1			4
#define	IR_CMD2			5

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
volatile unsigned char repeats;
volatile unsigned char timer_overflows;
volatile signed char bit_rec = 0;

/*
 * Debug variables
 */
#if IR_DBG_EN
volatile unsigned int 			cnt_start,
								cnt_space,
								cnt_addr1,
								cnt_addr2,
								cnt_cmd1,
								cnt_cmd2;
#endif

/*
 * Buffer with received data
 */
IRDATA data[IR_BUFFER_SIZE];

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
	cnt_addr1 = 0;
	cnt_addr2 = 0;
	cnt_cmd1 = 0;
	cnt_cmd2 = 0;
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

	data->address_low = data[buffer_read].address_low;
	data->address_high = data[buffer_read].address_high;
	data->command = data[buffer_read].command;

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
	static unsigned char index, addr1, addr2, cmd1, cmd2;

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

		if ((cnt - MAX_ERROR) < CMD_SPACE_CNT && (cnt + MAX_ERROR) > CMD_SPACE_CNT) {
			/*
			 * OK, so far so good. Next up is the address.
			 */
			status = IR_ADDR1;

			index = 0;
			addr1 = 0;
			addr2 = 0;
			cmd1 = 0;
			cmd2 = 0;

		}

		else if ((cnt - MAX_ERROR) < REP_SPACE_CNT && (cnt + MAX_ERROR) > REP_SPACE_CNT) {
			if (bit_rec == 1) {
				repeats++;
			}
			status = IR_STARTBIT;
		}

		else {
			status = IR_STARTBIT;
		}

		break;

	case IR_ADDR1:

#if IR_DBG_EN
		cnt_addr1 = cnt;
#endif

		if ((cnt - MAX_ERROR) < ONE_CNT && (cnt + MAX_ERROR) > ONE_CNT) {
			addr1 |= _BV(index);
		} else if ((cnt - MAX_ERROR) < ZERO_CNT && (cnt + MAX_ERROR) > ZERO_CNT) {
			// Bits initialized to zero already...
		} else {
			/*
			 * FAIL!
			 */
			status = IR_STARTBIT;
			return;
		}

		index++;

		if (index == 8) {
			status = IR_ADDR2;
			index = 0;
		}

		break;

	case IR_ADDR2:

#if IR_DBG_EN
		cnt_addr2 = cnt;
#endif

		if ((cnt - MAX_ERROR) < ONE_CNT && (cnt + MAX_ERROR) > ONE_CNT) {
			addr2 |= _BV(index);
		} else if ((cnt - MAX_ERROR) < ZERO_CNT && (cnt + MAX_ERROR) > ZERO_CNT) {
			// Bits initialized to zero already...
		} else {
			/*
			 * FAIL!
			 */
			status = IR_STARTBIT;
			return;
		}

		index++;

		if (index == 8) {
			status = IR_CMD1;
			index = 0;
		}

		break;

	case IR_CMD1:

#if IR_DBG_EN
		cnt_cmd1 = cnt;
#endif

		if ((cnt - MAX_ERROR) < ONE_CNT && (cnt + MAX_ERROR) > ONE_CNT) {
			cmd1 |= _BV(index);
		} else if ((cnt - MAX_ERROR) < ZERO_CNT && (cnt + MAX_ERROR) > ZERO_CNT) {
			// Bits initialized to zero already...
		} else {
			/*
			 * FAIL!
			 */
			status = IR_STARTBIT;
			return;
		}

		index++;

		if (index == 8) {
			status = IR_CMD2;
			index = 0;
		}

		break;

	case IR_CMD2:

#if IR_DBG_EN
		cnt_cmd2 = cnt;
#endif

		if ((cnt - MAX_ERROR) < ONE_CNT && (cnt + MAX_ERROR) > ONE_CNT) {
			cmd2 |= _BV(index);
		} else if ((cnt - MAX_ERROR) < ZERO_CNT && (cnt + MAX_ERROR) > ZERO_CNT) {
			// Bits initialized to zero already...
		} else {
			/*
			 * FAIL!
			 */
			status = IR_STARTBIT;
			return;
		}

		index++;

		if (index == 8) {
			/*
			 * Now we have all bits. Check for errors!
			 */
			if ((cmd1 ^ cmd2) == 0xFF) {
				/*
				 * No error as far as we can tell. Put address and command into ringbuffer.
				 */
				data[buffer_write].address_low = addr1;
				data[buffer_write].address_high = addr2;
				data[buffer_write].command = cmd1;

				if (buffer_write < IR_BUFFER_SIZE - 1) {
					buffer_write++;
				} else {
					buffer_write = 0;
				}

				repeats = 0;
				bit_rec = 1;

			}

			status = IR_STARTBIT;
		}

		break;

	}
}

ISR (IR_CNT_ISR) {

	timer_overflows++;

	if (timer_overflows > 200) {
		status = IR_STARTBIT;
		bit_rec = 0;
	}

}

#endif



